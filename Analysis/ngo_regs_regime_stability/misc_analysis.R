#' ---
#' title: "Non-regression analysis"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: ../html/fixes.css
#'     code_folding: hide
#'     toc: yes
#'     toc_float: true
#'     toc_depth: 4
#'     highlight: pygments
#'     theme: cosmo
#'     self_contained: no
#'     includes:
#'       after_body: ../html/add_home_link.html
#' ---


#' Load libraries and data
#+ message=FALSE
knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

# library(printr)
library(tidyverse)
library(magrittr)
library(broom)
library(lubridate)
library(stringr)
library(ggstance)
library(gridExtra)
library(scales)
library(Cairo)
library(countrycode)
library(DT)
library(rstanarm)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

full.data <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.rds"))

dcjw <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                          "dcjw.rds"))

# Load Robinson map projection
countries.ggmap <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                                     "countries110_robinson_ggmap.rds"))

# All mappable countries
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))

my.seed <- 1234
set.seed(my.seed)

CHAINS <- 4
ITER <-2000
WARMUP <- 1000

options(mc.cores = parallel::detectCores())  # Use all possible cores

bayesgazer.dcjw <- function(model, barrier.name) {
  model.posterior.probs <- as.data.frame(model) %>%
    summarise_each(funs(pp.less0 = mean(. < 0))) %>%
    gather(key, value) %>%
    separate(key, c("term", "key"), sep="_p") %>%
    spread(key, value) %>%
    filter(term == "value")
  
  p.less0 <- model.posterior.probs$p.less0
  
  model.output <- tidy(model) %>%
    filter(term == "value") %>%
    mutate(p.less0 = p.less0,
           Barrier = barrier.name) %>%
    mutate(combined = sprintf("%.2f (%.2f)", estimate, std.error)) %>%
    select(Barrier, `Posterior median β (SD)` = combined,
           `P(β < 0)` = p.less0)
  
  return(model.output)
}

#' # General data summary
#' 
#' Number of countries:
length(unique(full.data$cowcode))

#' Range of years:
min(full.data$year.num)
max(full.data$year.num)


#' # Regime type selection
#' 
#' There are lots of different ways to define autocracy, including Polity, UDS,
#' or Geddes et al.'s manual classification. Here, I follow Geddes et al., but
#' slightly expanded—for the sake of completeness of data (and in case I run
#' survival models in the future), I include a country in the data if it has
#' ever been an autocracy since 1991. This means countries that democratize or
#' backslide are included.
#' 
autocracies.gwf <- filter(full.data, gwf.ever.autocracy)

#' Number of GWF autocracies included:
#' 
cows.gwf <- unique(autocracies.gwf$cowcode)
cows.gwf %>% length()

#' I also use UDS scores of less than 0 as an alternative definition of
#' autocracy. Again, I consider a country to be an autocracy if it has ever
#' been one since 1991.
autocracies.uds <- filter(full.data, uds.ever.autocracy)

#' Number of UDS autocracies included:
#' 
cows.uds <- unique(autocracies.uds$cowcode)
cows.uds %>% length()

#' Countries in GWF autocracies not in UDS:
#' 
autocracies.gwf %>%
  distinct(cowcode, country) %>%
  filter(!(cowcode %in% cows.uds))

#' Countries in UDS autocracies not in GWF:
#' 
autocracies.uds %>%
  distinct(cowcode, country) %>%
  filter(!(cowcode %in% cows.gwf))

#' To fix this discrepancy, in the final analysis I include any country that 
#' appears in either dataset.
#' 
cows.autocracies <- unique(c(cows.uds, cows.gwf))
cows.autocracies %>% length()

autocracies <- full.data %>%
  filter(cowcode %in% cows.autocracies)

# Save for easier access later
saveRDS(autocracies, file.path(PROJHOME, "Data", "data_processed",
                               "autocracies.rds"))

#' Map of final autocracies (hello Africa and Asia, basically):
#' 
df.autocracy.countries <- possible.countries %>%
  mutate(autocracy = id %in% unique(autocracies$iso3))
  
plot.autocracy.map <- ggplot(df.autocracy.countries,
                             aes(fill=autocracy, map_id=id)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_manual(values=c("#FFFFFF", "grey50"), na.value="#FFFFFF", guide=FALSE) +
  theme_ath_map()
plot.autocracy.map

fig.save.cairo(plot.autocracy.map, filename="1-autocracies-map", 
               width=5, height=3)

#' Full list of autocracies:
#' 
#+ results="asis"
autocracies.countries <- sort(unique(autocracies$country))

# autocracies.output <- matrix(autocracies.countries, ncol=3, byrow=FALSE)
autocracies.output <- matrix(c(autocracies.countries, rep(NA, 2)),
                             ncol=3, byrow=FALSE)

caption <- "Countries identified as autocracies by either Geddes et. al or scoring less than zero in UDS {#tbl:autocracies}"
tbl.autocracies <- pandoc.table.return(autocracies.output, caption=caption)
cat(tbl.autocracies)
cat(tbl.autocracies, file=file.path(PROJHOME, "Output", "tables", 
                                    "1-autocracies.md"))


#' ## Visualizing basic correlation between regime type and CSRE
#' 
#' Regime type and CSRE are quite correlated
#' 
plot.data <- full.data %>%
  select(cs_env_sum, uds_mean, e_polity2) %>% 
  na.omit()

plot.data %>%
  summarise_each(funs(cor(., plot.data$cs_env_sum)), -cs_env_sum)

#' You can see that visually too
#' 
#+ warning=FALSE
plot.polity <- ggplot(plot.data, aes(x=e_polity2, y=cs_env_sum)) + 
  geom_vline(xintercept=0, colour="#EA2E49", size=0.5) +
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") + 
  labs(x="Polity IV score", 
       y="Civil society regulatory environment\n(CSRE)") +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  coord_cartesian(ylim=c(-6, 6)) +
  theme_ath()

plot.uds <- ggplot(plot.data, aes(x=uds_mean, y=cs_env_sum)) + 
  geom_vline(xintercept=0, colour="#EA2E49", size=0.5) +
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") +
  labs(x="Mean UDS score", y=NULL) +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  coord_cartesian(ylim=c(-6, 6)) +
  theme_ath()

# GWF regime type and Polity/UDS and CSRE
gwf.summary <- full.data %>%
  group_by(gwf.binary) %>%
  summarise(env.avg = mean(cs_env_sum, na.rm=TRUE),
            env.sd = sd(cs_env_sum, na.rm=TRUE),
            env.se = env.sd / sqrt(n()),
            env.upper = env.avg + (qnorm(0.975) * env.se),
            env.lower = env.avg + (qnorm(0.025) * env.se)) %>%
  na.omit

plot.gwf.csre <- ggplot(gwf.summary, aes(x=gwf.binary, y=env.avg)) + 
  geom_pointrange(aes(ymin=env.lower, ymax=env.upper), size=0.5) +
  geom_point(data=full.data, aes(y=cs_env_sum), size=0.25, alpha=0.05) +
  labs(y=NULL, x="Geddes et al. categorization") +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  coord_cartesian(ylim=c(-6, 6)) +
  theme_ath()

plot.regime.csre <- arrangeGrob(plot.polity, plot.uds, plot.gwf.csre, nrow=1)
grid::grid.draw(plot.regime.csre)

fig.save.cairo(plot.regime.csre, filename="1-regime-csre", 
               width=5, height=2)

#' # Dependent variable: civil society regulatory environment (CSRE)
#' 
#' ## Understanding and visualizing the CSRE
#' 
#' The V-Dem Bayesian measurement model is cool and all, but it's really hard
#' to interpret by itself. Showing a few example countries can help. 
#' 
cs.plot.all <- autocracies %>%
  filter(year.num > 2000) %>%
  group_by(country) %>%
  summarise(env.mean = mean(cs_env_sum, na.rm=TRUE)) %>%
  filter(!is.na(env.mean)) %>%
  arrange(env.mean) %>%
  ungroup()

cs.plot <- bind_rows(cs.plot.all %>% slice(1:5),
                     data_frame(country = "—", env.mean = 0),
                     cs.plot.all %>% slice((nrow(.) - 4):nrow(.))) %>%
  mutate(country = factor(country, levels=unique(country), ordered=TRUE))

#' The usual suspects get their scores
cs.plot

plot.csre.top.bottom <- ggplot(cs.plot, aes(x=country, y=env.mean)) +
  geom_point(size=2) + 
  geom_segment(aes(yend=0, xend=country), size=1) + 
  labs(x=NULL, y="Mean civil society regulatory environment index (CSRE)") +
  coord_flip() +
  theme_ath()
plot.csre.top.bottom

fig.save.cairo(plot.csre.top.bottom, filename="1-csre-top-bottom", 
               width=5, height=2.5)


#' ## CSRE and other measures of restriction?
#' 
#' How does the CSRE compare to other measures of civil society restriction?
#' 
#' ### Christensen and Weinstein NGO laws
#' 
#' The DCJW data shows the rough onset of different types of foreign-ish NGO
#' restriction, including barriers to (1) entry, (2) funding, and (3) advocacy.
#' I create a kind of index for each, sometimes reversing the coding (i.e. when
#' a "no" actually means worsening restrictions). Here's what I do:
#' 
#' **Barriers to entry**: (4 points)
#' 
#' - 2a. Are NGOs required to register with the government?: Yes = 1; No = 0
#' - 2b. How burdensome is registration?: Vague, onerous = 1; well defined = 0
#' - 2c. In law, can an NGO appeal if denied registration?: Yes = 0; No = 1
#'   (reversed)
#' - 2d. Are barriers to entry different for NGOs receiving foreign funds?:
#'   Less burdensome = −1; same = 0; more burdensome = 1
#' 
#' **Barriers to funding**: (8 points)
#' 
#' - 3a. Are NGOs required to disclose their funding sources to the
#'   government?: Yes = 1; No = 0
#' - 3b. Do NGOs need prior approval from the government to receive foreign
#'   funding?: Yes = 1; No = 0
#' - 3c. Are NGOs required to channel foreign funding through state-owned banks
#'   or government ministries?: Yes = 1; No = 0
#' - 3d. Are any additional restrictions on foreign support in place (beyond
#'   prior approval for and the channeling of foreign funding)?: Yes = 1; No = 0
#' - 3e. Are all NGOs prohibited from receiving foreign funds?: No = 0; No up
#'   to a certain threshold = 1; Yes = 2
#' - 3f. Are a category of NGOs prohibited from receiving foreign funds?: No =
#'   0; No up to a certain threshold = 1; Yes = 2
#' 
#' **Barriers to advocacy**: (4 points)
#' 
#' - 4a. Does the law restrict NGOs from engaging in political activities?: No
#'   = 0; topics/ability limited = 1; political activities prohibited = 2
#' - Has the government used intimidation or dissolution to deter NGOs from
#'   engaging in political activities?: Yes = 1; No = 0
#' - 4c. Are restrictions on political activities different for NGOs receiving
#'   foreign funds?: Less restrictive = −1; same = 0; more restrictive = 1
#' 
#' The count of restrictions on NGOs is negatively correlated with the CSRE,
#' which makes sense—the CSRE has a wide possible distribution when there are
#' only a few restrictions, since it's normal to have *some* sort of legal
#' control over NGOs. But as those restrictions increase, the CSRE worsens.
#' 
#' Barriers to entry aren't as linked to the CSRE; barriers to advocacy and
#' funding have a stronger negative effect.
#' 
#+ warning=FALSE
dcjw.barriers.plot.data <- autocracies %>%
  select(cs_env_sum, starts_with("dcjw")) %>%
  gather(barrier, value, starts_with("dcjw")) %>%
  filter(!is.na(value)) %>%
  mutate(barrier = factor(barrier,
                          levels=c("dcjw.entry", "dcjw.funding",
                                   "dcjw.advocacy", "dcjw.total"),
                          labels=c("Barriers to entry", "Barriers to funding",
                                   "Barriers to advocacy", "All barriers")))

p.csre.dcjw <- ggplot(dcjw.barriers.plot.data, aes(x=value, y=cs_env_sum)) + 
  geom_point(alpha=0.15, size=1) + 
  geom_smooth(method="loess") +
  labs(x="Number of legal restrictions on NGOs", y="CSRE") + 
  theme_ath() + 
  facet_wrap(~ barrier, scales="free_x")

p.csre.dcjw

fig.save.cairo(p.csre.dcjw, filename="1-csre-dcjw", 
               width=6, height=4)

#+ dcjw_bayes
dcjw.raw.bayes.rds <- file.path(PROJHOME, "Data", "data_processed",
                           "dcjw_raw_bayes.rds")

if (file.exists(dcjw.raw.bayes.rds)) {
  dcjw.raw.bayes <- readRDS(dcjw.raw.bayes.rds)
} else {
  dcjw.raw.bayes <- dcjw.barriers.plot.data %>%
    group_by(barrier) %>%
    nest() %>%
    mutate(model = data %>% map(~ stan_glm(cs_env_sum ~ value, data=.,
                                           family=gaussian(),
                                           prior=cauchy(),
                                           prior_intercept=cauchy(),
                                           chains=CHAINS, iter=ITER, warmup=WARMUP,
                                           algorithm="sampling", seed=my.seed)))
  saveRDS(dcjw.raw.bayes, file=dcjw.raw.bayes.rds)
}

dcjw.models <- dcjw.raw.bayes %>%
  gather(model.name, model, -barrier, -data) %>%
  mutate(output = map2(model, barrier, ~ bayesgazer.dcjw(.x, .y)))

dcjw.type.models <- bind_rows(dcjw.models$output)

#+ results="asis"
caption <- "Coefficients from Bayesian OLS models predicting the CSRE with the count of legal barriers to NGOs {#tbl:csre-dcjw-coefs}"
tbl.djcw.types <- pandoc.table.return(dcjw.type.models,
                                      justify="lcc", caption=caption)
cat(tbl.djcw.types)
cat(tbl.djcw.types, file=file.path(PROJHOME, "Output", "tables", 
                                   "1-csre-dcjw-coefs.md"))

#' This relationship holds up over time too. 
dcjw.time.plot.data <- autocracies %>%
  select(cowcode, year.num, cs_env_sum, cs_env_sum.lead, starts_with("dcjw"))

dcjw.agg.levels <- data_frame(barrier = c("cs_env_sum_avg",
                                          "dcjw.advocacy_avg",
                                          "dcjw.entry_avg",
                                          "dcjw.funding_avg",
                                          "dcjw.total_avg"),
                              barrier.clean = c("CSRE",
                                                "Barriers to advocacy",
                                                "Barriers to entry",
                                                "Barriers to funding",
                                                "All barriers"))

dcjw.time.agg <- dcjw.time.plot.data %>%
  group_by(year.num) %>%
  summarise_each(funs(avg = mean(., na.rm=TRUE)), 
                 cs_env_sum, starts_with("dcjw")) %>%
  gather(barrier, value, -year.num) %>%
  left_join(dcjw.agg.levels, by="barrier") %>%
  filter(year.num <= 2012) %>%
  mutate(measure = ifelse(barrier == "cs_env_sum_avg", 
                          "Average CSRE", "Average DCJW barriers"))

ggplot(dcjw.time.agg, aes(x=year.num, y=value, colour=barrier.clean)) +
  geom_line(size=1) + 
  labs(x=NULL, y="Average value") + 
  scale_color_manual(values=ath.palette("palette1"), name=NULL) +
  theme_ath() +
  facet_wrap(~ measure, ncol=1, scales="free_y")


#' So, overall, the CSRE tracks pretty well with DCJW's collection of legal
#' restrictions.
#' 
#' And how does DCJW data look in general over time?
#' 
dcjw.time.agg1 <- dcjw.time.agg %>%
  filter(barrier.clean != "CSRE")

p.dcjw.time.agg1 <- ggplot(dcjw.time.agg1, aes(x=year.num, y=value, colour=barrier.clean)) +
  geom_line(size=1) + 
  labs(x=NULL, y="Average number of barriers", 
       title="Legal barriers for NGOs in autocracies", 
       caption="Data source: Christensen and Weinstein, 2013") + 
  scale_color_manual(values=ath.palette("palette1"), name=NULL) +
  theme_ath()

fig.save.cairo(p.dcjw.time.agg1, filename="1-dcjw-agg-1", 
               width=5, height=3)


#' ### "Stop Meddling in My Country!" (DupuyRonPrakash:2014a)
#' 
#' Dupuy, Ron, and Prakash predict the onset of foreign funding restrictions on
#' NGOs in 45 countries, which is just one of the types tracked by Christensen
#' and Weinstein. I'm guessing that because this is just a subset, it won't
#' have *that* much of a relationship to the CSRE, since the CSRE is more
#' expansive. But I check anyway.
#' 
dpr.plot.data <- autocracies %>%
  select(year.num, cs_env_sum, starts_with("foreign")) %>%
  filter(!is.na(foreign.funding.count))

#+ warning=FALSE
ggplot(dpr.plot.data, aes(x=foreign.funding.count, y=cs_env_sum)) + 
  geom_point(alpha=0.15, size=1) + 
  geom_smooth(method="lm") + 
  labs(x="Number of foreign funding restrictions on NGOs", y="CSRE") + 
  theme_ath() 

#' Pretty flat. There's no correlation between the two over time, either:
#' 
dpr.time.agg <- dpr.plot.data %>%
  group_by(year.num) %>%
  summarise(cs_env_sum_avg = mean(cs_env_sum, na.rm=TRUE),
            foreign.funding.avg = mean(foreign.funding.count, na.rm=TRUE)) %>%
  gather(measure, value, -year.num) %>%
  # left_join(dcjw.agg.levels, by="barrier") %>%
  filter(year.num <= 2012) %>%
  mutate(measure = ifelse(measure == "cs_env_sum_avg", "Average CSRE",
                          "Average number of foreign funding restrictions"))

ggplot(dpr.time.agg, aes(x=year.num, y=value, colour=measure)) +
  geom_line(size=1) + 
  labs(x=NULL, y="Average value") + 
  scale_color_manual(values=ath.palette("palette1"), name=NULL) +
  theme_ath() +
  facet_wrap(~ measure, ncol=1, scales="free_y")


#' # Internal explanatory variables
#' 
#' ## Understanding and visualizing ICRG
risk.stats <- autocracies %>%
  filter(year.num > 2000) %>%
  group_by(country) %>%
  summarise(risk.max = max(icrg.pol.risk.internal.scaled, na.rm=TRUE),
            risk.min = min(icrg.pol.risk.internal.scaled, na.rm=TRUE),
            stability.max = max(icrg.stability, na.rm=TRUE),
            stability.min = min(icrg.stability, na.rm=TRUE),
            change.risk = risk.max - risk.min,
            change.stability = stability.max - stability.min)

datatable(risk.stats)

#' Conceptualizing the political stability measure is a little tricky. Showing
#' a few example countries can help. Figure out which countries have change the
#' least/most since 2000.
#' 
#' Domestic political stability:
#' 
risk.stats %>%
  select(country, change.risk) %>%
  filter(!is.na(change.risk)) %>%
  arrange(change.risk) %T>%
  {print(head(., 5))} %>% {print(tail(., 5))}

#' Congo most consistent; Middle East saw the biggest change.
#' 
#' Government stability only (just the ICRG subcomponent):
#' 
risk.stats %>%
  select(country, change.stability) %>%
  filter(!is.na(change.stability)) %>%
  arrange(change.stability) %T>%
  {print(head(., 5))} %>% {print(tail(., 5))}

#' Congo, Myanmar again most stable over time, government-wise; Guinea, Libya,
#' and Syria saw biggest changes.
#' 
#' Visualize changes:
#' 
example.countries <- c("Somalia", "Syria", "Qatar")

example.countries.plot <- autocracies %>%
  filter(year.num > 2000, 
         country %in% example.countries) %>%
  mutate(country = factor(country, levels=example.countries, ordered=TRUE))

plot.icrg.risk.examples <- ggplot(example.countries.plot, 
                                  aes(x=year.actual, 
                                      y=icrg.pol.risk.internal.scaled,
                                      colour=country)) + 
  geom_line(size=1) +
  labs(x=NULL, y="Domestic political stability (ICRG)") + 
  coord_cartesian(xlim=ymd(c("2000-01-01", "2015-01-01"))) +
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) +
  guides(colour=guide_legend(reverse=T)) + 
  theme_ath()

plot.icrg.stability.examples <- ggplot(example.countries.plot, 
                                       aes(x=year.actual, 
                                           y=icrg.stability,
                                           colour=country)) + 
  geom_line(size=1) +
  labs(x=NULL, y="Government cohesion (ICRG)") + 
  coord_cartesian(xlim=ymd(c("2000-01-01", "2015-01-01")),
                  ylim=c(2, 12)) +
  scale_y_continuous(breaks=c(seq(2, 12, 2))) +
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) +
  guides(colour=guide_legend(reverse=T)) + 
  theme_ath()

#+ warning=FALSE
plot.icrg.examples <- arrangeGrob(plot.icrg.risk.examples, 
                                  plot.icrg.stability.examples, nrow=1)
grid::grid.draw(plot.icrg.examples)

fig.save.cairo(plot.icrg.examples, filename="1-icrg-examples", 
               width=5, height=2)

#' Average change across regimes (since not everyone is like Syria):
#' 
risk.stats %>% 
  summarise(risk.change.avg = mean(change.risk, na.rm=TRUE),
            stability.change.avg = mean(change.stability, na.rm=TRUE))

#' ## ICRG and CSRE
#' 
#' What does the relationship between internal stability, government risk, and 
#' the CSRE look like?
#' 
#' The CSRE is positively correlated with general internal stability That is, 
#' as the country becomes more stable, the CSRE get better in the following 
#' year.
#' 
#+ warning=FALSE
ggplot(autocracies, aes(x=icrg.pol.risk.internal.scaled, y=cs_env_sum.lead)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method="lm") + 
  geom_smooth(method="loess") + 
  theme_ath()

#' The CSRE is *negatively* correlated with specific government stability,
#' though. That means that as autocratic governments become more stable, they
#' regulate civil society more in the following year.
#' 
#+ warning=FALSE
ggplot(autocracies, aes(x=icrg.stability, y=cs_env_sum.lead)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method="lm") + 
  geom_smooth(method="loess") + 
  theme_ath()

#' This holds when removing stability from the domestic political stability
#' index, too. The general risk score is still positively correlated; stability
#' is negatively correlated.
#' 
#+ warning=FALSE
ggplot(autocracies, aes(x=icrg.pol.risk.internal.nostab.scaled, y=cs_env_sum.lead)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method="lm") + 
  geom_smooth(method="loess") +
  theme_ath()


#' ## Other authoritarian stability variables
#' 
#' ### Other variables and CSRE
#' 
#' All three variables seem closely correlated with the CSRE
#' 
plot.data <- autocracies %>%
  select(cs_env_sum, yrsoffc, years.since.comp, opp1vote) %>% 
  na.omit()

plot.data %>%
  summarise_each(funs(cor(., plot.data$cs_env_sum)), -cs_env_sum)

#' Plot the correlations
#' 
plot.yrs.offc <- ggplot(plot.data, aes(x=yrsoffc, y=cs_env_sum)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") + 
  labs(x="Years executive has been in office", 
       y="Civil society regulatory environment\n(CSRE)") +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.yrs.since.comp <- ggplot(plot.data, 
                              aes(x=years.since.comp, y=cs_env_sum)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") +
  labs(x="Years since a competitive election", y=NULL) +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.opp.vote <- ggplot(plot.data, aes(x=opp1vote, y=cs_env_sum)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") +
  labs(x="Vote share for largest opposition party", y=NULL) +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

#+ warning=FALSE
plot.auth.vars <- arrangeGrob(plot.yrs.offc, plot.yrs.since.comp, 
                              plot.opp.vote, nrow=1)
grid::grid.draw(plot.auth.vars)

fig.save.cairo(plot.auth.vars, filename="1-auth-vars", 
               width=6, height=2)

#' ### Other variables and ICRG
plot.data <- autocracies %>%
  select(icrg.pol.risk.internal.scaled, yrsoffc, years.since.comp, opp1vote) %>% 
  na.omit()

plot.data %>%
  summarise_each(funs(cor(., plot.data$icrg.pol.risk.internal.scaled)),
                 -icrg.pol.risk.internal.scaled)

plot.yrs.offc <- ggplot(plot.data, aes(x=yrsoffc, y=icrg.pol.risk.internal.scaled)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") + 
  labs(x="Years executive has been in office", 
       y="ICRG") +
  scale_y_continuous(breaks=seq(-0, 100, 20)) +
  theme_ath()

plot.yrs.since.comp <- ggplot(plot.data, 
                              aes(x=years.since.comp, y=icrg.pol.risk.internal.scaled)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") +
  labs(x="Years since a competitive election", y=NULL) +
  scale_y_continuous(breaks=seq(-0, 100, 20)) +
  theme_ath()

plot.opp.vote <- ggplot(plot.data, aes(x=opp1vote, y=icrg.pol.risk.internal.scaled)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") +
  labs(x="Vote share for largest opposition party", y=NULL) +
  scale_y_continuous(breaks=seq(-0, 100, 20)) +
  theme_ath()

#+ warning=FALSE
plot.auth.vars <- arrangeGrob(plot.yrs.offc, plot.yrs.since.comp, 
                              plot.opp.vote, nrow=1)
grid::grid.draw(plot.auth.vars)


#' # External explanatory variables
#' 
#' ## ICRG political stability
#' 
#' Which countries are in the most stable/unstable areas?
#' 
neighbor.stability <- autocracies %>%
  group_by(country) %>%
  summarise(stability.mean_wt = mean(icrg.pol.risk.internal.scaled_wt, na.rm=TRUE),
            stability.mean_nb = mean(icrg.pol.risk.internal.scaled_mean_nb, na.rm=TRUE)) %>%
  arrange(desc(stability.mean_wt))

datatable(neighbor.stability)


#' ## Coups
#' 
#' Use data from the [Archigos database of political
#' leaders](http://privatewww.essex.ac.uk/~ksg/archigos.html)
#' 
#+ warning=FALSE, message=FALSE
leaders <- read_tsv("http://privatewww.essex.ac.uk/~ksg/data/1March_Archigos_4.1.txt")
leaders.autocracies.all <- leaders %>%
  filter(ccode %in% unique(autocracies$cowcode)) %>%
  filter(enddate > ymd("1950-01-01")) %>%
  filter(exit != "Still in Office") %>%
  mutate(leader = iconv(leader, from="Windows-1252", to="UTF-8"))  # Convert to UTF-8

leaders.removed.irregular <- leaders.autocracies.all %>%
  filter(exit == "Irregular")

leaders.removed.coup <- leaders.removed.irregular %>%
  filter(str_detect(exitcode, "Removed by")) %>%
  filter(!str_detect(exitcode, "with Foreign"))

leaders.removed.protest <- leaders.removed.irregular %>%
  filter(str_detect(exitcode, "Protest"))

#' According to data from \\[@GoemansGleditschChiozza:2009\], in the
#' `r length(unique(autocracies$cowcode))` countries I identify as autocracies,
#' `r nrow(leaders.autocracies.all)` executives have left office since 1950. In
#' `r nrow(leaders.removed.irregular)` cases, or nearly one third of all exits,
#' the exit from office occurred irregularly (i.e. not due to term limits or
#' natural death). A clear majority of these irregular collapses of executive
#' power—`r nrow(leaders.removed.coup)`, or 75%—occurred as a result of
#' military, political, or rebel coups.
#' 
#' So coups are a big worry.
#' 
#' Which countries are in the areas with the most coups? West Africa…
#' 
neighbor.coups <- autocracies %>%
  group_by(country) %>%
  summarise(coup.activity.sum_nb = sum(coups.activity.bin_sum_nb, na.rm=TRUE)) %>%
  arrange(desc(coup.activity.sum_nb))

datatable(neighbor.coups)


#' When do coups happen? More in the 90s than in the 2000s…
#' 
coups.time <- autocracies %>%
  group_by(year.actual) %>%
  summarise(coup.activity = sum(coups.activity.bin, na.rm=TRUE),
            coup.success = sum(coups.success.bin, na.rm=TRUE)) %>%
  gather(coup.type, value, -year.actual) %>%
  filter(!is.na(value), !is.na(year.actual))

ggplot(coups.time, aes(x=year.actual, y=value, colour=coup.type)) +
  geom_line() + 
  coord_cartesian(xlim=ymd(c("1990-01-05", "2015-01-01"))) +
  labs(x=NULL, y="Number of coups") + 
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) + 
  theme_ath()


#' ## Protests
#' 
#' The second most common form of irregular autocratic exit is stepping down
#' because of popular protest: `r nrow(leaders.removed.protest)` instances, or
#' `r nrow(leaders.removed.protest) / nrow(leaders.removed.irregular)`%
#' 
#' ### Protests in autocracies
autocracies.protests <- autocracies %>%
  filter(year.num > 1994) %>%
  select(year.actual, protests.violent, protests.nonviolent) %>%
  gather(protest.type, value, -year.actual)

#' Number of total protest events:
#' 
autocracies.protests %>% 
  group_by(protest.type) %>% 
  summarise(n = sum(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(n))

autocracies.protests.plot.data <- autocracies.protests %>%
  group_by(protest.type, year.actual) %>%
  summarise(n = sum(value, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(year.actual)) %>%  # TODO: Why are there NAs anyway?
  mutate(protest.type = str_replace(protest.type, "\\.std", ""),
         protest.type = factor(protest.type,
                               levels=c("protests.nonviolent",
                                        "protests.violent"),
                               labels=c("Nonviolent protests   ",
                                        "Violent protests")))
  
p.protests.autocracies <- ggplot(autocracies.protests.plot.data, 
                                 aes(x=year.actual, y=n, colour=protest.type)) + 
  geom_line() +
  scale_y_continuous(labels=comma) + 
  coord_cartesian(xlim=ymd(c("1995-01-05", "2015-01-01"))) +
  labs(x=NULL, y="Number of events") + 
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) + 
  theme_ath()
p.protests.autocracies


#' What do Egypt and China's raw and standardized counts of protests look like?
#' 
#+ warning=FALSE
example.protests <- autocracies %>%
  filter(year.num > 1994) %>%
  filter(country %in% c("Egypt", "China")) %>%
  select(year.actual, country,
         protests.violent, protests.violent.std, 
         protests.nonviolent, protests.nonviolent.std) %>%
  gather(protest.type, value, -year.actual, -country) %>%
  mutate(overall.type = ifelse(str_detect(protest.type, "std"),
                               "Standardized", "Count"),
         protest.type = str_replace(protest.type, "\\.std", ""),
         protest.type = factor(protest.type,
                               levels=c("protests.nonviolent",
                                        "protests.violent"),
                               labels=c("Nonviolent protests   ",
                                        "Violent protests")))

p.protests.std <- ggplot(filter(example.protests, overall.type == "Standardized"), 
                         aes(x=year.actual, y=value, colour=protest.type)) + 
  geom_line(size=1) + 
  scale_y_continuous(breaks=c(1:6)) +
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) +
  coord_cartesian(ylim=c(1, 6)) +
  labs(x=NULL, y="Standardized index") +
  facet_wrap(~ country) + 
  theme_ath() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

p.protests.num <- ggplot(filter(example.protests, overall.type == "Count"), 
                         aes(x=year.actual, y=value, colour=protest.type)) + 
  geom_line(size=1) + 
  scale_colour_manual(values=ath.palette("palette1"), guide=FALSE) +
  labs(x=NULL, y="Number of events") +
  facet_wrap(~ country) + 
  theme_ath() 

p.protests.example <- rbind(ggplotGrob(p.protests.num),
                            ggplotGrob(p.protests.std))
grid::grid.draw(p.protests.example)

fig.save.cairo(p.protests.example, filename="1-protests-egypt-china",
               width=6, height=4)

#' # Shaming explanatory variables
#' 
#' ## Interstate shaming
#' 
#' Number of total shaming events:
#' 
autocracies.shaming <- autocracies %>%
  filter(year.num > 1994) %>%
  select(year.actual, shaming.events.states, shaming.events.ingos) %>%
  gather(shaming.type, value, -year.actual)

autocracies.shaming %>%
  group_by(shaming.type) %>%
  summarise(n = sum(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(n))

#' Almost no INGO-based events. Sad!
#' 
