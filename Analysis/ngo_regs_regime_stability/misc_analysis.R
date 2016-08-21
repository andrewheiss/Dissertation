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

library(printr)
library(magrittr)
library(dplyr)
library(feather)
library(tidyr)
library(purrr)
library(broom)
library(lubridate)
library(ggplot2)
library(ggstance)
library(gridExtra)
library(scales)
library(Cairo)
library(pander)
library(countrycode)
library(DT)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

full.data <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                                    "full_data.feather"))

dcjw <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                               "dcjw.feather"))

my.seed <- 1234
set.seed(my.seed)

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
cs.plot.all <- full.data %>%
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

ggplot(dcjw.barriers.plot.data, aes(x=value, y=cs_env_sum)) + 
  geom_point(alpha=0.15, size=1) + 
  geom_smooth(method="loess") +
  labs(x="Number of legal restrictions on NGOs", y="CSRE") + 
  theme_ath() + 
  facet_wrap(~ barrier, scales="free_x")


dcjw.type.models <- dcjw.barriers.plot.data %>%
  filter(barrier != "All barriers") %>%
  group_by(barrier) %>%
  nest() %>%
  mutate(model = data %>% map(~ lm(cs_env_sum ~ value, data=.))) %>%
  mutate(tidy = model %>% map(broom::tidy),
         estimate = tidy %>% map_dbl(c(2, 2)),
         std.error = tidy %>% map_dbl(c(3, 2)),
         p.value = tidy %>% map_dbl(c(5, 2))) %>%
  mutate(p.stars = case_when(
    .$p.value <= 0.001 ~ "***",
    .$p.value <= 0.01 ~ "**",
    .$p.value <= 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  mutate(clean.estimate = sprintf("%.3f (%.3f)%s", estimate, std.error, p.stars)) %>%
  select(Barrier = barrier, Coefficient = clean.estimate)

#+ results="asis"
pandoc.table(dcjw.type.models)

caption <- "Coefficients from OLS models predicting the CSRE with the count of legal barriers to NGOs {#tbl:csre-dcjw-coefs}"
cat(pandoc.table.return(dcjw.type.models, caption=caption), 
    file=file.path(PROJHOME, "Output", "tables", "1-csre-dcjw-coefs.md"))


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

#' Conceptualizing the political risk measure is a little tricky. Showing a few
#' example countries can help. Figure out which countries have change the
#' least/most since 2000.
#' 
#' Internal political risk:
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

example.countries <- autocracies %>%
  filter(year.num > 2000, 
         country %in% example.countries)

plot.icrg.risk.examples <- ggplot(example.countries, 
                                  aes(x=year.actual, 
                                      y=icrg.pol.risk.internal.scaled,
                                      colour=country)) + 
  geom_line(size=1) +
  labs(x=NULL, y="Internal political risk (ICRG)") + 
  coord_cartesian(xlim=ymd(c("2000-01-01", "2015-01-01"))) +
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) +
  theme_ath()

plot.icrg.stability.examples <- ggplot(example.countries, 
                                       aes(x=year.actual, 
                                           y=icrg.stability,
                                           colour=country)) + 
  geom_line(size=1) +
  labs(x=NULL, y="Government stability (ICRG)") + 
  coord_cartesian(xlim=ymd(c("2000-01-01", "2015-01-01")),
                  ylim=c(2, 12)) +
  scale_y_continuous(breaks=c(seq(2, 12, 2))) +
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) +
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
#' What does the relationship between internal stability, internal risk, and 
#' the CSRE look like?
#' 
#' The CSRE is positively correlated with general internal risk. That is, as
#' the country becomes less risky, the CSRE get better in the following year.
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

#' This holds when removing stability from the internal political risk index,
#' too. The general risk score is still positively correlated; stability is
#' negatively correlated.
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
#' ## Neighboring and regional ICRG risk
#' 
#' Example of Kenya's neighborhood in 2012
#' 
kenya.neighbors <- full.data %>%
  filter(year.num == 2012, cowcode == 501) %>%
  mutate(neighbors.cow = strsplit(neighbors.cow, ",")) %>%
  select(neighbors.cow) %>% unlist %>% map_dbl(as.numeric)

full.data %>%
  filter(year.num == 2012, cowcode %in% kenya.neighbors) %>%
  select(country, icrg.pol.risk.internal.scaled)


#' ## Visualize neighbor and subregional instability
plot.data <- full.data %>%
  select(cs_env_sum, neighbor.pol.risk.min, icrg.pol.risk.subregional)

#' Plot the correlations
#' 
plot.neighbor.risk <- ggplot(plot.data, aes(x=neighbor.pol.risk.min, 
                                            y=cs_env_sum)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") + 
  labs(x="Minimum political risk in neighboring country", 
       y="Civil society regulatory environment\n(CSRE)") +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.subregion.risk <- ggplot(plot.data, aes(x=icrg.pol.risk.subregional, 
                                             y=cs_env_sum)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") + 
  labs(x="Mean political risk in subregion", 
       y=NULL) +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.ext.vars <- arrangeGrob(plot.neighbor.risk, plot.subregion.risk, nrow=1)
grid::grid.draw(plot.ext.vars)

fig.save.cairo(plot.ext.vars, filename="1-ext-risk-vars", 
               width=6, height=2)

#' The two external risk variables are fairly correlated though
#' 
cor(plot.data$icrg.pol.risk.subregional, 
    plot.data$neighbor.pol.risk.min, use="complete.obs")

plot.risk.cor <- ggplot(plot.data, aes(x=icrg.pol.risk.subregional, 
                                       y=neighbor.pol.risk.min)) + 
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") + 
  labs(x="Mean political risk in subregion", 
       y="Minimum political risk in neighboring country") +
  theme_ath()
plot.risk.cor

#' ## Visualize coups
plot.data <- full.data %>%
  select(cs_env_sum, neighbor.coups.activity.bin, coups.activity.subregional) %>%
  na.omit %>%
  mutate(neighbor.coups.activity.bin = factor(neighbor.coups.activity.bin, 
                                              labels=c("No coup activity",
                                                       "Coup activity")),
         coups.activity.subregional = factor(coups.activity.subregional))

plot.coups.neighbor <- ggplot(plot.data, aes(x=neighbor.coups.activity.bin, 
                                             y=cs_env_sum)) + 
  geom_violin() +
  geom_point(size=0.15, alpha=0.15, position="jitter") +
  geom_point(stat="summary", fun.y="mean", size=2) +
  labs(x="Coup activity in neighboring countries", 
       y="Civil society regulatory environment\n(CSRE)") +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.coups.subregion <- ggplot(plot.data, aes(x=coups.activity.subregional, 
                                              y=cs_env_sum)) + 
  geom_violin() +
  geom_point(size=0.15, alpha=0.15, position="jitter") +
  geom_point(stat="summary", fun.y="mean", size=2) +
  labs(x="Coup attemps in subregion", 
       y=NULL) +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.coup.vars <- arrangeGrob(plot.coups.neighbor, plot.coups.subregion, nrow=1)
grid::grid.draw(plot.coup.vars)

fig.save.cairo(plot.coup.vars, filename="1-ext-coup-vars", 
               width=6, height=2)


#' # Shaming explanatory variables
#' 
#' This is all old stuff that I've replaced with more accurate data.
#' 
# plot.data <- full.data %>%
#   select(Country, year.num, year.actual, gwf.binary,
#          icews.conflict.severity.abs, icews.pct.shame,
#          icews.conflict.severity.abs.ingos, icews.pct.shame.ingos) %>%
#   na.omit %>%
#   group_by(year.actual, gwf.binary) %>%
#   summarise(shame = mean(icews.pct.shame),
#             shame.sd = sd(icews.pct.shame, na.rm=TRUE),
#             shame.se = shame.sd / sqrt(n()),
#             shame.upper = shame + (qnorm(0.975) * shame.se),
#             shame.lower = shame + (qnorm(0.025) * shame.se),
#             shame.ingo = mean(icews.pct.shame.ingos),
#             shame.ingo.sd = sd(icews.pct.shame.ingos, na.rm=TRUE),
#             shame.ingo.se = shame.ingo.sd / sqrt(n()),
#             shame.ingo.upper = shame.ingo + (qnorm(0.975) * shame.ingo.se),
#             shame.ingo.lower = shame.ingo + (qnorm(0.025) * shame.ingo.se),
#             severity = mean(icews.conflict.severity.abs),
#             severity.sd = sd(icews.conflict.severity.abs, na.rm=TRUE),
#             severity.se = severity.sd / sqrt(n()),
#             severity.upper = severity + (qnorm(0.975) * severity.se),
#             severity.lower = severity + (qnorm(0.025) * severity.se),
#             severity.ingo = mean(icews.conflict.severity.abs.ingos),
#             severity.ingo.sd = sd(icews.conflict.severity.abs.ingos, na.rm=TRUE),
#             severity.ingo.se = severity.ingo.sd / sqrt(n()),
#             severity.ingo.upper = severity.ingo + (qnorm(0.975) * severity.ingo.se),
#             severity.ingo.lower = severity.ingo + (qnorm(0.025) * severity.ingo.se)) %>%
#   mutate(gwf.binary = factor(gwf.binary, levels=c("Democracy", "Autocracy"),
#                              labels=c("Democracies    ", "Autocracies")))
# 
# #' ## Interstate shame percent and severity
# plot.states.shame <- ggplot(plot.data, aes(x=year.actual, y=shame, 
#                                            colour=gwf.binary)) + 
#   geom_ribbon(aes(ymin=shame.lower, ymax=shame.upper, fill=gwf.binary), 
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) +
#   labs(x=NULL, y="Mean percent of all interstate\nevents that are conflictual") + 
#   scale_colour_manual(values=c(col.dem, col.auth), name=NULL) +
#   scale_fill_manual(values=c(col.dem, col.auth), name=NULL, guide=FALSE) +
#   scale_y_continuous(labels=percent, limits=c(0, 1)) +
#   theme_ath()
# 
# plot.states.severity <- ggplot(plot.data, aes(x=year.actual, y=severity, 
#                                               colour=gwf.binary)) + 
#   geom_ribbon(aes(ymin=severity.lower, ymax=severity.upper, fill=gwf.binary), 
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) + 
#   labs(x=NULL, y="Mean intensity of\ninterstate conflictual events") + 
#   scale_colour_manual(values=c(col.dem, col.auth), name=NULL) +
#   scale_fill_manual(values=c(col.dem, col.auth), name=NULL, guide=FALSE) +
#   scale_y_continuous(limits=c(0, 10)) +
#   theme_ath()
# 
# plot.events.states <- arrangeGrob(plot.states.shame, plot.states.severity, nrow=1)
# grid::grid.draw(plot.events.states)
# 
# fig.save.cairo(plot.events.states, filename="1-events-states", 
#                width=6, height=2)
# 
# #' Check if the difference in average shame percent is significant in each year
# year.diffs <- full.data %>%
#   select(gwf.binary, year.num, icews.conflict.severity.abs) %>%
#   na.omit() %>%
#   group_by(year.num) %>%
#   do(tidy(t.test(icews.conflict.severity.abs ~ gwf.binary, data=.)))
# 
# year.diffs %>% select(1:6) %>% print(n=nrow(.))
# #' Nope, not really.
# #' 
# 
# #' Check if the difference in average severity is significant in each year
# year.diffs <- full.data %>%
#   select(gwf.binary, year.num, icews.pct.shame) %>%
#   na.omit() %>%
#   group_by(year.num) %>%
#   do(tidy(t.test(icews.pct.shame ~ gwf.binary, data=.)))
# 
# year.diffs %>% select(1:6) %>% print(n=nrow(.))
# #' Again, nope.
# #' 
# 
# #' ## INGO shaming percent and severity
# plot.ingos.shame <- ggplot(plot.data, aes(x=year.actual, y=shame.ingo, 
#                                           colour=gwf.binary)) + 
#   geom_ribbon(aes(ymin=shame.ingo.lower, ymax=shame.ingo.upper, fill=gwf.binary), 
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) +
#   labs(x=NULL, y="Mean percent of all INGO-state\nevents that are conflictual") + 
#   scale_colour_manual(values=c(col.dem, col.auth), name=NULL) +
#   scale_fill_manual(values=c(col.dem, col.auth), name=NULL, guide=FALSE) +
#   scale_y_continuous(labels=percent, limits=c(0, 1)) +
#   theme_ath()
# 
# plot.ingos.severity <- ggplot(plot.data, aes(x=year.actual, y=severity.ingo, 
#                                              colour=gwf.binary)) + 
#   geom_ribbon(aes(ymin=severity.ingo.lower, ymax=severity.ingo.upper, fill=gwf.binary), 
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) + 
#   labs(x=NULL, y="Mean intensity of\nINGO-state conflictual events") + 
#   scale_colour_manual(values=c(col.dem, col.auth), name=NULL) +
#   scale_fill_manual(values=c(col.dem, col.auth), name=NULL, guide=FALSE) +
#   scale_y_continuous(limits=c(0, 10)) +
#   theme_ath()
# 
# plot.events.ingos <- arrangeGrob(plot.ingos.shame, plot.ingos.severity, nrow=1)
# grid::grid.draw(plot.events.ingos)
# 
# fig.save.cairo(plot.events.ingos, filename="1-events-ingos", 
#                width=6, height=2)
# 
# #' Check if the difference in average shame percent is significant in each year
# year.diffs <- full.data %>%
#   select(gwf.binary, year.num, icews.conflict.severity.abs.ingos) %>%
#   na.omit() %>%
#   group_by(year.num) %>%
#   do(tidy(t.test(icews.conflict.severity.abs.ingos ~ gwf.binary, data=.)))
# 
# year.diffs %>% select(1:6) %>% print(n=nrow(.))
# #' Nope.
# #' 
# 
# #' Check if the difference in average severity is significant in each year
# year.diffs <- full.data %>%
#   select(gwf.binary, year.num, icews.pct.shame.ingos) %>%
#   na.omit() %>%
#   group_by(year.num) %>%
#   do(tidy(t.test(icews.pct.shame.ingos ~ gwf.binary, data=.)))
# 
# year.diffs %>% select(1:6) %>% print(n=nrow(.))
# #' Nope.
# #' 
# 
# #' All four plots at the same time
# plot.events <- arrangeGrob(plot.states.shame + theme(legend.position="none"), 
#                            plot.states.severity + theme(legend.position="none"), 
#                            plot.ingos.shame, plot.ingos.severity)
# grid::grid.draw(plot.events)
# 
# fig.save.cairo(plot.events, filename="1-events-both", 
#                width=6, height=4)
