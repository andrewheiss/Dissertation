#' ---
#' title: "Non-regression analysis"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: ../html/fixes.css
#'     toc: yes
#'     highlight: pygments
#'     theme: cosmo
#'     includes:
#'       after_body: ../html/jump.html
#' ---

#' # Load libraries and data
library(magrittr)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(gridExtra)
library(Cairo)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

full.data <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.rds"))


#' # General data summary
#' 
#' Number of countries:
length(unique(full.data$Country))

#' Range of years:
min(full.data$year.num)
max(full.data$year.num)

#' # Understanding and visualizing civil society environment
#' 
#' The V-Dem Bayesian measurement model is cool and all, but it's really hard
#' to interpret by itself. Showing a few example countries can help. 
cs.plot.all <- full.data %>%
  filter(year.num > 2000) %>%
  group_by(Country) %>%
  summarise(env.mean = mean(cs_env_sum, na.rm=TRUE)) %>%
  filter(!is.na(env.mean)) %>%
  arrange(env.mean) %>%
  ungroup()

cs.plot <- bind_rows(cs.plot.all %>% slice(1:5),
                     data_frame(Country = "—", env.mean = 0),
                     cs.plot.all %>% slice((nrow(.) - 4):nrow(.))) %>%
  mutate(Country = factor(Country, levels=unique(Country), ordered=TRUE))

#' The usual suspects get their scores
cs.plot

#' Average volatility/range by regime type:
cs.plot.all %>% 
  left_join(select(full.data, Country, polity_ord), by="Country") %>%
  group_by(polity_ord) %>% summarise(index.change.avg = mean(env.mean))

cs.plot.all %>% 
  left_join(select(full.data, Country, polity_ord2), by="Country") %>%
  group_by(polity_ord2) %>% summarise(index.change.avg = mean(env.mean))

plot.csre.top.bottom <- ggplot(cs.plot, aes(x=Country, y=env.mean)) +
  geom_point(size=2) + 
  geom_segment(aes(yend=0, xend=Country), size=1) + 
  labs(x=NULL, y="Mean civil society regulatory environment index (CSRE)") +
  coord_flip() +
  theme_ath()
plot.csre.top.bottom

fig.save.cairo(plot.csre.top.bottom, filename="1-csre-top-bottom", 
               width=5, height=2.5)


#' # Visualizing basic correlation between regime type and CSRE
#' 
#' Regime type and CSRE are quite correlated
plot.data <- full.data %>%
  select(cs_env_sum, uds_mean, e_polity2) %>% 
  na.omit()

plot.data %>%
  summarise_each(funs(cor(., plot.data$cs_env_sum)), -cs_env_sum)

#' You can see that visually too
plot.polity <- ggplot(plot.data, aes(x=e_polity2, y=cs_env_sum)) + 
  geom_vline(xintercept=0, colour="#EA2E49", size=0.5) +
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") + 
  labs(x="Polity IV score", 
       y="Civil society regulatory environment\n(CSRE)") +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.uds <- ggplot(plot.data, aes(x=uds_mean, y=cs_env_sum)) + 
  geom_vline(xintercept=0, colour="#EA2E49", size=0.5) +
  geom_point(size=0.25, alpha=0.25) + 
  geom_smooth(method="lm", se=TRUE, colour="#014358") +
  labs(x="Mean UDS score", y=NULL) +
  scale_y_continuous(breaks=seq(-6, 6, 2)) +
  theme_ath()

plot.regime.csre <- arrangeGrob(plot.polity, plot.uds, nrow=1)
grid::grid.draw(plot.regime.csre)

fig.save.cairo(plot.regime.csre, filename="1-regime-csre", 
               width=5, height=2)


#' # Understanding and visualizing ICRG
#' 
#' Conceptualizing the political risk measure is a little tricky. Showing a few
#' example countries can help. Figure out which countries have change the
#' least/most since 2000:
risk.stats <- full.data %>%
  filter(year.num > 2000) %>%
  group_by(Country) %>%
  summarise(index.change = max(icrg.pol.risk.internal.scaled) - 
              min(icrg.pol.risk.internal.scaled)) %>%
  filter(!is.na(index.change)) %>%
  arrange(index.change) %T>%
  {print(head(., 5))} %>% {print(tail(., 5))}

#' Norway and Congo are the most consistent; Syria saw the biggest change.
#'
#' Average volatility/range by regime type:
risk.stats %>% 
  left_join(select(full.data, Country, polity_ord), by="Country") %>%
  group_by(polity_ord) %>% summarise(index.change.avg = mean(index.change))

risk.stats %>% 
  left_join(select(full.data, Country, polity_ord2), by="Country") %>%
  group_by(polity_ord2) %>% summarise(index.change.avg = mean(index.change))


#' Visualize changes:
example.countries <- c(652, 385, 484)
example.stability <- full.data %>%
  filter(year.num > 2000, cowcode %in% example.countries) %>%
  mutate(country.plot = factor(Country,
                               levels=c("Norway", "Syria", "Congo"), 
                               ordered=TRUE))

# example.stability$icrg.pol.grade.internal
# c(0, 49.99, 59.99, 69.99, 79.99, Inf)
plot.icrg.examples <- ggplot(example.stability, 
                             aes(x=year.actual, 
                                 y=icrg.pol.risk.internal.scaled,
                                 colour=country.plot)) + 
  geom_line(size=1.5) + 
  labs(x=NULL, y="Internal political risk (ICRG)") + 
  scale_colour_manual(values=c("#014358", "#FD7401", "#BEDB3A"), name=NULL) +
  theme_ath()
plot.icrg.examples

fig.save.cairo(plot.icrg.examples, filename="1-icrg-examples", 
               width=5, height=3)


#' # ICRG across regime type
#' 
#' Are autocracies necessarily more unstable than democracies? They are more
#' volatile, as shown above with `risk.stats` summaries…
plot.data <- full.data %>%
  group_by(polity_ord2, year.actual) %>%
  summarise(icrg = mean(icrg.pol.risk.internal.scaled, na.rm=TRUE),
            icrg.sd = sd(icrg.pol.risk.internal.scaled, na.rm=TRUE),
            icrg.se = icrg.sd / sqrt(n()),
            icrg.upper = icrg + (qnorm(0.975) * icrg.se),
            icrg.lower = icrg + (qnorm(0.025) * icrg.se)) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(polity_ord2 = factor(polity_ord2, levels=c("Democracy", "Autocracy"),
                              labels=c("Democracies    ", "Autocracies")))

plot.icrg.regime <- ggplot(plot.data, aes(x=year.actual, y=icrg, 
                                          colour=polity_ord2)) + 
  geom_ribbon(aes(ymin=icrg.lower, ymax=icrg.upper, fill=polity_ord2), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x=NULL, y="Mean internal political risk (ICRG)") + 
  scale_colour_manual(values=c("#BEDB3A", "#441152"), name=NULL) +
  scale_fill_manual(values=c("#BEDB3A", "#441152"), name=NULL, guide=FALSE) +
  theme_ath()
plot.icrg.regime

fig.save.cairo(plot.icrg.regime, filename="1-icrg-regime", 
               width=5, height=3)

#' Check if the difference in means is significant in each year
year.diffs <- full.data %>%
  select(polity_ord2, year.num, icrg.pol.risk.internal.scaled) %>%
  na.omit() %>%
  group_by(year.num) %>%
  do(tidy(t.test(icrg.pol.risk.internal.scaled ~ polity_ord2, data=.)))

year.diffs %>% select(1:6) %>% print(n=nrow(.))
#' Yup. They are.
#' 


#' # Other authoritarian stability variables and CSRE
#' 
#' All three variables seem moderately correlated with the CSRE
plot.data <- full.data %>%
  select(cs_env_sum, yrsoffc, years.since.comp, opp1vote) %>% 
  na.omit()

plot.data %>%
  summarise_each(funs(cor(., plot.data$cs_env_sum)), -cs_env_sum)

#' Plot the correlations
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

plot.auth.vars <- arrangeGrob(plot.yrs.offc, plot.yrs.since.comp, 
                              plot.opp.vote, nrow=1)
grid::grid.draw(plot.auth.vars)

fig.save.cairo(plot.auth.vars, filename="1-auth-vars", 
               width=6, height=2)
