#' ---
#' title: "Determinants of international civil society restrictions"
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
#+ load_data_libraries, message=FALSE
knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

library(printr)
library(magrittr)
library(dplyr)
library(purrr)
library(broom)
library(feather)
library(rstanarm)
library(ggplot2)
library(scales)
library(gridExtra)
library(Cairo)
library(stargazer)
library(countrycode)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

full.data <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.feather")) %>%
  mutate(case.study = cowcode %in% c(710, 775, 651, 663, 365, 705)) %>%
  # Make these variables more interpretable unit-wise
  mutate_each(funs(. * 100), dplyr::contains("pct")) %>%
  mutate(.rownames = rownames(.))

my.seed <- 1234
set.seed(my.seed)

options(mc.cores = parallel::detectCores())  # Use all possible cores


#' # Variables
#' 
#' ## Dependent variable
#' 
#' Civil society regulatory environment: 
#' 
#' - `v2csreprss`: V-Dem CSO repression ("Does the government attempt to 
#'   repress civil society organizations?). Higher numbers = less repression.
#' - `v2csreprss_ord`: Ordinal version of `v2csreprss` (labels = "Severely", 
#'   "Substantially", "Moderately", "Weakly", "No")
#' - `v2cseeorgs`: V-Dem CSO entry and exit ("To what extent does the 
#'   government achieve control over entry and exit by civil society 
#'   organizations (CSOs) into public life?"). Higher numbers = less control.
#' - `cs_env_sum`: Sum of `v2csreprss` and `v2cseeorgs`. V-Dem includes a 
#'   "Core civil society index" that uses Bayesian factor analysis to combine 
#'   repression, entry/exit, *and* participatory environment. I'm less 
#'   interested in the participatory environment. The [paper describing the 
#'   original factor analysis model][vdem13], however, does not explain how it 
#'   was built, so here I make a simple additive index of the two variables.
#' - `cs_env_sum.lead`: `cs_env_sum` in the following time period (easier than 
#'   lagging every single variable).
#'
#' ## Explanatory variables
#' 
#' Internal stability:
#' 
#' - `icrg.stability`: Government stability (0-12; 12 is stable, 0 is unstable)
#' - `icrg.pol.risk.internal`: `icrg.stability` + `icrg.socioeconomic` + 
#'   `icrg.investment` + `icrg.internal` + `icrg.corruption` +
#'   `icrg.military` + `icrg.religion` + `icrg.law` + `icrg.ethnic` + 
#'   `icrg.bureau`
#' - `icrg.pol.risk.internal.scaled`: `icrg.pol.risk.internal` rescaled to
#'    0-100 scale
#' - `yrsoffc`: Years the executive has been in office
#' - `years.since.comp`: Years since last competitve election
#' - `opp1vote`: Opposition vote share
#' 
#' External stability:
#' 
#' - `neighbor.pol.risk.XXX`
#' 
#' International reputation: 
#' 
#' - Variable from ICEWS / shaming data from Murdie
#' 
#' Controls:
#' 
#' - `e_polity2`: 
#' - `physint`: 
#' - `gdpcap.log`: 
#' - `population.log`: 
#' - `oda.log`: 
#' - `countngo`: 
#' - `globalization`: 
#' 
#' [vdem13]: https://www.v-dem.net/media/filer_public/47/2e/472eec11-830f-4578-9a09-d9f8d43cee3a/v-dem_working_paper_2015_13_edited.pdf


#' # Large-N analysis (LNA) and modeling
#' 
#' ## Model construction
#' 
#' Note on time frame of variables: Not all variables overlap perfectly with
#' V-Dem. Models that include any of the following variables will be inherently
#' limited to the corresponding years (since non-overlapping years are
#' dropped):
#' 
#' - ICRG: 1991-2014
#' - ICEWS: 1995-2015
#' - ICEWS EOIs: 2000-2014
#' 

#+ model_specifications
lna.simple.stability <- function(df) {
  lm(cs_env_sum.lead ~ 
       icrg.stability +
       icrg.pol.risk_wt +
       shaming.states.std +
       shaming.ingos.std + 
       as.factor(year.num),
     data=df)
}

lna.simple.internal <- function(df) {
  lm(cs_env_sum.lead ~ 
       icrg.internal +
       icrg.pol.risk_wt +
       shaming.states.std +
       shaming.ingos.std + 
       as.factor(year.num),
     data=df)
}

lna.internal <- function(df) {
  lm(cs_env_sum.lead ~ 
       icrg.internal +
       as.factor(year.num),
     data=df)
}

lna.stability <- function(df) {
  lm(cs_env_sum.lead ~ 
       icrg.stability +
       as.factor(year.num),
     data=df)
}

lna.full.stability <- function(df) {
  lm(cs_env_sum.lead ~ 
       # Internal
       icrg.stability +
       yrsoffc + years.since.comp + opp1vote +
       # External
       # any.crisis_pct_wt +
       # insurgency_pct_mean_nb +
       icrg.pol.risk_wt + 
       any.crisis_pct_mean_nb +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt +
       protests.nonviolent.std_wt +
       # Shaming
       shaming.states.std +
       shaming.ingos.std +
       # Minimal controls
       as.factor(year.num),
     data=df)
}

lna.full.internal <- function(df) {
  lm(cs_env_sum.lead ~ 
       # Internal
       icrg.internal + 
       yrsoffc + years.since.comp + opp1vote +
       # External
       # any.crisis_pct_wt +
       # insurgency_pct_mean_nb +
       icrg.pol.risk_wt + 
       any.crisis_pct_mean_nb +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt +
       protests.nonviolent.std_wt +
       # Shaming
       shaming.states.std +
       shaming.ingos.std +
       # Minimal controls
       as.factor(year.num),
     data=df)
}

lna.simple.b <- function(df) {
  stan_glm(cs_env_sum.lead ~ 
             icrg.stability + icrg.internal +
             icrg.pol.risk_wt +
             shaming.states.std +
             shaming.ingos.std + 
             as.factor(year.num),
           data=df, family=gaussian(), 
           prior=cauchy(), prior_intercept=cauchy(), 
           seed=my.seed)
}

lna.full.b <- function(df) {
  stan_glm(cs_env_sum.lead ~ 
             # Internal
             icrg.stability + icrg.internal +
             yrsoffc + years.since.comp + opp1vote +
             # External
             # any.crisis_pct_wt +
             # insurgency_pct_mean_nb +
             icrg.pol.risk_wt + 
             any.crisis_pct_mean_nb +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt +
             protests.nonviolent.std_wt +
             # Shaming
             shaming.states.std +
             shaming.ingos.std +
             # Minimal controls
             as.factor(year.num),
           data=df, family=gaussian(), 
           prior=cauchy(), prior_intercept=cauchy(), 
           seed=my.seed)
}

# List of clean model names for later left_join()ing
model.names.clean <- data_frame(model.name = c("lna.simple",
                                               "lna.simple.b",
                                               "lna.full",
                                               "lna.full.b"),
                                model.name.clean = c("Simple model",
                                                     "Simple model (Bayesian)",
                                                     "Full model",
                                                     "Full model (Bayesian)"))

# Amazing. Manage tons of models with dplyr, tidyr, and purrr (from Hadley's
# presentation at https://www.youtube.com/watch?v=rz3_FDVt9eg - around 27:00ish)
#
# This takes forever because of the Bayesian stuff
#
#+ model_calculation
autocracies.gwf <- filter(full.data, gwf.ever.autocracy)
cows.gwf <- unique(autocracies.gwf$cowcode)

autocracies.uds <- filter(full.data, uds.ever.autocracy)
cows.uds <- unique(autocracies.uds$cowcode)

autocracies <- full.data %>%
  filter(cowcode %in% unique(c(cows.uds, cows.gwf)))

autocracies.combined <- data_frame(subset.type = c("GWF", "UDS", "Both"),
                                   data = list(autocracies.gwf,
                                               autocracies.uds,
                                               autocracies))

models.raw <- autocracies.combined %>%
  # Run all the models
  mutate(lna.simple.stability = data %>% map(lna.simple.stability),
         lna.simple.internal = data %>% map(lna.simple.internal),
         lna.full.stability = data %>% map(lna.full.stability),
         lna.full.internal = data %>% map(lna.full.internal),
         lna.internal = data %>% map(lna.internal),
         lna.stability = data %>% map(lna.stability)) #%>%
  # Bayesian models
  # mutate(lna.simple.b = data %>% map(lna.simple.b),
  #        lna.full.b = data %>% map(lna.full.b))

models <- models.raw %>%
  gather(model.name, model, -subset.type, -data) %>%
  left_join(model.names.clean, by="model.name") %>%
  mutate(glance = model %>% map(broom::glance),
         tidy = model %>% map(broom::tidy),
         augment = model %>% map(broom::augment))

#+ results="asis"
stargazer(models$model, type="html", omit="\\.factor")

# 
# stargazer.simple <- function(x, col.labs=NULL) {
#   stargazer(x, column.labels=col.labs, type="text", omit="\\.factor")
# }
# stargazer.simple(models$model, col.labs=models$model.name)
# 
# #' ## Internal factors
# #' 
# #' ### Models
# #' 
# lna.internal.simple <- lm(cs_env_sum.lead ~ 
#                             # Internal (low is bad; high is good)
#                             icrg.stability + icrg.internal + 
#                             as.factor(year.num),
#                           data=autocracies)
# 
# lna.internal.full <- lm(cs_env_sum.lead ~ 
#                           icrg.stability + icrg.internal + 
#                           yrsoffc + years.since.comp + opp1vote +
#                           as.factor(year.num),
#                         data=autocracies)
# 
# lna.internal.alt <- lm(cs_env_sum.lead ~ 
#                          icrg.pol.risk.internal.scaled + 
#                          yrsoffc + years.since.comp + opp1vote +
#                          as.factor(year.num),
#                        data=autocracies)
# 
# #+ results='asis'
# stargazer(lna.internal.simple, lna.internal.full, 
#           lna.all.simple, lna.all.full,
#           type="html", 
#           dep.var.caption="CSRE in following year",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           omit="\\.factor",
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 4))))
# 
# #' ### Coefficient plot
# #' 
# vars.included <- c("icrg.stability", "icrg.internal", "yrsoffc", 
#                    "years.since.comp", "opp1vote")
# 
# coef.plot.int <- fig.coef(list("Simple" = lna.internal.simple,
#                                "All internal factors" = lna.internal.full,
#                                "All factors (excerpt)" = lna.all.full), 
#                           xlab="Civil society regulatory environment (CSRE)",
#                           vars.included=vars.included)
# coef.plot.int
# 
# fig.save.cairo(coef.plot.int, filename="1-coefs-lna-int",
#                width=6, height=3)
# 
# 
# #' ## External factors
# #' 
# #' ### Models
# #' 
# lna.external.simple <- lm(cs_env_sum.lead ~ 
#                              icrg.pol.risk_wt +
#                             as.factor(year.num),
#                           data=autocracies)
# 
# lna.external.full <- lm(cs_env_sum.lead ~ 
#                           # TODO: Consistent wt vs nb variables? Or 
#                           # justification for them being different?
#                           icrg.pol.risk_wt + 
#                           any.crisis_pct_mean_nb +
#                           coups.activity.bin_sum_nb +
#                           protests.violent.std_wt +
#                           protests.nonviolent.std_wt +
#                           as.factor(year.num),
#                         data=autocracies)
# 
# #+ results='asis'
# stargazer(lna.external.simple, lna.external.full,
#           lna.all.simple, lna.all.full,
#           type="html", 
#           dep.var.caption="CSRE in following year",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           omit="\\.factor",
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 4))))
# 
# 
# #' ### Coefficient plot
# #' 
# vars.included <- c("icrg.pol.risk_wt", "any.crisis_pct_mean_nb", 
#                    "coups.activity.bin_sum_nb", "protests.violent.std_wt", 
#                    "protests.nonviolent.std_wt")
# 
# coef.plot.ext <- fig.coef(list("Simple" = lna.external.simple,
#                                "All external factors" = lna.external.full,
#                                "All factors (excerpt)" = lna.all.full), 
#                           xlab="Civil society regulatory environment (CSRE)",
#                           vars.included=vars.included)
# coef.plot.ext
# 
# fig.save.cairo(coef.plot.ext, filename="1-coefs-lna-ext",
#                width=6, height=3)
# 
# 
# #' ## Shaming factors
# #' 
# #' ### Models
# #' 
# lna.shame.simple <- lm(cs_env_sum.lead ~
#                          # TODO: Deal with proper normalized or weighted values
#                          # shaming.states.pct.govt +
#                          # shaming.ingos.pct.ingo +
#                          shaming.states.std +
#                          shaming.ingos.std +
#                          as.factor(year.num),
#                        data=autocracies)
# 
# #+ results='asis'
# stargazer(lna.shame.simple, lna.all.simple, lna.all.full,
#           type="html", 
#           dep.var.caption="CSRE in following year",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           omit="\\.factor",
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 3))))
# 
# #' ### Coefficient plot
# #' 
# vars.included <- c("shaming.states.std", "shaming.ingos.std")
# coef.plot.shame <- fig.coef(list("All shaming factors" = lna.shame.simple,
#                                  "All factors (excerpt)" = lna.all.full), 
#                             xlab="Civil society regulatory environment (CSRE)",
#                             vars.included=vars.included)
# coef.plot.shame
# 
# fig.save.cairo(coef.plot.shame, filename="1-coefs-lna-shame",
#                width=6, height=3)
# 
# 
# #' ## All factors at once
# #' 
# #' ### Models
# #' 
# #' Actual full models run previously so they can be compared to smaller models.
# #' 
# #' Here I show the results of all models side-by-side.
# #' 
# var.labs <-  c(
#   "Internal stability (ICRG)",
#   "Internal conflict (ICRG)",
#   "Years executive in office",
#   "Years since competitive election",
#   "Opposition vote share",
#   "Average political risk in neighboring countries (ICRG)",
#   "Government-based shaming reports (% of all events)",
#   "INGO-based shaming reports (% of all events)",
#   "Average time in crisis in neighboring countries (% of a year)",
#   "Coup activity in neighboring countries (binary)",
#   "Violent protests, weighted by distance (relative within country)",
#   "Nonviolent protests, weighted by distance (relative within country)"
# )
# 
# #+ results='asis'
# stargazer(lna.internal.simple, lna.internal.full,
#           lna.external.simple, lna.external.full,
#           lna.shame.simple, lna.all.simple, lna.all.full,
#           type="html",
#           dep.var.caption="CSRE in following year",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           # covariate.labels=var.labs,
#           omit="\\.factor",
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 7)))#,
#           # out=file.path("~/Desktop/all.html")
#           )
# 
# #' ### Coefficient plot
# #' 
# coef.plot.all <- fig.coef(list("Simple" = lna.all.simple,
#                                "Full" = lna.all.full), 
#                           xlab="Civil society regulatory environment (CSRE)")
# coef.plot.all
# 
# fig.save.cairo(coef.plot.all, filename="1-coefs-lna-all",
#                width=6, height=4.5)
# 
# # ggplot(autocracies, aes(x=shaming.ingos.pct.all, y=cs_env_sum.lead)) +
# #   geom_point(aes(text=paste(country_name, year.num), colour=case.study)) +
# #   geom_smooth() #+ facet_wrap(~ case.study)
# # plotly::ggplotly()
# 
# 
# #' ## Marginal effects
# #' 
# #' ### CSRE across internal variables
# new.data.int.stability <- lna.internal.full$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, icrg.stability, yrsoffc)) %>%
#   right_join(expand.grid(icrg.stability =
#                            seq(0, 12, by=0.1),
#                          yrsoffc = c(2, 30),
#                          index = 1),
#              by="index") %>%
#   select(-index)
# 
# new.data.int.conflict <- lna.internal.full$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, icrg.internal, yrsoffc)) %>%
#   right_join(expand.grid(icrg.internal =
#                            seq(0, 12, by=0.1), 
#                          yrsoffc = c(2, 30),
#                          index = 1),
#              by="index") %>%
#   select(-index)
# 
# plot.predict.stability <- lna.internal.full %>%
#   augment(newdata=new.data.int.stability) %>%
#   mutate(val.to.plot = icrg.stability,
#          var.name = "Internal stability")
# 
# plot.predict.conflict <- lna.internal.full %>%
#   augment(newdata=new.data.int.conflict) %>%
#   mutate(val.to.plot = icrg.internal,
#          var.name = "Internal conflict")
# 
# plot.predict <- bind_rows(plot.predict.stability, plot.predict.conflict) %>%
#   mutate(pred = .fitted,
#          pred.lower = pred + (qnorm(0.025) * .se.fit),
#          pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
#   mutate(yrsoffc = factor(yrsoffc, levels=c(2, 30), 
#                           labels=paste(c(2, 30), "years in office")))
# 
# plot.icrg.int.pred <- ggplot(plot.predict, 
#                              aes(x=val.to.plot, y=pred, 
#                                  fill=var.name, colour=var.name)) +
#   geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) +
#   labs(x="ICRG score", y="Predicted CSRE in following year") +
#   scale_colour_manual(values=c(col.auth, col.dem), name=NULL) +
#   scale_fill_manual(values=c(col.auth, col.dem), name=NULL, guide=FALSE) +
#   coord_cartesian(ylim=c(-4, 4)) +
#   theme_ath() + 
#   facet_wrap(~ yrsoffc)
# plot.icrg.int.pred
# 
# fig.save.cairo(plot.icrg.int.pred, filename="1-icrg-int-pred",
#                width=5, height=3)
# 
#  
# #' ### CSRE across external variables
# new.data.ext <- lna.external.full$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, protests.nonviolent.std_wt,
#             icrg.pol.risk_wt)) %>%
#   right_join(expand.grid(protests.nonviolent.std_wt = c(1, 2.5, 5),
#                          icrg.pol.risk_wt = seq(40, 100, by=1),
#                          index=1),
#              by="index") %>%
#   select(-index)
# 
# plot.predict.ext.icrg.protests <- lna.external.full %>%
#   augment(newdata=new.data.ext) %>%
#   mutate(pred = .fitted,
#          pred.lower = pred + (qnorm(0.025) * .se.fit),
#          pred.upper = pred + (qnorm(0.975) * .se.fit),
#          protests = factor(protests.nonviolent.std_wt, levels=c(1, 2.5, 5),
#                            labels=c("Much fewer than normal (1)", "Normal (2.5)", 
#                                     "Much more than normal (5)"))) %>%
#   # Remove confidence intervals for normal levels of protests
#   mutate(pred.lower = ifelse(protests.nonviolent.std_wt == 2.5, NA, pred.lower),
#          pred.upper = ifelse(protests.nonviolent.std_wt == 2.5, NA, pred.upper))
# 
# plot.ext.icrg.nonviolent <- ggplot(plot.predict.ext.icrg.protests, 
#                                    aes(x=icrg.pol.risk_wt, y=pred,
#                                        colour=protests)) + 
#   geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=protests),
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) +
#   labs(x="Average political risk in neighboring countries (ICRG)",
#        y="Predicted CSRE in following year") + 
#   scale_colour_manual(values=c("#004259", "grey50", "#4A0A3D"), name=NULL) +
#   scale_fill_manual(values=c("#004259", "grey50" ,"#4A0A3D"), name=NULL, guide=FALSE) +
#   theme_ath()
# plot.ext.icrg.nonviolent
# 
# # fig.save.cairo(plot.subregion.coup.pred, filename="1-icrg-subregion-coup-ext-pred", 
# #                width=5, height=2.5)
# # 
# 
# #' ### CSRE across shaming variables
# new.data.shame.states <- lna.all.full$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, shaming.states.std)) %>%
#   right_join(expand.grid(shaming.states.std =
#                            seq(1, 5, by=0.1),
#                          index = 1),
#              by="index") %>%
#   select(-index)
# 
# new.data.shame.ingos <- lna.all.full$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, shaming.ingos.std)) %>%
#   right_join(expand.grid(shaming.ingos.std =
#                            seq(1, 5, by=0.1),
#                          index = 1),
#              by="index") %>%
#   select(-index)
# 
# plot.predict.states <- lna.all.full %>%
#   augment(newdata=new.data.shame.states) %>%
#   mutate(val.to.plot = shaming.states.std,
#          var.name = "State-based shaming")
# 
# plot.predict.ingos <- lna.all.full %>%
#   augment(newdata=new.data.shame.ingos) %>%
#   mutate(val.to.plot = shaming.ingos.std,
#          var.name = "INGO-based shaming")
# 
# plot.predict.shame <- bind_rows(plot.predict.states, plot.predict.ingos) %>%
#   mutate(pred = .fitted,
#          pred.lower = pred + (qnorm(0.025) * .se.fit),
#          pred.upper = pred + (qnorm(0.975) * .se.fit))
# 
# plot.shame.states.ingos <- ggplot(plot.predict.shame, 
#                                   aes(x=val.to.plot, y=pred, 
#                                       fill=var.name, colour=var.name)) +
#   geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) +
#   labs(x="Level of shaming", y="Predicted CSRE in following year") +
#   scale_colour_manual(values=c(col.auth, col.dem), name=NULL) +
#   scale_fill_manual(values=c(col.auth, col.dem), name=NULL, guide=FALSE) +
#   coord_cartesian(ylim=c(-2, 4)) +
#   theme_ath() 
# plot.shame.states.ingos
# 
# fig.save.cairo(plot.shame.states.ingos, filename="1-shame-states-ingos",
#                width=5, height=3)
# 
# 
# #' ## Robustness checks
# #' 
# #' ### Different external variables
# #'
# robust.ext.no.eois <- lm(cs_env_sum.lead ~ 
#                            icrg.pol.risk_wt + 
#                            coups.activity.bin_sum_nb +
#                            protests.violent.std_wt +
#                            protests.nonviolent.std_wt +
#                            as.factor(year.num),
#                          data=autocracies)
# 
# robust.ext.risk.nb <- lm(cs_env_sum.lead ~ 
#                            icrg.pol.risk_mean_nb + 
#                            any.crisis_pct_mean_nb +
#                            coups.activity.bin_sum_nb +
#                            protests.violent.std_wt +
#                            protests.nonviolent.std_wt +
#                            as.factor(year.num),
#                          data=autocracies)
# 
# robust.ext.risk.nb.no.eois <- lm(cs_env_sum.lead ~ 
#                                    icrg.pol.risk_mean_nb + 
#                                    coups.activity.bin_sum_nb +
#                                    protests.violent.std_wt +
#                                    protests.nonviolent.std_wt +
#                                    as.factor(year.num),
#                                  data=autocracies)
# 
# robust.ext.protests.prop.wt <- lm(cs_env_sum.lead ~ 
#                                     icrg.pol.risk_wt + 
#                                     any.crisis_pct_mean_nb +
#                                     coups.activity.bin_sum_nb +
#                                     protests.violent.pct.all_wt +
#                                     protests.nonviolent.pct.all_wt +
#                                     as.factor(year.num),
#                                   data=autocracies)
# 
# robust.ext.protests.prop.nb <- lm(cs_env_sum.lead ~ 
#                                     icrg.pol.risk_wt + 
#                                     any.crisis_pct_mean_nb +
#                                     coups.activity.bin_sum_nb +
#                                     protests.violent.pct.all_mean_nb +
#                                     protests.nonviolent.pct.all_mean_nb +
#                                     as.factor(year.num),
#                                   data=autocracies)
# 
# robust.ext.protests.prop.wt.no.eois <- lm(cs_env_sum.lead ~ 
#                                             icrg.pol.risk_wt + 
#                                             coups.activity.bin_sum_nb +
#                                             protests.violent.pct.all_wt +
#                                             protests.nonviolent.pct.all_wt +
#                                             as.factor(year.num),
#                                           data=autocracies)
# 
# robust.ext.protests.prop.nb.no.eois <- lm(cs_env_sum.lead ~ 
#                                             icrg.pol.risk_wt + 
#                                             coups.activity.bin_sum_nb +
#                                             protests.violent.pct.all_mean_nb +
#                                             protests.nonviolent.pct.all_mean_nb +
#                                             as.factor(year.num),
#                                           data=autocracies)
# 
# robust.ext.protests.log.wt <- lm(cs_env_sum.lead ~ 
#                                    icrg.pol.risk_wt + 
#                                    any.crisis_pct_mean_nb +
#                                    coups.activity.bin_sum_nb +
#                                    protests.violent.log_wt +
#                                    protests.nonviolent.log_wt +
#                                    as.factor(year.num),
#                                  data=autocracies)
# 
# robust.ext.protests.log.nb <- lm(cs_env_sum.lead ~ 
#                                    icrg.pol.risk_wt + 
#                                    any.crisis_pct_mean_nb +
#                                    coups.activity.bin_sum_nb +
#                                    protests.violent.log_mean_nb +
#                                    protests.nonviolent.log_mean_nb +
#                                    as.factor(year.num),
#                                  data=autocracies)
# 
# robust.ext.protests.log.wt.no.eois <- lm(cs_env_sum.lead ~ 
#                                            icrg.pol.risk_wt + 
#                                            coups.activity.bin_sum_nb +
#                                            protests.violent.log_wt +
#                                            protests.nonviolent.log_wt +
#                                            as.factor(year.num),
#                                          data=autocracies)
# 
# robust.ext.protests.log.nb.no.eois <- lm(cs_env_sum.lead ~ 
#                                            icrg.pol.risk_wt + 
#                                            coups.activity.bin_sum_nb +
#                                            protests.violent.log_mean_nb +
#                                            protests.nonviolent.log_mean_nb +
#                                            as.factor(year.num),
#                                          data=autocracies)
# 
# #+ results='asis'
# stargazer(lna.external.simple, lna.external.full,
#           lna.all.simple, lna.all.full, 
#           robust.ext.no.eois, robust.ext.risk.nb,
#           robust.ext.risk.nb.no.eois,
#           type="html", 
#           dep.var.caption="CSRE in following year",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           omit="\\.factor",
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 7))))
# 
# #+ results='asis'
# stargazer(lna.external.simple, lna.external.full,
#           lna.all.simple, lna.all.full,
#           robust.ext.protests.prop.nb,
#           robust.ext.protests.prop.wt,
#           robust.ext.protests.log.wt,
#           robust.ext.protests.log.nb,
#           robust.ext.protests.prop.nb.no.eois,
#           robust.ext.protests.prop.wt.no.eois,
#           robust.ext.protests.log.wt.no.eois,
#           robust.ext.protests.log.nb.no.eois,
#           type="html", 
#           dep.var.caption="CSRE in following year",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           omit="\\.factor",
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 12))))
# 
# 
# #' 
# #' ### Different internal variables
# #' 
# 
# #' # Nested analysis case selection
# #'
# #' Use the basic model with minimal variables, in part because of the
# #' principles of LNA, and in part because there are so many dropped, incomplete
# #' cases when using years in office, opposition vote, and ICEWS EOIs.
# #' 
# #' Because the results from the LNA are inconclusive and not that robust, I
# #' move to model-building SNA, where I choose cases deliberately that are both
# #' on and off the line. With model *testing* SNA, you only choose cases on the
# #' line with widest variation in explanatory variables. With model *building*
# #' SNA, you choose at least one case off the line, and you remain wary about
# #' cases on the line. With Mb-SNA, cases are selected on the dependent
# #' variable: 
# #' 
# #' > The very nature of Mb-SNA implies that we may lack the scores on the
# #' explanatory variables of interest at the outset of the project, making it
# #' impossible to use the explanatory variables for case selection. (p. 445)
# #' 
# #' This might seemut it's okay: the nestedness takes care of lots of
# #' those concerns:
# #' 
# #' > Because causal inference in the nested approach does not rely solely on
# #' the small-N portion, the standard pitfalls of selection bias are less likely
# #' to lead to faulty inferences. (p. 446)
# #' 
# #' TODO: Find Uzbekistan, other -stans?  
# #' TODO: Find a couple other Mb-SNA cases
# #' 
# #' Issues with model:
# #' 
# #' - Doesn't predict extremes - everything is within ≈−3–3 range
# #' - Not all hypotheses check out; not very robust
# #' 
# lna.selection.data <- lna.all.simple %>%
#   augment() %>%
#   left_join(select(autocracies, .rownames, country_name, cowcode), by=".rownames") %>%
#   mutate(case.study = cowcode %in% c(710, 651, 365),
#          country.plot = ifelse(case.study, country_name, "Other"),
#          country.plot = factor(country.plot, levels=c("China", "Egypt", "Russia", "Other"),
#                                ordered=TRUE))
# 
# plot.lna.selection <- ggplot(lna.selection.data, 
#                              aes(x=.fitted, y=cs_env_sum.lead,
#                                  colour=country.plot, label=country_name)) +
#   geom_segment(x=-6, xend=6, y=-6, yend=6, colour="grey75", size=0.5) + 
#   geom_point(aes(alpha=country.plot, size=country.plot)) +
#   stat_ellipse(aes(linetype=country.plot), type="norm", size=0.5) +
#   scale_linetype_manual(values=c(1, 1, 1, 0), guide=FALSE) +
#   scale_color_manual(values=c("#CC3340", "#6B4A3D", "#00A1B0", "grey50"), name=NULL) +
#   scale_alpha_manual(values=c(1, 1, 1, 0.25), guide=FALSE) +
#   scale_size_manual(values=c(1, 1, 1, 0.5), guide=FALSE) +
#   labs(x="Predicted CSRE", y="Actual CSRE") +
#   coord_cartesian(xlim=c(-6, 6), ylim=c(-6, 6)) + 
#   theme_ath()
# 
# plot.lna.selection
# 
# # plotly::ggplotly(plot.lna.selection, tooltip="label")
# 
# 
# #' # Bayesian tinkering?
# #'
# #' See:
# #'
# #' - https://cran.r-project.org/web/packages/rstanarm/vignettes/lm.html
# #' - https://cran.r-project.org/web/packages/rstanarm/vignettes/continuous.html
# #' - https://thinkinator.com/2016/01/12/r-users-will-now-inevitably-become-bayesians/
# #' - http://datascienceplus.com/bayesian-regression-with-stan-part-1-normal-regression/
# #' - https://stats.stackexchange.com/questions/2272/whats-the-difference-between-a-confidence-interval-and-a-credible-interval
# #'
# #' Good, solid, interpretable results, but craaaaazy slow
# 
# 
# 
# lna.all.simple.b <- stan_glm(cs_env_sum.lead ~ 
#                        icrg.stability + icrg.internal +
#                        icrg.pol.risk_wt +
#                        shaming.states.std +
#                        shaming.ingos.std + 
#                        as.factor(year.num),
#                      data=autocracies.gwf, family=gaussian(), 
#                      prior=cauchy(), prior_intercept=cauchy(), 
#                      seed=my.seed)
# 
# print(lna.all.simple.b)
# plot(lna.all.simple.b) + geom_vline(xintercept=0)
# pp_check(lna.all.simple.b, check="distributions", overlay=FALSE, nreps=5)
# 
# 
# lna.all.full.b <- stan_glm(cs_env_sum.lead ~ 
#                              # Internal
#                              icrg.stability + icrg.internal +
#                              yrsoffc + years.since.comp + opp1vote +
#                              # External
#                              # any.crisis_pct_wt +
#                              # insurgency_pct_mean_nb +
#                              icrg.pol.risk_wt + 
#                              any.crisis_pct_mean_nb +
#                              coups.activity.bin_sum_nb +
#                              protests.violent.std_wt +
#                              protests.nonviolent.std_wt +
#                              # Shaming
#                              shaming.states.std +
#                              shaming.ingos.std +
#                              # Minimal controls
#                              as.factor(year.num),
#                            data=autocracies, family=gaussian(), 
#                            prior=cauchy(), prior_intercept=cauchy(), 
#                            seed=my.seed)
# 
# print(lna.all.full.b)
# plot(lna.all.full.b, pars=c("icrg.stability", "icrg.internal", "yrsoffc")) + geom_vline(xintercept=0) + theme_ath()
# pp_check(lna.all.full.b, check="distributions", overlay=FALSE, nreps=5)
# 
# ci95 <- posterior_interval(lna.all.full.b, prob = 0.95)
# round(ci95, 2)
# 
# cases <- data_frame(cowcode = c(710, 651, 365, 663, 775, 705),
#                     country.name = countrycode(cowcode, "cown", "country.name"),
#                     colour = ath.palette("palette1", n=6),
#                     linetype = 1, alpha = 1, point.size = 1)
# 
# lna.selection.data.b <- lna.all.simple.b %>%
#   augment() %>%
#   left_join(select(autocracies, .rownames, cowcode), by=".rownames") %>%
#   left_join(cases, by="cowcode") %>%
#   mutate(country.name = ifelse(is.na(country.name), "Other", country.name),
#          colour = ifelse(is.na(colour), "grey70", colour),
#          linetype = ifelse(is.na(linetype), 0, linetype),
#          alpha = ifelse(is.na(alpha), 0.25, alpha),
#          point.size = ifelse(is.na(point.size), 0.55, point.size)) %>%
#   mutate(country.name = factor(country.name, levels=c(cases$country.name, "Other"),
#                                ordered=TRUE),
#          colour = factor(colour, levels=c(cases$colour, "grey70"),
#                          ordered=TRUE))
# 
# plot.lna.selection.b <- ggplot(lna.selection.data.b,
#                                aes(x=.fitted, y=cs_env_sum.lead,
#                                    colour=colour)) +
#   geom_segment(x=-6, xend=6, y=-6, yend=6, colour="grey75", size=0.5) +
#   geom_point(aes(alpha=alpha, size=point.size)) +
#   stat_ellipse(aes(linetype=linetype), type="norm", size=0.5) +
#   scale_color_identity(guide="legend", labels=c(cases$country.name, "Other"), 
#                        name=NULL) +
#   scale_size_identity() +
#   scale_alpha_identity() +
#   scale_linetype_identity() +
#   labs(x="Predicted CSRE", y="Actual CSRE") +
#   coord_cartesian(xlim=c(-3, 3), ylim=c(-6, 6)) +
#   theme_ath()
# 
# plot.lna.selection.b
# 
# # cat(lna.all.simple.b$stanfit@stanmodel@model_code, file="~/Desktop/blah.txt")
# #
# # model.simple.b <- stan_glm(cs_env_sum.lead ~ icrg.stability + e_polity2,
# #                           data=full.data, family=gaussian(),
# #                           prior=cauchy(), prior_intercept=cauchy(),
# #                           seed=my.seed)
# # print(model.simple.b, digits=2)
# # plot(model.simple.b, pars=c("icrg.stability", "e_polity2"))
# # pp_check(model.simple.b, check="distributions", overlay=FALSE, nreps=5)
