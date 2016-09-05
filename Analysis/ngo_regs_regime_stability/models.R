#' ---
#' title: "Modeling the determinants of the CSRE"
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

#+ load_data_libraries, message=FALSE
knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

library(printr)
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(broom)
library(feather)
library(rstanarm)
library(ggplot2)
library(scales)
library(gridExtra)
library(Cairo)
library(pander)
library(stargazer)
library(countrycode)
library(DT)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('missing', '')
panderOptions('big.mark', ',')
panderOptions('digits', 2)
panderOptions('table.alignment.default', 'left')

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

full.data <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.feather")) %>%
  mutate(case.study = cowcode %in% c(710, 775, 651, 663, 365, 705)) %>%
  # Make these variables more interpretable unit-wise
  mutate_each(funs(. * 100), dplyr::contains("pct")) %>%
  mutate(.rownames = rownames(.))

my.seed <- 1234
set.seed(my.seed)

CHAINS <- 4
ITER <-2000
WARMUP <- 1000

options(mc.cores = parallel::detectCores())  # Use all possible cores

bayesgazer <- function(model) {
  model.posterior.probs <- as.data.frame(model) %>%
    summarise_each(funs(pp.greater0 = mean(. > 0),
                        pp.less0 = mean(. < 0))) %>%
    gather(key, value) %>%
    separate(key, c("term", "key"), sep="_p") %>%
    spread(key, value)
  
  model.credible.intervals <- posterior_interval(model, prob=0.95) %>%
    as.data.frame() %>%
    mutate(term = rownames(.))
  
  # Calculate the median and MAD_SD for the posterior predictive distribution
  # of y (the bottom part of summary(model) output)
  model.sample.avg <- as.data.frame(model$stanfit) %>%
    select(mean_PPD) %>%
    summarise(Median = median(mean_PPD),
              MAD_SD = mad(mean_PPD))
  
  model.glance <- glance(model)
  
  model.output <- tidy(model) %>%
    filter(!str_detect(term, "as\\.factor")) %>%
    left_join(model.credible.intervals, by="term") %>%
    left_join(model.posterior.probs, by="term") %>%
    left_join(coef.names, by="term") %>%
    arrange(term.clean) %>%
    select(`Term` = term.clean, `Posterior median` = estimate, 
           `Posterior SD` = std.error, `2.5%`, `97.5%`,
           `P(β > 0)` = p.greater0, `P(β < 0)` = p.less0) %>%
    mutate(Term = as.character(Term))
  
  # N = number of observations
  # PSS = posterior sample size
  # Sigma = standard deviation of the normally-distributed errors
  # MAD/MAD_SD = median absolute deviation
  model.bottom.row <- tibble::tribble(
    ~Term,                            ~`Posterior median`,
    "—",                              NA,
    "Year fixed effects included",    NA,
    "N",                              model.glance$nobs,
    "σ",                              model.glance$sigma,
    "Posterior sample size",          model.glance$pss,
    "—",                              NA,
    "Sample average posterior\\\npredictive distribution of y ($X = \\bar{x}$):", NA,
    "Median",                         model.sample.avg$Median,
    "Median absolute deviation (SD)", model.sample.avg$MAD_SD
  )
  
  final.output <- bind_rows(model.output, model.bottom.row)
}


#' # Variables
#' 
#' ## Selection of autocracies
#' 
#' Combining the definition of autocracies from GWF and UDS doesn't really have
#' a big effect on the results. It's possible to run all these models on
#' datasets for just GWF, just UDS, and all combined, but it takes literally
#' forever when run with Bayesian models. The code scaffolding for running
#' these robustness checks is still here, though, in the commented-out
#' `autocracies.list` below.
#' 
autocracies.gwf <- filter(full.data, gwf.ever.autocracy)
cows.gwf <- unique(autocracies.gwf$cowcode)

autocracies.uds <- filter(full.data, uds.ever.autocracy)
cows.uds <- unique(autocracies.uds$cowcode)

autocracies <- full.data %>%
  filter(cowcode %in% unique(c(cows.uds, cows.gwf))) %>%
  mutate(shaming.ingos.std = ifelse(is.nan(shaming.ingos.std), 0, shaming.ingos.std))

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
#' - `cs_env_sum.lead2`: `cs_env_sum` leaded by two years
#' - `cs_env_sum.lead3`: `cs_env_sum` leaded by three years
#'
#' ## Explanatory variables
#' 
#' ### Internal stability
#' 
#' - `icrg.stability`: Government stability (0-12; 12 is stable, 0 is unstable)
#' - `icrg.pol.risk.internal`: `icrg.stability` + `icrg.socioeconomic` + 
#'   `icrg.investment` + `icrg.internal` + `icrg.corruption` +
#'   `icrg.military` + `icrg.religion` + `icrg.law` + `icrg.ethnic` + 
#'   `icrg.bureau`
#' - `icrg.pol.risk.internal.nostab`: `icrg.socioeconomic` + 
#'   `icrg.investment` + `icrg.internal` + `icrg.corruption` + 
#'   `icrg.military` + `icrg.religion` + `icrg.law` + `icrg.ethnic` + 
#'   `icrg.bureau`
#' - `icrg.pol.risk.internal.scaled`: `icrg.pol.risk.internal` rescaled to
#'    0-100 scale
#' - `icrg.pol.risk.internal.nostab.scaled`: `icrg.pol.risk.internal.nostab`
#'    rescaled to 0-100 scale
#' - `yrsoffc`: Years the executive has been in office
#' - `years.since.comp`: Years since last competitve election
#' - `opp1vote`: Opposition vote share
#' 
#' ### External stability
#' 
#' - `icrg.pol.risk_wt`: `icrg.pol.risk.internal` of all other countries,
#'   weighted by distance
#' - `coups.activity.bin_sum_nb`: Count of coup attempts and actual coups in
#'   countries within 900 km of closest border
#' - `protests.violent.std_wt`: Relative measure of violent protests in all
#'   other countries, weighted by distance
#' - `protests.nonviolent.std_wt`: Relative measure of nonviolent protests in
#'   all other countries, weighted by distance
#' 
#' ### International reputation
#' 
#' - `shaming.states.std`: Relative measure of shaming events targeted at a
#'   country, originating from other states
#' - `shaming.ingos.std`: Relative measure of shaming events targeted at a
#'   country, originating from INGOs
#' 
#' [vdem13]: https://www.v-dem.net/media/filer_public/47/2e/472eec11-830f-4578-9a09-d9f8d43cee3a/v-dem_working_paper_2015_13_edited.pdf
#' 
#' ### Time frame of variables
#' 
#' Not all variables overlap perfectly with V-Dem. Models that include any of
#' the following variables will be inherently limited to the corresponding
#' years (since non-overlapping years are dropped):
#' 
#' - ICRG: 1991-2014
#' - ICEWS: 1995-2015
#' - ICEWS EOIs: 2000-2014
#' 
#' 
#' ## Summary of variables in models
vars.to.summarize <- coef.names %>%
  filter(term != "(Intercept)", term != "shaming.ingos.std")

vars.summarized <- autocracies %>%
  filter(year.num > 1994) %>%
  select(one_of(vars.to.summarize$term)) %>%
  summarise_each(funs(XXMean = mean(., na.rm=TRUE),
                      XXSD = sd(., na.rm=TRUE),
                      XXMedian = median(., na.rm=TRUE),
                      XXN = sum(!is.na(.)),
                      XXMin = min(., na.rm=TRUE),
                      XXMax = max(., na.rm=TRUE))) %>%
  gather(key, value) %>%
  separate(key, c("term", "key"), sep="_XX") %>%
  spread(key, value) %>%
  left_join(vars.to.summarize, by="term") %>%
  arrange(term.clean) %>%
  mutate(Variable = sprintf("%s (%s)", as.character(term.clean.breaks), source.clean),
         Variable = str_replace(Variable, "XXX", "\\\n")) %>%
  select(Variable, Mean, SD, Median, Min, Max, N) %>%
  as.data.frame()

#+ results="asis"
caption <- "Summary of all variables included in regression models {#tbl:var-summary}"
var.summary <- pandoc.table.return(vars.summarized, keep.line.breaks=TRUE,
                                   justify="lcccccc", caption=caption, style="multiline")

cat(var.summary)
cat(var.summary, file=file.path(PROJHOME, "Output", "tables", 
                                "1-var-summary.md"))


#' # Model building
#' 
#' ## Combination of variables
#' 
#' Because I'm trying different operationalizations of internal and external
#' stability/risk, I fit models using different combinations of explanatory
#' variables, labeled in the code with A-J:
#' 
#' - Internal
#'     - A: `icrg.stability`
#'     - B: `icrg.pol.risk.internal.scaled`
#'     - C: `icrg.stability` + `yrsoffc` + `years.since.comp` + `opp1vote`
#'     - D: `icrg.pol.risk.internal.scaled` + `yrsoffc` + `years.since.comp` + `opp1vote`
#'     - E: `icrg.stability` + `icrg.pol.risk.internal.nostab.scaled` + `yrsoffc` + `years.since.comp` + `opp1vote`
#'     - J: `icrg.stability` + `icrg.pol.risk.internal.nostab.scaled`
#' - External
#'     - F: `icrg.pol.risk_wt`
#'     - G: `coups.activity.bin_sum_nb` + `protests.violent.std_wt` + `protests.nonviolent.std_wt`
#'     - H: `icrg.pol.risk_wt` + `coups.activity.bin_sum_nb` + `protests.violent.std_wt` + `protests.nonviolent.std_wt`
#' - Shaming:
#'     - I: `shaming.states.std` 
#'     - (No INGO shaming because that variable is horribly unreliable and unstable)
#' 

# Load the model definitions from other file
source(file.path(PROJHOME, "Analysis", 
                 "ngo_regs_regime_stability", 
                 "model_definitions.R"))

#' ## Frequentist results
#' 
# Amazing. Manage tons of models with dplyr, tidyr, and purrr (from Hadley's
# presentation at https://www.youtube.com/watch?v=rz3_FDVt9eg - around 27:00ish)

# Make a dataframe with a column of subsetted dataframes
# autocracies.list <- data_frame(subset.type = c("GWF", "UDS", "Both"),
#                                data = list(autocracies.gwf,
#                                            autocracies.uds,
#                                            autocracies))

# Just use the combination
autocracies.list <- data_frame(subset.type = c("Both"),
                               data = list(autocracies))

#+ model_calculation_freq
models.raw.freq <- autocracies.list %>%
  # Run all the models
  mutate(lna.AFI = data %>% map(lna.AFI),
         lna.AGI = data %>% map(lna.AGI),
         lna.AHI = data %>% map(lna.AHI),
         lna.BFI = data %>% map(lna.BFI),
         lna.BGI = data %>% map(lna.BGI),
         lna.BHI = data %>% map(lna.BHI),
         lna.CFI = data %>% map(lna.CFI),
         lna.CGI = data %>% map(lna.CGI),
         lna.CHI = data %>% map(lna.CHI),
         lna.DFI = data %>% map(lna.DFI),
         lna.DGI = data %>% map(lna.DGI),
         lna.DHI = data %>% map(lna.DHI),
         lna.EFI = data %>% map(lna.EFI),
         lna.EGI = data %>% map(lna.EGI),
         lna.EHI = data %>% map(lna.EHI),
         lna.JFI = data %>% map(lna.JFI),
         lna.JGI = data %>% map(lna.JGI),
         lna.JHI = data %>% map(lna.JHI))

models.freq <- models.raw.freq %>%
  gather(model.name, model, -subset.type, -data) %>%
  mutate(glance = model %>% map(broom::glance),
         tidy = model %>% map(broom::tidy, conf.int=TRUE),
         augment = model %>% map(broom::augment))

#+ results="asis"
stargazer(models.freq$model, type="html", omit="\\.factor",
          column.labels=c("lna.AFI", "lna.AGI", "lna.AHI",
                          "lna.BFI", "lna.BGI", "lna.BHI",
                          "lna.CFI", "lna.CGI", "lna.CHI",
                          "lna.DFI", "lna.DGI", "lna.DHI",
                          "lna.EFI", "lna.EGI", "lna.EHI",
                          "lna.JFI", "lna.JGI", "lna.JHI"))

#+ model_calculation_bayes
raw.bayes.rds <- file.path(PROJHOME, "Data", "data_processed",
                           "models_raw_bayes.rds")

if (file.exists(raw.bayes.rds)) {
  models.raw.bayes <- readRDS(raw.bayes.rds)
} else {
  models.raw.bayes <- autocracies.list %>%
    # Run all the models
    mutate(lna.AFI.b = data %>% map(lna.AFI.b),
           lna.AGI.b = data %>% map(lna.AGI.b),
           lna.AHI.b = data %>% map(lna.AHI.b),
           lna.BFI.b = data %>% map(lna.BFI.b),
           lna.BGI.b = data %>% map(lna.BGI.b),
           lna.BHI.b = data %>% map(lna.BHI.b),
           lna.CFI.b = data %>% map(lna.CFI.b),
           lna.CGI.b = data %>% map(lna.CGI.b),
           lna.CHI.b = data %>% map(lna.CHI.b),
           lna.DFI.b = data %>% map(lna.DFI.b),
           lna.DGI.b = data %>% map(lna.DGI.b),
           lna.DHI.b = data %>% map(lna.DHI.b),
           lna.EFI.b = data %>% map(lna.EFI.b),
           lna.EGI.b = data %>% map(lna.EGI.b),
           lna.EHI.b = data %>% map(lna.EHI.b),
           lna.JFI.b = data %>% map(lna.JFI.b),
           lna.JGI.b = data %>% map(lna.JGI.b),
           lna.JHI.b = data %>% map(lna.JHI.b))
  
  saveRDS(models.raw.bayes, file=raw.bayes.rds)
}

#+ gather_models_bayes
models.bayes <- models.raw.bayes %>%
  gather(model.name, model, -subset.type, -data) %>%
  mutate(glance = model %>% map(broom::glance),
         tidy = model %>% map(broom::tidy, intervals=TRUE, prob=0.95),
         augment = model %>% map(broom::augment),
         output = model %>% map(bayesgazer))

#' ## Bayesian results
#' 
#' - Basic model = everything
#' - Alternate model = just government stability and internal political
#'   stability, just coups and protests externally (so those events don't get
#'   double counted in general ICRG stability)
#'
#' ### Coefficient plot
models.to.keep <- c("lna.JGI.b", "lna.EHI.b")

plot.data <- models.bayes %>%
  select(model.name, tidy) %>%
  unnest(tidy) %>%
  filter(!str_detect(term, "year\\.num")) %>%
  filter(term != "(Intercept)") %>%
  filter(model.name %in% models.to.keep) %>%
  left_join(coef.names, by="term") %>%
  mutate(model.name.clean = factor(model.name, levels=models.to.keep,
                                   labels=c("Basic model", "Alternate model"),
                                   ordered=TRUE))

plot.coefs <- ggplot(plot.data,
                     aes(x=estimate, y=term.clean.rev,
                         xmin=lower, xmax=upper, colour=model.name.clean)) +
  geom_vline(xintercept=0) +
  geom_pointrangeh(position=position_dodge(width=0.5), size=0.5) +
  scale_colour_manual(values=ath.palette("contention"), name=NULL) +
  coord_cartesian(xlim=c(-0.5, 1)) +
  labs(x="Posterior median change in CSRE", y=NULL) +
  theme_ath() + 
  facet_wrap(~ category, ncol=1, scales="free")

grid::grid.newpage()
grid::grid.draw(correct_panel_size(plot.coefs))

fig.save.cairo(correct_panel_size(plot.coefs), filename="1-coefs-bayes",
               width=6, height=4)

#' ### Results for basic model (`lna.JGI.b`)
#' 
#+ results="asis"
caption <- "Results from basic Bayesian generalized linear regression model {#tbl:results-basic}"
tbl.full.model <- pandoc.table.return(filter(models.bayes, 
                                             model.name == "lna.JGI.b")$output[[1]],
                                      keep.line.breaks=TRUE,
                                      justify="lcccccc", caption=caption)
cat(tbl.full.model)
cat(tbl.full.model, file=file.path(PROJHOME, "Output", "tables", 
                                   "1-results-basic.md"))

#' ### Results for alternate model (`lna.EHI.b`)
#' 
#' The N is lower here because of missing data in the years in office, years
#' since competitive election, and opposition vote share variables.
#' 
#+ results="asis"
caption <- "Results from alternate Bayesian generalized linear regression model {#tbl:results-alternate}"
tbl.alt.model <- pandoc.table.return(filter(models.bayes, 
                                            model.name == "lna.EHI.b")$output[[1]],
                                     keep.line.breaks=TRUE,
                                     justify="lcccccc", caption=caption)
cat(tbl.alt.model)
cat(tbl.alt.model, file=file.path(PROJHOME, "Output", "tables", 
                                  "1-results-alternate.md"))


#' # Criticize the model
model.to.check <- filter(models.bayes,
                         model.name == "lna.JGI.b")$model[[1]]
# launch_shinystan(model.to.check)

#' Diagnostics based on model `lna.JGI.b`.
#' 
#' How well does the posterior predictive distribution fit the observed
#' outcome?
#' 
#' Not great, but not that terrible either. The biggest issue is the
#' bimodalness of the distribution, with peaks at 0 and 3/4ish. The posterior
#' distribution just averages out those peaks. I can live with it, though.
#' 
pp_check(model.to.check, check="dist", overlay=TRUE, nreps = 5) + 
  theme_ath()

#' What about chain convergence? These should look like tops if everything 
#' converges, with no observations at 0 in the mean Metropolis–Hastings
#' acceptance
#' 
rstan::stan_diag(model.to.check, information="divergence")


#' # Analyze effects of predictors
#' 
#' ## Internal
#' 
#' ### Internal political stability
#' 
#' ICRG political stability by itself
#' 
model.to.use <- filter(models.bayes,
       model.name == "lna.EHI.b")$model[[1]]

new.data.int.risk <- model.to.use$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.pol.risk.internal.nostab.scaled)) %>%
  right_join(expand.grid(icrg.pol.risk.internal.nostab.scaled =
                           seq(0, 100, by=1),
                         index = 1),
             by="index") %>%
  select(-index)

#' This isn't 100% accurate because I should be using `posterior_predict`
#' instead of `predict.stanreg` since I'm using MCMC, but `posterior_predict`
#' doesn't automatically create standard errors and I can't figure out how to
#' calculate them manually.
#' 
plot.predict.int.risk <- augment(model.to.use,
                                 newdata=new.data.int.risk) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(stability = "ICRG internal stability (scaled)")

# What should happen, in theory:
# new.post.pred <- posterior_predict(model.to.use, new.data.int.stability,
#                                    seed=my.seed)
# 
# plot.predict.int.stability <- new.data.int.stability %>%
#   mutate(pred = apply(new.post.pred, 2, mean),
#          pred.lower = apply(new.post.pred, 2,
#                             function(x) quantile(x, probs = c(0.025))),
#          pred.upper = apply(new.post.pred, 2,
#                             function(x) quantile(x, probs = c(0.975))))

# Have to add a single-valued aes so that something shows up in the legend so
# that the figure can be aligned in a row with plot.int.stability.pred
plot.int.risk.pred <- ggplot(plot.predict.int.risk,
                             aes(x=icrg.pol.risk.internal.nostab.scaled, y=pred,
                                 colour=stability, fill=stability)) +
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
              alpha=0.3, colour=NA, fill="#0074D9") +
  geom_line(size=1) +
  coord_cartesian(xlim=c(15, 70)) +
  scale_colour_manual(values="#0074D9", name=NULL) +
  scale_fill_manual(values="#0074D9", name=NULL, guide=FALSE) +
  labs(x="Internal political stability", y="Predicted CSRE in following year") +
  theme_ath()
plot.int.risk.pred

fig.save.cairo(plot.int.risk.pred, filename="1-int-risk-pred",
               width=5, height=3)

#' ### Government stability
#' 
#' ICRG stability + years in office
#' 
model.to.use <- filter(models.bayes,
                       model.name == "lna.EHI.b")$model[[1]]

new.data.int.stability <- model.to.use$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.stability, yrsoffc)) %>%
  right_join(expand.grid(icrg.stability =
                           seq(0, 12, by=0.1),
                         yrsoffc = c(2, 30),
                         index = 1),
             by="index") %>%
  select(-index)

plot.predict.int.stability <- augment(model.to.use,
                                      newdata=new.data.int.stability) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(yrsoffc = factor(yrsoffc, levels=c(2, 30),
                          labels=paste(c(2, 30), "years in office")))

plot.int.stability.pred <- ggplot(plot.predict.int.stability,
                                  aes(x=icrg.stability, y=pred,
                                      fill=yrsoffc, colour=yrsoffc)) +
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
              alpha=0.3, colour=NA) +
  geom_line(size=1) +
  coord_cartesian(xlim=c(3, 12)) +
  scale_x_continuous(breaks=seq(3, 12, 3)) +
  scale_colour_manual(values=ath.palette("contention"), name=NULL) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL, guide=FALSE) +
  labs(x="Government stability", y="Predicted CSRE in following year") +
  theme_ath()
plot.int.stability.pred

fig.save.cairo(plot.int.stability.pred, filename="1-int-stability-pred",
               width=5, height=3)

#' ### Both together
plot.int.pred <- arrangeGrob(plot.int.risk.pred, 
                             grob.blank,
                             plot.int.stability.pred + ylab(NULL), 
                             nrow=1, widths=c(0.475, 0.05, 0.475))
grid::grid.newpage()
grid::grid.draw(plot.int.pred)

fig.save.cairo(plot.int.pred, filename="1-int-pred",
               width=5, height=2.5)


#' ## External
#' 
#' ### General risk of neighbors + coups
model.to.use <- filter(models.bayes,
                       model.name == "lna.EHI.b")$model[[1]]

new.data.ext.risk.coups <- model.to.use$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.pol.risk_wt, coups.activity.bin_sum_nb)) %>%
  right_join(expand.grid(icrg.pol.risk_wt =
                           seq(0, 100, by=1),
                         coups.activity.bin_sum_nb = c(0, 2),
                         index = 1),
             by="index") %>%
  select(-index)

plot.predict.ext.risk.coups <- augment(model.to.use,
                                       newdata=new.data.ext.risk.coups) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(coups.activity.bin_sum_nb = factor(coups.activity.bin_sum_nb,
                                            levels=c(0, 2),
                                            labels=c("No coups in neighboring countries",
                                                     "Coup activity in neigboring countries"),
                                            ordered=TRUE))

plot.ext.risk.coups <- ggplot(plot.predict.ext.risk.coups,
                              aes(x=icrg.pol.risk_wt, y=pred,
                                  fill=coups.activity.bin_sum_nb,
                                  colour=coups.activity.bin_sum_nb)) +
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
              alpha=0.3, colour=NA) +
  geom_line(size=1) +
  scale_colour_manual(values=ath.palette("contention"), name=NULL) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL, guide=FALSE) +
  labs(x="Political stability of neighbors", y="Predicted CSRE in following year") +
  theme_ath()
plot.ext.risk.coups

fig.save.cairo(plot.ext.risk.coups, filename="1-ext-risk-coups",
               width=5, height=3)

#' ### Relative protests, violent/nonviolent
model.to.use <- filter(models.bayes,
                       model.name == "lna.EHI.b")$model[[1]]

new.data.ext.protests.violent <- model.to.use$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, protests.violent.std_wt)) %>%
  right_join(expand.grid(protests.violent.std_wt =
                           seq(1, 5, by=0.1),
                         index = 1),
             by="index") %>%
  select(-index)

new.data.ext.protests.nonviolent <- model.to.use$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, protests.nonviolent.std_wt)) %>%
  right_join(expand.grid(protests.nonviolent.std_wt =
                           seq(1, 5, by=0.1),
                         index = 1),
             by="index") %>%
  select(-index)

plot.predict.ext.protests.violent <- augment(model.to.use,
                                       newdata=new.data.ext.protests.violent) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  rename(protest.std = protests.violent.std_wt) %>%
  mutate(protest.type = "Violent")

plot.predict.ext.protests.nonviolent <- augment(model.to.use,
                                     newdata=new.data.ext.protests.nonviolent) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  rename(protest.std = protests.nonviolent.std_wt) %>%
  mutate(protest.type = "Nonviolent")

plot.predict.ext.protests <- bind_rows(plot.predict.ext.protests.violent,
                                       plot.predict.ext.protests.nonviolent)

plot.ext.protests <- ggplot(plot.predict.ext.protests,
                            aes(x=protest.std, y=pred,
                                fill=protest.type, colour=protest.type)) +
  geom_vline(xintercept=3, linetype="dotted", colour="grey50") +
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
              alpha=0.3, colour=NA) +
  geom_line(size=1) +
  scale_colour_manual(values=ath.palette("palette1"), name=NULL) +
  scale_fill_manual(values=ath.palette("palette1"), name=NULL, guide=FALSE) +
  scale_x_continuous(labels=c("Less\n(1)", "", "Normal\n(3)", "", 
                              "More\n(5)")) +
  labs(x="Relative protest activity", y="Predicted CSRE in following year") +
  theme_ath()
plot.ext.protests

fig.save.cairo(plot.ext.protests, filename="1-ext-protests",
               width=5, height=3)


#' ### Both together
plot.ext.pred <- arrangeGrob(plot.ext.risk.coups + guides(col=guide_legend(nrow=2)), 
                             grob.blank,
                             plot.ext.protests + ylab(NULL), 
                             nrow=1, widths=c(0.475, 0.05, 0.475))
grid::grid.newpage()
grid::grid.draw(plot.ext.pred)

fig.save.cairo(plot.ext.pred, filename="1-ext-pred",
               width=5, height=2.75)


#' ## Reputation
#' 
#' ### Relative shaming
model.to.use <- filter(models.bayes,
                       model.name == "lna.EHI.b")$model[[1]]

new.data.shaming.1 <- model.to.use$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, shaming.states.std)) %>%
  right_join(expand.grid(shaming.states.std =
                           seq(1, 5, by=0.1),
                         index = 1),
             by="index") %>%
  select(-index)

plot.predict.shaming.1 <- augment(model.to.use,
                                  newdata=new.data.shaming.1) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(shaming.type = "With extra internal variables")

model.to.use <- filter(models.bayes,
                       model.name == "lna.JFI.b")$model[[1]]

new.data.shaming.2 <- model.to.use$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, shaming.states.std)) %>%
  right_join(expand.grid(shaming.states.std =
                           seq(1, 5, by=0.1),
                         index = 1),
             by="index") %>%
  select(-index)

plot.predict.shaming.2 <- augment(model.to.use,
                                  newdata=new.data.shaming.2) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(shaming.type = "Without extra internal variables")

plot.predict.shaming <- bind_rows(plot.predict.shaming.1,
                                  plot.predict.shaming.2)

plot.shaming <- ggplot(plot.predict.shaming,
                                 aes(x=shaming.states.std, y=pred,
                                     fill=shaming.type,
                                     colour=shaming.type)) +
  geom_vline(xintercept=3, linetype="dotted", colour="grey50") +
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
              alpha=0.3, colour=NA) +
  geom_line(size=1) +
  scale_colour_manual(values=ath.palette("leaders"), name=NULL) +
  scale_fill_manual(values=ath.palette("leaders"), name=NULL, guide=FALSE) +
  scale_x_continuous(labels=c("Less than normal\n(1)", "", "Normal\n(3)", "", 
                              "More than normal\n(5)")) +
  labs(x="Relative state-based shaming", y="Predicted CSRE in following year") +
  theme_ath()
plot.shaming

fig.save.cairo(plot.shaming, filename="1-shaming-pred",
               width=5, height=3)


#' # LNA-based case selection
#' 
#' ## Issues with list-wise deletion
#' 
#' Missing data makes perfect LNA selection based on predicted values 
#' tricky—there are lots of instances where there's not enough data to get lots
#' of predicted values. `lna.JGI.b` is the most complete model, but even then, 
#' it omits lots of observations, mostly because ICRG doesn't cover everything.
#' Someday I need to figure out how to correctly impute data for Bayesian
#' models (i.e. not use Amelia).
#' 
model.for.case.selection <- filter(models.bayes,
                                   model.name == "lna.JGI.b")$model[[1]]

rows.used <- as.numeric(rownames(model.for.case.selection$model))
vars.used <- Filter(function(x) !str_detect(x, "as\\.factor"),
                    colnames(model.for.case.selection$model))

autocracies.modeled <- autocracies %>%
  mutate(rowname = row_number(),
         in.model = rowname %in% rows.used) %>%
  filter(year.num > 1994) %>%
  select(rowname, in.model, cowcode, country, year.num, one_of(vars.used))

datatable(autocracies.modeled)

#' ## Predicted vs. actual CSRE
cases <- data_frame(cowcode = c(710, 651, 365, 663, 775, 705),
                    country.name = countrycode(cowcode, "cown", "country.name"),
                    colour = ath.palette("palette1", n=6),
                    fill = ath.palette("palette1", n=6),
                    linetype = 1, alpha = 1, point.size = 1) %>%
  mutate(country.name = ifelse(cowcode == 365, "Russia", country.name))

plot.data.sna.selection.b <- model.for.case.selection %>%
  augment() %>%
  mutate(post.pred.fit = apply(posterior_predict(model.for.case.selection, 
                                                 seed=my.seed), 2, median)) %>%
  mutate(.rownames = as.numeric(.rownames)) %>%
  left_join(select(autocracies.modeled, rowname, cowcode, country),
            by=c(".rownames" = "rowname")) %>%
  left_join(cases, by="cowcode") %>%
  mutate(country.name = ifelse(is.na(country.name), "Other", country.name),
         colour = ifelse(is.na(colour), "grey70", colour),
         fill = ifelse(is.na(fill), NA, fill),
         linetype = ifelse(is.na(linetype), 0, linetype),
         alpha = ifelse(is.na(alpha), 0.25, alpha),
         point.size = ifelse(is.na(point.size), 0.55, point.size)) %>%
  mutate(country.name = factor(country.name, levels=c(cases$country.name, "Other"),
                               ordered=TRUE),
         colour = factor(colour, levels=c(cases$colour, "grey70"),
                         ordered=TRUE),
         fill = factor(fill, levels=c(cases$fill, NA),
                         ordered=TRUE))

plot.sna.selection.b <- ggplot(plot.data.sna.selection.b,
                               aes(x=post.pred.fit, y=cs_env_sum.lead,
                                   colour=colour)) +
  geom_segment(x=-6, xend=6, y=-6, yend=6, colour="grey75", size=0.5) +
  geom_point(aes(alpha=alpha, size=point.size)) +
  # stat_ellipse(aes(linetype=linetype), type="norm", size=0.5) +
  stat_chull(aes(linetype=linetype, fill=fill), alpha=0.1, show.legend=FALSE) +
  stat_smooth(aes(linetype=linetype, colour=colour), method="lm", se=FALSE) +
  scale_color_identity(guide="legend", labels=c(cases$country.name, "Other"),
                       name=NULL) +
  scale_fill_identity() +
  scale_size_identity() +
  scale_alpha_identity() +
  scale_linetype_identity() +
  labs(x="Predicted CSRE", y="Actual CSRE") +
  coord_cartesian(xlim=c(-4, 4), ylim=c(-6, 6)) +
  theme_ath()
plot.sna.selection.b

#' ## Data for case studies
#' 
#' Better to look at this with the timelines though.
#' 
final.case.studies <- autocracies.modeled %>%
  filter(cowcode %in% cases$cowcode) %>%
  group_by(country) %>%
  summarise_each(funs(mean = mean(., na.rm=TRUE)), one_of(vars.used))

datatable(final.case.studies)
