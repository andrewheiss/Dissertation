#' ---
#' title: "Determinants of international civil society restrictions"
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
library(purrr)
library(broom)
library(ggplot2)
library(Cairo)
library(stargazer)

full.data <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.rds"))

my.seed <- 1234
set.seed(my.seed)


#' # Variables
#' 
#' ## Dependent variables
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
#'
#' ## Explanatory variables
#' 
#' Internal stability:
#' 
#' - `icrg.stability`: 
#' - `icrg.pol.risk.internal.scaled`: Political risk rating---external measures
#'   removed and index rescaled to 0-100 scale
#' - `yrsoffc`: 
#' - `years.since.comp`: 
#' - `opp1vote`: 
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


#' # Models
#' 
#' ## Internal factors
#' 
#' ### Just internal political risk
model.int.simple <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ icrg.pol.risk.internal.scaled,
           data=.))

#+ results='asis'
stargazer(model.int.simple, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))

#' ### All internal variables
model.int.full <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ icrg.pol.risk.internal.scaled +
             yrsoffc + years.since.comp + opp1vote + as.factor(year.num),
           data=.))

#+ results='asis'
stargazer(model.int.full, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.full),
          omit="factor\\(year",
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### All internal variables + controls
model.int.all <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ icrg.pol.risk.internal.scaled +
             yrsoffc + years.since.comp + opp1vote +
             physint + gdpcap.log + population.log +
             oda.log + countngo + globalization + as.factor(year.num),
           data=.))

#+ results='asis'
stargazer(model.int.all, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))


#' ### All models (to LaTeX)
all.models <- c(model.int.simple, model.int.full, model.int.all)

coef.labs <- c("Internal political risk (ICRG)", "Years executive in office", 
               "Years since competitive election", "Opposition vote share",
               "Physical integrity rights", "GDP per capita (log)", 
               "Population (log)", "Foreign aid (log)", 
               "Number of INGO members", "Globalization")
extra.lines <- list(c("Year fixed effects",
                      c(rep("No", 2), rep("Yes", 4))))

capture.output({
  stargazer(all.models, type="latex", 
            out=file.path(PROJHOME, "Output", "tables", "1-internal-models-all.tex"),
            covariate.labels=coef.labs,
            dep.var.caption="Civil society regulatory environment (CSRE) in following year",
            dep.var.labels.include=FALSE,
            column.labels=names(all.models),
            omit="factor\\(year", no.space=TRUE,
            add.lines=extra.lines)
}, file="/dev/null")


#' ## External factors
#' 
#' ### Just neighbors
model.ext.neighbors <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
             neighbor.coups.activity.bin,
           data=.))

#+ results='asis'
stargazer(model.ext.neighbors, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))

#' ### Neighbors and subregion
model.ext.neighbors.subregion <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
             icrg.pol.risk.subregional +
             neighbor.coups.activity.bin + 
             coups.activity.subregional,
           data=.))

#+ results='asis'
stargazer(model.ext.neighbors.subregion, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))

#' ### Neighbors and region
model.ext.neighbors.region <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
             icrg.pol.risk.regional +
             neighbor.coups.activity.bin + 
             coups.activity.regional,
           data=.))

#+ results='asis'
stargazer(model.ext.neighbors.region, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))


#' ### Neighbors and subregion + controls
model.ext.neighbors.subregion.ctrl <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
             icrg.pol.risk.subregional +
             neighbor.coups.activity.bin + 
             coups.activity.subregional + 
             gdpcap.log + population.log + oda.log + 
             countngo + globalization + as.factor(year.num),
           data=.))

#+ results='asis'
stargazer(model.ext.neighbors.subregion.ctrl, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))


#' ### All models (to LaTeX)
all.models <- c(model.ext.neighbors, model.ext.neighbors.region, 
                model.ext.neighbors.subregion,
                model.ext.neighbors.subregion.ctrl)

coef.labs <- c("Lowest political risk in all neighboring countries", 
               "Average political risk in subregion", 
               "Average political risk in region",
               "Coup activity in neighboring countries (binary)",
               "Coup activity in subregion (binary)",
               "Coup activity in region (binary)",
               "GDP per capita (log)", 
               "Population (log)", "Foreign aid (log)", 
               "Number of INGO members", "Globalization")
extra.lines <- list(c("Year fixed effects",
                      c(rep("No", 6), rep("Yes", 2))))

capture.output({
  stargazer(all.models, type="latex", 
            out=file.path(PROJHOME, "Output", "tables", "1-external-models-all.tex"),
            covariate.labels=coef.labs,
            dep.var.caption="Civil society regulatory environment (CSRE) in following year",
            dep.var.labels.include=FALSE,
            column.labels=names(all.models),
            omit="factor\\(year", no.space=TRUE,
            add.lines=extra.lines)
}, file="/dev/null")


#' ## International reputation



#' ## All three at once?
model.everything <- lm(cs_env_sum.lead ~ icrg.pol.risk.internal.scaled +
                         yrsoffc + years.since.comp + opp1vote +
                         neighbor.pol.risk.min +
                         e_polity2 +
                         physint + gdpcap.log + population.log +
                         oda.log + countngo + globalization,
                       data=full.data)
summary(model.everything)


#' # Bayesian tinkering
#' 
#' See:
#' 
#' - https://cran.r-project.org/web/packages/rstanarm/vignettes/lm.html
#' - https://cran.r-project.org/web/packages/rstanarm/vignettes/continuous.html

# library(rstanarm)
# options(mc.cores = parallel::detectCores())  # Use all possible cores
# 
# model.simple.b <- stan_glm(cs_env_sum.lead ~ icrg.stability + e_polity2,
#                           data=full.data, family=gaussian(),
#                           prior=cauchy(), prior_intercept=cauchy(),
#                           seed=my.seed)
# print(model.simple.b, digits=2)
# plot(model.simple.b, pars=c("icrg.stability", "e_polity2"))
# pp_check(model.simple.b, check="distributions", overlay=FALSE, nreps=5)
