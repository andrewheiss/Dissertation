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
library(ggplot2)
library(Cairo)

full.data <- readRDS(file.path(PROJHOME, "Data","data_processed",
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
#' - `yrsoffc`: 
#' - `years.since.comp`: 
#' - `opp1vote`: 
#' 
#' External stability:
#' 
#' - `neighbor.stability.XXX`
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
#' ## Internal factors only
model.int.simple <- lm(v2csreprss ~ icrg.stability + e_polity2,
                       data=full.data)
summary(model.int.simple)

model.int.full <- lm(v2csreprss ~ icrg.stability +
                       yrsoffc + years.since.comp + opp1vote +
                       e_polity2,
                     data=full.data)
summary(model.int.full)

model.int.all <- lm(v2csreprss ~ icrg.stability +
                      yrsoffc + years.since.comp + opp1vote +
                      e_polity2 +
                      physint + gdpcap.log + population.log +
                      oda.log + countngo + globalization,
                    data=full.data)
summary(model.int.all)


#' ## External factors only
model.ext.simple.min <- lm(v2csreprss ~ neighbor.stability.min + e_polity2,
                           data=full.data)
summary(model.ext.simple.min)

model.ext.simple.med <- lm(v2csreprss ~ neighbor.stability.median + e_polity2,
                           data=full.data)
summary(model.ext.simple.med)

model.ext.simple.mean <- lm(v2csreprss ~ neighbor.stability.mean + e_polity2,
                            data=full.data)
summary(model.ext.simple.mean)

model.ext.min.all <- lm(v2csreprss ~ neighbor.stability.min + e_polity2 +
                          gdpcap.log + population.log +
                          oda.log + countngo + globalization,
                        data=full.data)
summary(model.ext.min.all)


#' ## International reputation only



#' # Bayesian tinkering
#' 
#' See:
#' 
#' - https://cran.r-project.org/web/packages/rstanarm/vignettes/lm.html
#' - https://cran.r-project.org/web/packages/rstanarm/vignettes/continuous.html

# library(rstanarm)
# options(mc.cores = parallel::detectCores())  # Use all possible cores
# 
# model.simple.b <- stan_glm(v2csreprss ~ icrg.stability + e_polity2,
#                           data=full.data, family=gaussian(),
#                           prior=cauchy(), prior_intercept=cauchy(),
#                           seed=my.seed)
# print(model.simple.b, digits=2)
# plot(model.simple.b, pars=c("icrg.stability", "e_polity2"))
# pp_check(model.simple.b, check="distributions", overlay=FALSE, nreps=5)
