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
#+ message=FALSE
library(magrittr)
library(dplyr)
library(purrr)
library(broom)
library(feather)
library(ggplot2)
library(scales)
library(gridExtra)
library(Cairo)
library(stargazer)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

full.data <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.feather"))

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


#' # Preliminary large-N analysis (LNA)
#' 
#' There are lots of different ways to define autocracy, including Polity, UDS,
#' or Geddes et al.'s manual classification. Here, I follow Geddes et al., but
#' slightly expanded—for the sake of completeness of data (and in case I run
#' survival models in the future), I include a country in the data if it has
#' ever been an autocracy since 1991. This means countries that democratize or
#' backslide are included.
#' 
autocracies <- filter(full.data, gwf.ever.autocracy) %>%
  mutate(case.study = cowcode %in% c(710, 651, 365)) %>%
  # Make these variables more interpretable unit-wise
  mutate_each(funs(. * 100), dplyr::contains("pct")) %>%
  mutate(.rownames = rownames(.))
  

#' Note on time frame of variables: Not all variables overlap perfectly with
#' V-Dem. Models that include any of the following variables will be inherently
#' limited to the corresponding years (since non-overlapping years are
#' dropped):
#' 
#' - ICRG: 1991-2014
#' - ICEWS: 1995-2015
#' - ICEWS EOIs: 2000-2014
#' 

#' All models
lna.all.simple <- lm(cs_env_sum.lead ~ 
                       icrg.stability + icrg.internal +
                       icrg.pol.risk_wt +
                       shaming.states.std +
                       shaming.ingos.std + 
                       as.factor(year.num),
                     data=autocracies)

lna.all.full <- lm(cs_env_sum.lead ~ 
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
                   data=autocracies)


#' ## Internal factors
#' 
#' ### Models
#' 
lna.internal.simple <- lm(cs_env_sum.lead ~ 
                            # Internal (low is bad; high is good)
                            icrg.stability + icrg.internal + 
                            as.factor(year.num),
                          data=autocracies)

lna.internal.full <- lm(cs_env_sum.lead ~ 
                          icrg.stability + icrg.internal + 
                          yrsoffc + years.since.comp + opp1vote +
                          as.factor(year.num),
                        data=autocracies)

lna.internal.alt <- lm(cs_env_sum.lead ~ 
                         icrg.pol.risk.internal.scaled + 
                         yrsoffc + years.since.comp + opp1vote +
                         as.factor(year.num),
                       data=autocracies)

#+ results='asis'
stargazer(lna.internal.simple, lna.internal.full, 
          lna.all.simple, lna.all.full,
          type="html", 
          dep.var.caption="CSRE in following year",
          dep.var.labels.include=FALSE, no.space=TRUE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 4))))

#' ### Coefficient plot
#' 
vars.included <- c("icrg.stability", "icrg.internal", "yrsoffc", 
                   "years.since.comp", "opp1vote")

coef.plot.int <- fig.coef(list("Simple" = lna.internal.simple,
                               "All internal factors" = lna.internal.full,
                               "All factors (excerpt)" = lna.all.full), 
                          xlab="Civil society regulatory environment (CSRE)",
                          vars.included=vars.included)
coef.plot.int

fig.save.cairo(coef.plot.int, filename="1-coefs-lna-int",
               width=6, height=3)


#' ## External factors
#' 
#' ### Models
#' 
lna.external.simple <- lm(cs_env_sum.lead ~ 
                             icrg.pol.risk_wt +
                            as.factor(year.num),
                          data=autocracies)

lna.external.full <- lm(cs_env_sum.lead ~ 
                          # TODO: Consistent wt vs nb variables? Or 
                          # justification for them being different?
                          icrg.pol.risk_wt + 
                          any.crisis_pct_mean_nb +
                          coups.activity.bin_sum_nb +
                          protests.violent.std_wt +
                          protests.nonviolent.std_wt +
                          as.factor(year.num),
                        data=autocracies)

#+ results='asis'
stargazer(lna.external.simple, lna.external.full,
          lna.all.simple, lna.all.full,
          type="html", 
          dep.var.caption="CSRE in following year",
          dep.var.labels.include=FALSE, no.space=TRUE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 4))))


#' ### Coefficient plot
#' 
vars.included <- c("icrg.pol.risk_wt", "any.crisis_pct_mean_nb", 
                   "coups.activity.bin_sum_nb", "protests.violent.std_wt", 
                   "protests.nonviolent.std_wt")

coef.plot.ext <- fig.coef(list("Simple" = lna.external.simple,
                               "All external factors" = lna.external.full,
                               "All factors (excerpt)" = lna.all.full), 
                          xlab="Civil society regulatory environment (CSRE)",
                          vars.included=vars.included)
coef.plot.ext

fig.save.cairo(coef.plot.ext, filename="1-coefs-lna-ext",
               width=6, height=3)


#' ## Shaming factors
#' 
#' ### Models
#' 
lna.shame.simple <- lm(cs_env_sum.lead ~
                         # TODO: Deal with proper normalized or weighted values
                         # shaming.states.pct.govt +
                         # shaming.ingos.pct.ingo +
                         shaming.states.std +
                         shaming.ingos.std +
                         as.factor(year.num),
                       data=autocracies)

#+ results='asis'
stargazer(lna.shame.simple, lna.all.simple, lna.all.full,
          type="html", 
          dep.var.caption="CSRE in following year",
          dep.var.labels.include=FALSE, no.space=TRUE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 3))))

#' ### Coefficient plot
#' 
vars.included <- c("shaming.states.std", "shaming.ingos.std")
coef.plot.shame <- fig.coef(list("All shaming factors" = lna.shame.simple,
                                 "All factors (excerpt)" = lna.all.full), 
                            xlab="Civil society regulatory environment (CSRE)",
                            vars.included=vars.included)
coef.plot.shame

fig.save.cairo(coef.plot.shame, filename="1-coefs-lna-shame",
               width=6, height=3)


#' ## All factors at once
#' 
#' ### Models
#' 
#' Actual full models run previously so they can be compared to smaller models.
#' 
#' Here I show the results of all models side-by-side.
#' 
var.labs <-  c(
  "Internal stability (ICRG)",
  "Internal conflict (ICRG)",
  "Years executive in office",
  "Years since competitive election",
  "Opposition vote share",
  "Average political risk in neighboring countries (ICRG)",
  "Government-based shaming reports (% of all events)",
  "INGO-based shaming reports (% of all events)",
  "Average time in crisis in neighboring countries (% of a year)",
  "Coup activity in neighboring countries (binary)",
  "Violent protests, weighted by distance (relative within country)",
  "Nonviolent protests, weighted by distance (relative within country)"
)

#+ results='asis'
stargazer(lna.internal.simple, lna.internal.full,
          lna.external.simple, lna.external.full,
          lna.shame.simple, lna.all.simple, lna.all.full,
          type="html",
          dep.var.caption="CSRE in following year",
          dep.var.labels.include=FALSE, no.space=TRUE,
          # covariate.labels=var.labs,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 7))),
          out=file.path("~/Desktop/all.html")
          )

#' ### Coefficient plot
#' 
coef.plot.all <- fig.coef(list("Simple" = lna.all.simple,
                               "Full" = lna.all.full), 
                          xlab="Civil society regulatory environment (CSRE)")
coef.plot.all

fig.save.cairo(coef.plot.all, filename="1-coefs-lna-all",
               width=6, height=4.5)

# ggplot(autocracies, aes(x=shaming.ingos.pct.all, y=cs_env_sum.lead)) +
#   geom_point(aes(text=paste(country_name, year.num), colour=case.study)) +
#   geom_smooth() #+ facet_wrap(~ case.study)
# plotly::ggplotly()


#' ## Marginal effects
#' 
#' ### CSRE across range of ICRG scores
new.data.int.stability <- lna.internal.full$model %>%
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

new.data.int.conflict <- lna.internal.full$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.internal, yrsoffc)) %>%
  right_join(expand.grid(icrg.internal =
                           seq(0, 12, by=0.1), 
                         yrsoffc = c(2, 30),
                         index = 1),
             by="index") %>%
  select(-index)

plot.predict.stability <- lna.internal.full %>%
  augment(newdata=new.data.int.stability) %>%
  mutate(val.to.plot = icrg.stability,
         var.name = "Internal stability")

plot.predict.conflict <- lna.internal.full %>%
  augment(newdata=new.data.int.conflict) %>%
  mutate(val.to.plot = icrg.internal,
         var.name = "Internal conflict")

plot.predict <- bind_rows(plot.predict.stability, plot.predict.conflict) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(yrsoffc = factor(yrsoffc, levels=c(2, 30), 
                          labels=paste(c(2, 30), "years in office")))

plot.icrg.int.pred <- ggplot(plot.predict, 
                             aes(x=val.to.plot, y=pred, 
                                 fill=var.name, colour=var.name)) +
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper),
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) +
  labs(x="ICRG score", y="Predicted CSRE in following year") +
  scale_colour_manual(values=c(col.auth, col.dem), name=NULL) +
  scale_fill_manual(values=c(col.auth, col.dem), name=NULL, guide=FALSE) +
  coord_cartesian(ylim=c(-4, 4)) +
  theme_ath() + 
  facet_wrap(~ yrsoffc)
plot.icrg.int.pred

fig.save.cairo(plot.icrg.int.pred, filename="1-icrg-int-pred",
               width=5, height=3)


#' ### CSRE across years executive has been in office
new.data.yrsoffc <- lna.internal.full$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.stability, yrsoffc)) %>%
  right_join(expand.grid(icrg.stability = seq(0, 12, by=0.1),
                         yrsoffc = c(2, 30),
                         index = 1, stringsAsFactors=FALSE),
             by="index") %>%
  select(-index)

plot.predict.yrsoffc <- lna.internal.full %>%
  augment(newdata=new.data.yrsoffc) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         yrsoffc.clean = factor(yrsoffc, labels=paste(unique(yrsoffc),
                                                      "years in office")))

plot.icrg.yrs.int.pred <- ggplot(plot.predict.yrsoffc,
                                 aes(x=icrg.stability,
                                     y=pred, colour=yrsoffc.clean)) +
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=yrsoffc.clean),
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) +
  labs(x="Internal stability (ICRG)", y="Predicted CSRE in following year") +
  scale_colour_manual(values=c("#004259", "#4A0A3D"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#4A0A3D"), name=NULL, guide=FALSE) +
  coord_cartesian(ylim=c(-4, 4)) +
  theme_ath()
plot.icrg.yrs.int.pred

fig.save.cairo(plot.icrg.yrs.int.pred, filename="1-icrg-yrs-int-pred",
               width=5, height=3)
 
#' ### Both predictions together
plot.icrg.int.pred.both <- arrangeGrob(plot.icrg.int.pred,
                                       plot.icrg.yrs.int.pred,
                                       nrow=1)
grid::grid.draw(plot.icrg.int.pred.both)

fig.save.cairo(plot.icrg.int.pred.both, filename="1-icrg-yrs-int-pred-both",
               width=5, height=2.5)

 
#' ### CSRE across neighbor stability and coups in region
new.data.ext <- lna.external.full$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, protests.nonviolent.std_wt,
            icrg.pol.risk_wt)) %>%
  right_join(expand.grid(protests.nonviolent.std_wt = c(1, 2.5, 5),
                         icrg.pol.risk_wt = seq(40, 100, by=1),
                         index=1),
             by="index") %>%
  select(-index)

plot.predict.ext.icrg.protests <- lna.external.full %>%
  augment(newdata=new.data.ext) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         protests = factor(protests.nonviolent.std_wt, levels=c(1, 2.5, 5),
                           labels=c("Much fewer than normal (1)", "Normal (2.5)", 
                                    "Much more than normal (5)"))) %>%
  # Remove confidence intervals for normal levels of protests
  mutate(pred.lower = ifelse(protests.nonviolent.std_wt == 2.5, NA, pred.lower),
         pred.upper = ifelse(protests.nonviolent.std_wt == 2.5, NA, pred.upper))

plot.ext.icrg.nonviolent <- ggplot(plot.predict.ext.icrg.protests, 
                                   aes(x=icrg.pol.risk_wt, y=pred,
                                       colour=protests)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=protests),
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) +
  labs(x="Average political risk in neighboring countries (ICRG)",
       y="Predicted CSRE in following year") + 
  scale_colour_manual(values=c("#004259", "grey50", "#4A0A3D"), name=NULL) +
  scale_fill_manual(values=c("#004259", "grey50" ,"#4A0A3D"), name=NULL, guide=FALSE) +
  theme_ath()
plot.ext.icrg.nonviolent

# fig.save.cairo(plot.subregion.coup.pred, filename="1-icrg-subregion-coup-ext-pred", 
#                width=5, height=2.5)
# 


#' # Nested analysis case selection
#'
#' Use the basic model with minimal variables, in part because of the
#' principles of LNA, and in part because there are so many dropped, incomplete
#' cases when using years in office, opposition vote, and ICEWS EOIs.
#' 
lna.selection.data <- lna.all.simple %>%
  augment() %>%
  left_join(select(autocracies, .rownames, country_name, cowcode), by=".rownames") %>%
  mutate(case.study = cowcode %in% c(710, 651, 365),
         country.plot = ifelse(case.study, country_name, "Other"),
         country.plot = factor(country.plot, levels=c("China", "Egypt", "Russia", "Other"),
                               ordered=TRUE))

plot.lna.selection <- ggplot(lna.selection.data, 
                             aes(x=.fitted, y=cs_env_sum.lead,
                                 colour=country.plot)) +
  geom_segment(x=-6, xend=6, y=-6, yend=6, colour="grey75", size=0.5) + 
  geom_point(aes(alpha=country.plot, size=country.plot)) +
  stat_ellipse(aes(linetype=country.plot), type="norm", size=0.5) +
  scale_linetype_manual(values=c(1, 1, 1, 0), guide=FALSE) +
  scale_color_manual(values=c("#CC3340", "#6B4A3D", "#00A1B0", "grey50"), name=NULL) +
  scale_alpha_manual(values=c(1, 1, 1, 0.25), guide=FALSE) +
  scale_size_manual(values=c(1, 1, 1, 0.5), guide=FALSE) +
  labs(x="Predicted CSRE", y="Actual CSRE") +
  coord_cartesian(xlim=c(-6, 6), ylim=c(-6, 6)) + 
  theme_ath()

plot.lna.selection


# #' # Shaming and diplomatic conflict factors
# #' 
# #' ## Actual models
# #' 
# #' ### Just severity and shame percent
# model.shame.simple <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
#                            icews.pct.shame + 
#                            as.factor(year.num),
#                          data=filter(full.data, gwf.binary == "Autocracy"))
# 
# #+ results='asis'
# stargazer(model.shame.simple, type="html", 
#           dep.var.caption="CSRE",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           column.labels=names(model.int.all),
#           omit="factor\\(year", 
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 2))))
# 
# #' ### Severity and shame and regional instability
# model.shame.regional <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
#                              icews.pct.shame + 
#                              icrg.pol.risk.regional.loo +
#                              as.factor(year.num),
#                            data=filter(full.data, gwf.binary == "Autocracy"))
# 
# #+ results='asis'
# stargazer(model.shame.regional, type="html", 
#           dep.var.caption="CSRE",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           column.labels=names(model.int.all),
#           omit="factor\\(year", 
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 2))))
# 
# #' ### All shaming variables + controls (interstate)
# model.shame.full.ctrl.state <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
#                                     icews.pct.shame + 
#                                     icrg.pol.risk.regional.loo +
#                                     gdpcap.log + population.log +
#                                     oda.log + countngo + globalization + as.factor(year.num),
#                                   data=filter(full.data, gwf.binary == "Autocracy"))
# 
# #+ results='asis'
# stargazer(model.shame.full.ctrl.state, type="html", 
#           dep.var.caption="CSRE",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           column.labels=names(model.int.all),
#           omit="factor\\(year", 
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 2))))
# 
# #' ### All shaming variables + controls (INGOs)
# model.shame.full.ctrl.ingos <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs.ingos +
#                                     icews.pct.shame.ingos +
#                                     icrg.pol.risk.regional.loo +
#                                     gdpcap.log + population.log +
#                                     oda.log + countngo + globalization + as.factor(year.num),
#                                   data=filter(full.data, gwf.binary == "Autocracy"))
# 
# #+ results='asis'
# stargazer(model.shame.full.ctrl.ingos, type="html", 
#           dep.var.caption="CSRE",
#           dep.var.labels.include=FALSE, no.space=TRUE,
#           column.labels=names(model.int.all),
#           omit="factor\\(year", 
#           add.lines=list(c("Year fixed effects",
#                            rep("Yes", 2))))
# 
# 
# #' ### All models (to LaTeX)
# models.shame <- list("Severity, conflict, and regional instability" = model.shame.regional,
#                      "All controls (interstate)" = model.shame.full.ctrl.state,
#                      "All controls (INGOs)" = model.shame.full.ctrl.ingos)
# 
# coef.labs <- c("Severity of conflictual events (interstate)",
#                "Percent of all events that are conflictual (interstate)", 
#                "Severity of conflictual events (INGOs)",
#                "Percent of all events that are conflictual (INGOs)",
#                "Average political risk in region",
#                "GDP per capita (log)", 
#                "Population (log)", "Foreign aid (log)", 
#                "Number of INGO members", "Globalization")
# extra.lines <- list(c("Year fixed effects",
#                       c(rep("Yes", 8))))
# 
# capture.output({
#   stargazer(models.shame, 
#             type="latex", font.size="tiny",
#             out=file.path(PROJHOME, "Output", "tables", "1-shaming-models-all.tex"),
#             covariate.labels=coef.labs,
#             title="Reputational determinants of restrictions on the civil society regulatory environment",
#             label="models-reputational",
#             dep.var.caption="Civil society regulatory environment (CSRE) in following year",
#             dep.var.labels.include=FALSE,
#             column.labels=names(models.shame),
#             omit="factor\\(year", no.space=TRUE,
#             add.lines=extra.lines)
# }, file="/dev/null")
# 
# #' ## Model plots
# #' 
# #' ### Coefficient plot
# shame.var.order <- c("icews.conflict.severity.abs", "icews.pct.shame", 
#                      "icews.conflict.severity.abs.ingos", "icews.pct.shame.ingos", 
#                      "icrg.pol.risk.regional.loo", "icews.conflict.severity.abs", 
#                      "icews.pct.shame", "icrg.pol.risk.regional.loo", "gdpcap.log", 
#                      "population.log", "oda.log", "countngo", "globalization")
# 
# plot.shame <- fig.coef(models.shame, xlab="Civil society regulatory environment (CSRE)",
#                        var.order=shame.var.order)
# plot.shame
# 
# fig.save.cairo(plot.shame, filename="1-coefs-shame", 
#                width=6, height=3)
# 
# 
# #' ### CSRE across severity of events
# new.data.severity.state <- model.shame.full.ctrl.state$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, icews.conflict.severity.abs)) %>%
#   right_join(data_frame(icews.conflict.severity.abs = seq(0, 10, by=0.1), 
#                         index = 1),
#              by="index") %>% 
#   select(-index)
# 
# plot.predict.state <- model.shame.full.ctrl.state %>% 
#   augment(newdata=new.data.severity.state) %>%
#   mutate(pred = .fitted,
#          pred.lower = pred + (qnorm(0.025) * .se.fit),
#          pred.upper = pred + (qnorm(0.975) * .se.fit),
#          actor = "Interstate") %>%
#   rename(severity = icews.conflict.severity.abs)
# 
# new.data.severity.ingo <- model.shame.full.ctrl.ingos$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, icews.conflict.severity.abs.ingos)) %>%
#   right_join(data_frame(icews.conflict.severity.abs.ingos = seq(0, 10, by=0.1), 
#                         index = 1),
#              by="index") %>% 
#   select(-index)
# 
# plot.predict.ingo <- model.shame.full.ctrl.ingos %>% 
#   augment(newdata=new.data.severity.ingo) %>%
#   mutate(pred = .fitted,
#          pred.lower = pred + (qnorm(0.025) * .se.fit),
#          pred.upper = pred + (qnorm(0.975) * .se.fit),
#          actor = "INGO-based") %>%
#   rename(severity = icews.conflict.severity.abs.ingos)
# 
# plot.predict <- bind_rows(plot.predict.state, plot.predict.ingo) %>%
#   mutate(actor = factor(actor, levels=c("Interstate", "INGO-based"), ordered=TRUE))
# 
# plot.shame.severity.pred <- ggplot(plot.predict, 
#                                    aes(x=severity, y=pred, colour=actor)) + 
#   geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=actor), 
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) + 
#   labs(x="Average severity of conflictual events", 
#        y="Predicted CSRE") + 
#   scale_colour_manual(values=c("#004259", "#4A0A3D"), name=NULL) +
#   scale_fill_manual(values=c("#004259", "#4A0A3D"), name=NULL, guide=FALSE) +
#   theme_ath() 
# plot.shame.severity.pred
# 
# fig.save.cairo(plot.shame.severity.pred, filename="1-shame-severity-pred", 
#                width=5, height=2.5)
# 
# 
# #' ### CSRE across percent of conflictual events
# new.data.shame.state <- model.shame.full.ctrl.state$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, icews.pct.shame)) %>%
#   right_join(data_frame(icews.pct.shame = seq(0, 0.6, by=0.05), 
#                         index = 1),
#              by="index") %>% 
#   select(-index)
# 
# plot.predict.state <- model.shame.full.ctrl.state %>% 
#   augment(newdata=new.data.shame.state) %>%
#   mutate(pred = .fitted,
#          pred.lower = pred + (qnorm(0.025) * .se.fit),
#          pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
#   mutate(actor = "Interstate",
#          pct.shame = icews.pct.shame)
# 
# new.data.shame.ingos <- model.shame.full.ctrl.ingos$model %>%
#   summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
#   mutate(year.num = 2005,
#          index = 1) %>%
#   select(-c(cs_env_sum.lead, icews.pct.shame.ingos)) %>%
#   right_join(data_frame(icews.pct.shame.ingos = seq(0, 0.6, by=0.05), 
#                         index = 1),
#              by="index") %>% 
#   select(-index)
# 
# plot.predict.ingos <- model.shame.full.ctrl.ingos %>% 
#   augment(., newdata=new.data.shame.ingos) %>%
#   mutate(pred = .fitted,
#          pred.lower = pred + (qnorm(0.025) * .se.fit),
#          pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
#   mutate(actor = "INGO-based",
#          pct.shame = icews.pct.shame.ingos)
# 
# plot.predict <- bind_rows(plot.predict.state, plot.predict.ingos) %>%
#   mutate(actor = factor(actor, levels=c("Interstate", "INGO-based"), ordered=TRUE))
# 
# plot.shame.pct.pred <- ggplot(plot.predict, 
#                               aes(x=pct.shame, 
#                                   y=pred, colour=actor)) + 
#   geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=actor), 
#               alpha=0.3, colour=NA) +
#   geom_line(size=1.5) + 
#   labs(x="Percent of events that are conflictual", 
#        y="Predicted CSRE") + 
#   scale_colour_manual(values=c("#004259", "#4A0A3D"), name=NULL) +
#   scale_fill_manual(values=c("#004259", "#4A0A3D"), name=NULL, guide=FALSE) +
#   scale_x_continuous(labels=percent) +
#   theme_ath()
# plot.shame.pct.pred
# 
# fig.save.cairo(plot.shame.pct.pred, filename="1-shame-pct-pred", 
#                width=5, height=3)
# 
# 
# #' # Bayesian tinkering?
# #' 
# #' See:
# #' 
# #' - https://cran.r-project.org/web/packages/rstanarm/vignettes/lm.html
# #' - https://cran.r-project.org/web/packages/rstanarm/vignettes/continuous.html
# #' 
# #' Good, solid, interpretable results, but craaaaazy slow
# 
# # library(rstanarm)
# # options(mc.cores = parallel::detectCores())  # Use all possible cores
# # 
# # model.simple.b <- stan_glm(cs_env_sum.lead ~ icrg.stability + e_polity2,
# #                           data=full.data, family=gaussian(),
# #                           prior=cauchy(), prior_intercept=cauchy(),
# #                           seed=my.seed)
# # print(model.simple.b, digits=2)
# # plot(model.simple.b, pars=c("icrg.stability", "e_polity2"))
# # pp_check(model.simple.b, check="distributions", overlay=FALSE, nreps=5)
