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


#' # Internal factors
#' 
#' ## Actual models
#' 
#' ### Just internal political risk
model.int.simple <- lm(cs_env_sum.lead ~ icrg.pol.risk.internal.scaled,
                       data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.int.simple, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))

#' ### All internal variables
model.int.full <- lm(cs_env_sum.lead ~ icrg.pol.risk.internal.scaled +
                       yrsoffc + years.since.comp + opp1vote + as.factor(year.num),
                     data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.int.full, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.full),
          omit="factor\\(year",
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### All internal variables + controls
model.int.all <- lm(cs_env_sum.lead ~ icrg.pol.risk.internal.scaled +
                      yrsoffc + years.since.comp + opp1vote +
                      physint + gdpcap.log + population.log +
                      oda.log + countngo + globalization + as.factor(year.num),
                    data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.int.all, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))


#' ### All models (to LaTeX)
models.int <- list("Simple" = model.int.simple, "Full" = model.int.full,
                   "Full + controls" = model.int.all)

coef.labs <- c("Internal political risk (ICRG)", "Years executive in office", 
               "Years since competitive election", "Opposition vote share",
               "Physical integrity rights", "GDP per capita (log)", 
               "Population (log)", "Foreign aid (log)", 
               "Number of INGO members", "Globalization")
extra.lines <- list(c("Year fixed effects",
                      c(rep("No", 2), rep("Yes", 4))))

capture.output({
  stargazer(models.int,
            type="latex", font.size="tiny",
            out=file.path(PROJHOME, "Output", "tables", "1-internal-models-all.tex"),
            covariate.labels=coef.labs,
            title="Internal determinants of restrictions on the civil society regulatory environment",
            label="models-internal",
            dep.var.caption="Civil society regulatory environment (CSRE) in following year",
            dep.var.labels.include=FALSE,
            column.labels=names(models.int),
            omit="factor\\(year", no.space=TRUE,
            add.lines=extra.lines)
}, file="/dev/null")

#' ## Model plots
#' 
#' ### Coefficient plot
plot.int <- fig.coef(models.int, xlab="Civil society regulatory environment (CSRE)")
plot.int

fig.save.cairo(plot.int, filename="1-coefs-int", 
               width=6, height=3)


#' ### CSRE across range of ICRG risk
new.data.icrg <- model.int.all$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.pol.risk.internal.scaled)) %>%
  right_join(data_frame(icrg.pol.risk.internal.scaled = 
                          seq(0, 100, by=0.1), index = 1),
             by="index") %>% 
  select(-index)

# When mapping augment onto the new data, it makes predictions for both types
# of regimes, which isn't necessary (i.e. no need to use the democracy model
# on hypothetical data intended for the autocracy model), so only keep rows
# where the model matches the hypothetical data
plot.predict <- model.int.all %>% 
  augment(newdata=new.data.icrg) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit))

plot.icrg.int.pred <- ggplot(plot.predict, aes(x=icrg.pol.risk.internal.scaled, 
                                               y=pred)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Internal political risk (ICRG)", y="Predicted CSRE") + 
  scale_colour_manual(values=c(col.auth), name=NULL) +
  scale_fill_manual(values=c(col.auth), name=NULL, guide=FALSE) +
  coord_cartesian(ylim=c(-4, 4)) +
  theme_ath()
plot.icrg.int.pred

fig.save.cairo(plot.icrg.int.pred, filename="1-icrg-int-pred", 
               width=5, height=3)


#' ### CSRE across years executive has been in office
new.data.yrsoffc <- model.int.all$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.pol.risk.internal.scaled, yrsoffc)) %>%
  right_join(expand.grid(icrg.pol.risk.internal.scaled = seq(0, 100, by=0.1), 
                         yrsoffc = c(2, 30),
                         index = 1, stringsAsFactors=FALSE),
             by="index") %>% 
  select(-index)

plot.predict <- model.int.all %>% 
  augment(newdata=new.data.yrsoffc) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         yrsoffc.clean = factor(yrsoffc, labels=paste(unique(yrsoffc), 
                                                      "years in office")))

plot.icrg.yrs.int.pred <- ggplot(plot.predict, 
                                 aes(x=icrg.pol.risk.internal.scaled, 
                                     y=pred, colour=yrsoffc.clean)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=yrsoffc.clean), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Internal political risk (ICRG)", y="Predicted CSRE") + 
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

#' # External factors
#' 
#' ## Actual models
#' 
#' ### Just neighbors
model.ext.neighbors <-lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
                           neighbor.coups.activity.bin,
                         data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.ext.neighbors, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))

#' ### Neighbors and subregion
model.ext.neighbors.subregion <- lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
                                      icrg.pol.risk.subregional.loo +
                                      neighbor.coups.activity.bin + 
                                      coups.activity.subregional,
                                    data=filter(full.data, 
                                                gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.ext.neighbors.subregion, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))

#' ### Neighbors and region
model.ext.neighbors.region <- lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
                                   icrg.pol.risk.regional.loo +
                                   neighbor.coups.activity.bin + 
                                   coups.activity.regional,
                                 data=filter(full.data, 
                                             gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.ext.neighbors.region, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("No", 2))))


#' ### Neighbors and subregion + controls
model.ext.neighbors.subregion.ctrl <- lm(cs_env_sum.lead ~ neighbor.pol.risk.min + 
                                           icrg.pol.risk.subregional.loo +
                                           neighbor.coups.activity.bin + 
                                           coups.activity.subregional + 
                                           gdpcap.log + population.log + oda.log + 
                                           countngo + globalization + as.factor(year.num),
                                         data=filter(full.data, 
                                                     gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.ext.neighbors.subregion.ctrl, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.simple),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))


#' ### All models (to LaTeX)
models.ext <- list("Neighbors" = model.ext.neighbors,
                   "Subregion" = model.ext.neighbors.subregion,
                   "Subregion + controls" = model.ext.neighbors.subregion.ctrl)

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
  stargazer(models.ext,
            type="latex", font.size="tiny",
            out=file.path(PROJHOME, "Output", "tables", "1-external-models-all.tex"),
            covariate.labels=coef.labs,
            title="External determinants of restrictions on the civil society regulatory environment",
            label="models-external",
            dep.var.caption="Civil society regulatory environment (CSRE) in following year",
            dep.var.labels.include=FALSE,
            column.labels=names(models.ext),
            omit="factor\\(year", no.space=TRUE,
            add.lines=extra.lines)
}, file="/dev/null")

#' ## Model plots
#' 
#' ### Coefficient plot
plot.ext <- fig.coef(models.ext, xlab="Civil society regulatory environment (CSRE)")
plot.ext

fig.save.cairo(plot.ext, filename="1-coefs-ext", 
               width=6, height=3)


#' ### CSRE across neighbor stability and coups in region
new.data.ext <- model.ext.neighbors.subregion.ctrl$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         neighbor.coups.activity.bin = FALSE,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.pol.risk.subregional.loo,
            coups.activity.subregional)) %>%
  right_join(expand.grid(icrg.pol.risk.subregional.loo = seq(0, 100, by=0.1), 
                         coups.activity.subregional = c(0, 2),
                         index = 1),
             by="index") %>% 
  select(-index)

plot.predict <- model.ext.neighbors.subregion.ctrl %>% 
  augment(newdata=new.data.ext) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         coup.in.region = factor(coups.activity.subregional, 
                                 labels=c("No coup activity in subregion", 
                                          "Moderate coup activity in subregion")))

plot.subregion.coup.pred <- ggplot(plot.predict, 
                                   aes(x=icrg.pol.risk.subregional.loo, 
                                       y=pred, colour=coup.in.region)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=coup.in.region), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Average political risk in subregion (ICRG)", 
       y="Predicted CSRE") + 
  scale_colour_manual(values=c("#004259", "#4A0A3D"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#4A0A3D"), name=NULL, guide=FALSE) +
  theme_ath()
plot.subregion.coup.pred

fig.save.cairo(plot.subregion.coup.pred, filename="1-icrg-subregion-coup-ext-pred", 
               width=5, height=2.5)


#' # Shaming and diplomatic conflict factors
#' 
#' ## Actual models
#' 
#' ### Just severity and shame percent
model.shame.simple <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
                           icews.pct.shame + 
                           as.factor(year.num),
                         data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.shame.simple, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### Severity and shame and regional instability
model.shame.regional <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
                             icews.pct.shame + 
                             icrg.pol.risk.regional.loo +
                             as.factor(year.num),
                           data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.shame.regional, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### All shaming variables + controls (interstate)
model.shame.full.ctrl.state <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
                                    icews.pct.shame + 
                                    icrg.pol.risk.regional.loo +
                                    gdpcap.log + population.log +
                                    oda.log + countngo + globalization + as.factor(year.num),
                                  data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.shame.full.ctrl.state, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### All shaming variables + controls (INGOs)
model.shame.full.ctrl.ingos <- lm(cs_env_sum.lead ~ icews.conflict.severity.abs.ingos +
                                    icews.pct.shame.ingos +
                                    icrg.pol.risk.regional.loo +
                                    gdpcap.log + population.log +
                                    oda.log + countngo + globalization + as.factor(year.num),
                                  data=filter(full.data, gwf.binary == "Autocracy"))

#+ results='asis'
stargazer(model.shame.full.ctrl.ingos, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))


#' ### All models (to LaTeX)
models.shame <- list("Severity, conflict, and regional instability" = model.shame.regional,
                     "All controls (interstate)" = model.shame.full.ctrl.state,
                     "All controls (INGOs)" = model.shame.full.ctrl.ingos)

coef.labs <- c("Severity of conflictual events (interstate)",
               "Percent of all events that are conflictual (interstate)", 
               "Severity of conflictual events (INGOs)",
               "Percent of all events that are conflictual (INGOs)",
               "Average political risk in region",
               "GDP per capita (log)", 
               "Population (log)", "Foreign aid (log)", 
               "Number of INGO members", "Globalization")
extra.lines <- list(c("Year fixed effects",
                      c(rep("Yes", 8))))

capture.output({
  stargazer(models.shame, 
            type="latex", font.size="tiny",
            out=file.path(PROJHOME, "Output", "tables", "1-shaming-models-all.tex"),
            covariate.labels=coef.labs,
            title="Reputational determinants of restrictions on the civil society regulatory environment",
            label="models-reputational",
            dep.var.caption="Civil society regulatory environment (CSRE) in following year",
            dep.var.labels.include=FALSE,
            column.labels=names(models.shame),
            omit="factor\\(year", no.space=TRUE,
            add.lines=extra.lines)
}, file="/dev/null")

#' ## Model plots
#' 
#' ### Coefficient plot
shame.var.order <- c("icews.conflict.severity.abs", "icews.pct.shame", 
                     "icews.conflict.severity.abs.ingos", "icews.pct.shame.ingos", 
                     "icrg.pol.risk.regional.loo", "icews.conflict.severity.abs", 
                     "icews.pct.shame", "icrg.pol.risk.regional.loo", "gdpcap.log", 
                     "population.log", "oda.log", "countngo", "globalization")

plot.shame <- fig.coef(models.shame, xlab="Civil society regulatory environment (CSRE)",
                       var.order=shame.var.order)
plot.shame

fig.save.cairo(plot.shame, filename="1-coefs-shame", 
               width=6, height=3)


#' ### CSRE across severity of events
new.data.severity.state <- model.shame.full.ctrl.state$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icews.conflict.severity.abs)) %>%
  right_join(data_frame(icews.conflict.severity.abs = seq(0, 10, by=0.1), 
                        index = 1),
             by="index") %>% 
  select(-index)

plot.predict.state <- model.shame.full.ctrl.state %>% 
  augment(newdata=new.data.severity.state) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         actor = "Interstate") %>%
  rename(severity = icews.conflict.severity.abs)

new.data.severity.ingo <- model.shame.full.ctrl.ingos$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icews.conflict.severity.abs.ingos)) %>%
  right_join(data_frame(icews.conflict.severity.abs.ingos = seq(0, 10, by=0.1), 
                        index = 1),
             by="index") %>% 
  select(-index)

plot.predict.ingo <- model.shame.full.ctrl.ingos %>% 
  augment(newdata=new.data.severity.ingo) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         actor = "INGO-based") %>%
  rename(severity = icews.conflict.severity.abs.ingos)

plot.predict <- bind_rows(plot.predict.state, plot.predict.ingo) %>%
  mutate(actor = factor(actor, levels=c("Interstate", "INGO-based"), ordered=TRUE))

plot.shame.severity.pred <- ggplot(plot.predict, 
                                   aes(x=severity, y=pred, colour=actor)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=actor), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Average severity of conflictual events", 
       y="Predicted CSRE") + 
  scale_colour_manual(values=c("#004259", "#4A0A3D"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#4A0A3D"), name=NULL, guide=FALSE) +
  theme_ath() 
plot.shame.severity.pred

fig.save.cairo(plot.shame.severity.pred, filename="1-shame-severity-pred", 
               width=5, height=2.5)


#' ### CSRE across percent of conflictual events
new.data.shame.state <- model.shame.full.ctrl.state$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icews.pct.shame)) %>%
  right_join(data_frame(icews.pct.shame = seq(0, 0.6, by=0.05), 
                        index = 1),
             by="index") %>% 
  select(-index)

plot.predict.state <- model.shame.full.ctrl.state %>% 
  augment(newdata=new.data.shame.state) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(actor = "Interstate",
         pct.shame = icews.pct.shame)

new.data.shame.ingos <- model.shame.full.ctrl.ingos$model %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icews.pct.shame.ingos)) %>%
  right_join(data_frame(icews.pct.shame.ingos = seq(0, 0.6, by=0.05), 
                        index = 1),
             by="index") %>% 
  select(-index)

plot.predict.ingos <- model.shame.full.ctrl.ingos %>% 
  augment(., newdata=new.data.shame.ingos) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit)) %>%
  mutate(actor = "INGO-based",
         pct.shame = icews.pct.shame.ingos)

plot.predict <- bind_rows(plot.predict.state, plot.predict.ingos) %>%
  mutate(actor = factor(actor, levels=c("Interstate", "INGO-based"), ordered=TRUE))

plot.shame.pct.pred <- ggplot(plot.predict, 
                              aes(x=pct.shame, 
                                  y=pred, colour=actor)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=actor), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Percent of events that are conflictual", 
       y="Predicted CSRE") + 
  scale_colour_manual(values=c("#004259", "#4A0A3D"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#4A0A3D"), name=NULL, guide=FALSE) +
  scale_x_continuous(labels=percent) +
  theme_ath()
plot.shame.pct.pred

fig.save.cairo(plot.shame.pct.pred, filename="1-shame-pct-pred", 
               width=5, height=3)


#' # Bayesian tinkering?
#' 
#' See:
#' 
#' - https://cran.r-project.org/web/packages/rstanarm/vignettes/lm.html
#' - https://cran.r-project.org/web/packages/rstanarm/vignettes/continuous.html
#' 
#' Good, solid, interpretable results, but craaaaazy slow

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
