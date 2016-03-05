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
library(scales)
library(gridExtra)
library(Cairo)
library(stargazer)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

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


#' # Internal factors
#' 
#' ## Actual models
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

#' ## Model plots
#' 
#' ### Coefficient plot
plot.int <- arrangeGrob(fig.coef(model.int.simple, "Simple", legend=FALSE), 
                        fig.coef(model.int.full, "Full", legend=FALSE), 
                        fig.coef(model.int.all, "Full + controls", ylab="Coefficient"), 
                        heights=c(1.5/10, 2.5/10, 6/10), ncol=1)
grid::grid.draw(plot.int)

fig.save.cairo(plot.int, filename="1-coefs-int", 
               width=6, height=6)


#' ### CSRE across range of ICRG risk
new.data.icrg <- model.int.all %>%
  map_df("model", .id="regime.type") %>%
  group_by(regime.type) %>%
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
  map_df(~ augment(., newdata=new.data.icrg), .id="regime.type.pred") %>%
  filter(regime.type == regime.type.pred) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         regime.type = factor(regime.type, 
                              levels=c("Democracy", "Autocracy"), 
                              labels=c("Democracies    ", "Autocracies"),  
                              ordered=TRUE))

plot.icrg.int.pred <- ggplot(plot.predict, aes(x=icrg.pol.risk.internal.scaled, 
                                               y=pred, colour=regime.type)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=regime.type), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Internal political risk (ICRG)", y="Predicted CSRE") + 
  scale_colour_manual(values=c("#BEDB3A", "#441152"), name=NULL) +
  scale_fill_manual(values=c("#BEDB3A", "#441152"), name=NULL, guide=FALSE) +
  theme_ath()
plot.icrg.int.pred

fig.save.cairo(plot.icrg.int.pred, filename="1-icrg-int-pred", 
               width=5, height=3)


#' ### CSRE across years executive has been in office
new.data.yrsoffc <- model.int.all %>%
  map_df("model", .id="regime.type") %>%
  group_by(regime.type) %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icrg.pol.risk.internal.scaled, yrsoffc)) %>%
  right_join(expand.grid(icrg.pol.risk.internal.scaled = seq(0, 100, by=0.1), 
                         yrsoffc = c(2, 20),
                         index = 1, stringsAsFactors=FALSE),
             by="index") %>% 
  select(-index)

plot.predict <- model.int.all %>% 
  map_df(~ augment(., newdata=new.data.yrsoffc), .id="regime.type.pred") %>%
  filter(regime.type == regime.type.pred) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         regime.type = factor(regime.type, 
                              levels=c("Democracy", "Autocracy"), 
                              labels=c("Democracies    ", "Autocracies"), 
                              ordered=TRUE),
         yrsoffc.clean = factor(yrsoffc, labels=paste(unique(yrsoffc), 
                                                      "years since election")))

plot.icrg.yrs.int.pred <- ggplot(plot.predict, 
                                 aes(x=icrg.pol.risk.internal.scaled, 
                                     y=pred, colour=regime.type)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=regime.type), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Internal political risk (ICRG)", y="Predicted CSRE") + 
  scale_colour_manual(values=c("#BEDB3A", "#441152"), name=NULL) +
  scale_fill_manual(values=c("#BEDB3A", "#441152"), name=NULL, guide=FALSE) +
  theme_ath() + facet_wrap(~ yrsoffc.clean)
plot.icrg.yrs.int.pred

fig.save.cairo(plot.icrg.yrs.int.pred, filename="1-icrg-yrs-int-pred", 
               width=5, height=3)


#' # External factors
#' 
#' ## Actual models
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

#' ## Model plots
#' 
#' ### Coefficient plot
plot.ext <- arrangeGrob(fig.coef(model.ext.neighbors, 
                                 "Neighbors", legend=FALSE), 
                        fig.coef(model.ext.neighbors.subregion, 
                                 "Subregion", legend=FALSE), 
                        fig.coef(model.ext.neighbors.subregion.ctrl, 
                                 "Subregion + controls", ylab="Coefficient"), 
                        heights=c(2/10, 2.5/10, 5.5/10), ncol=1)
grid::grid.draw(plot.ext)

fig.save.cairo(plot.ext, filename="1-coefs-ext", 
               width=6, height=6)


#' ### CSRE across neighbor stability and coups in region
new.data.ext <- model.ext.neighbors.subregion.ctrl %>%
  map_df("model", .id="regime.type") %>%
  group_by(regime.type) %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         neighbor.coups.activity.bin = FALSE,
         index = 1) %>%
  select(-c(cs_env_sum.lead, neighbor.pol.risk.min,
            coups.activity.subregional)) %>%
  right_join(expand.grid(neighbor.pol.risk.min = seq(0, 100, by=0.1), 
                         coups.activity.subregional = c(0, 2),
                         index = 1),
             by="index") %>% 
  select(-index)

plot.predict <- model.ext.neighbors.subregion.ctrl %>% 
  map_df(~ augment(., newdata=new.data.ext), .id="regime.type.pred") %>%
  filter(regime.type == regime.type.pred) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         regime.type = factor(regime.type, 
                              levels=c("Democracy", "Autocracy"), 
                              labels=c("Democracies    ", "Autocracies"), 
                              ordered=TRUE),
         coup.in.region = factor(coups.activity.subregional, 
                                 labels=c("No coup activity in subregion", 
                                          "Moderate coup activity in subregion")))

plot.neighbor.coup.pred <- ggplot(plot.predict, 
                                 aes(x=neighbor.pol.risk.min, 
                                     y=pred, colour=regime.type)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=regime.type), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Minimum political risk in neighboring countries (ICRG)", 
       y="Predicted CSRE") + 
  scale_colour_manual(values=c("#BEDB3A", "#441152"), name=NULL) +
  scale_fill_manual(values=c("#BEDB3A", "#441152"), name=NULL, guide=FALSE) +
  theme_ath() + facet_wrap(~ coup.in.region)
plot.neighbor.coup.pred

fig.save.cairo(plot.neighbor.coup.pred, filename="1-icrg-neighbor-coup-ext-pred", 
               width=5, height=3)


#' # Shaming and diplomatic conflict factors
#' 
#' ## Actual models
#' 
#' ### Just severity and shame percent
model.shame.simple <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
             icews.pct.shame + 
             as.factor(year.num),
           data=.))

#+ results='asis'
stargazer(model.shame.simple, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### Severity and shame and regional instability
model.shame.regional <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
             icews.pct.shame + 
             icrg.pol.risk.regional +
             as.factor(year.num),
           data=.))

#+ results='asis'
stargazer(model.shame.regional, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### All shaming variables + controls
model.shame.full.ctrl <- full.data %>%
  split(.$polity_ord2) %>%
  map(~ lm(cs_env_sum.lead ~ icews.conflict.severity.abs + 
             icews.pct.shame + 
             icrg.pol.risk.regional +
             gdpcap.log + population.log +
             oda.log + countngo + globalization + as.factor(year.num),
           data=.))

#+ results='asis'
stargazer(model.shame.full.ctrl, type="html", 
          dep.var.caption="CSRE",
          dep.var.labels.include=FALSE, no.space=TRUE,
          column.labels=names(model.int.all),
          omit="factor\\(year", 
          add.lines=list(c("Year fixed effects",
                           rep("Yes", 2))))

#' ### All models (to LaTeX)
all.models <- c(model.shame.simple, model.shame.regional, 
                model.shame.full.ctrl)

coef.labs <- c("Severity of conflictual events",
               "Percent of all events that are conflictual", 
               "Average political risk in region",
               "GDP per capita (log)", 
               "Population (log)", "Foreign aid (log)", 
               "Number of INGO members", "Globalization")
extra.lines <- list(c("Year fixed effects",
                      c(rep("Yes", 6))))

capture.output({
  stargazer(all.models, type="latex", 
            out=file.path(PROJHOME, "Output", "tables", "1-shaming-models-all.tex"),
            covariate.labels=coef.labs,
            dep.var.caption="Civil society regulatory environment (CSRE) in following year",
            dep.var.labels.include=FALSE,
            column.labels=names(all.models),
            omit="factor\\(year", no.space=TRUE,
            add.lines=extra.lines)
}, file="/dev/null")

#' ## Model plots
#' 
#' ### Coefficient plot
plot.shame <- arrangeGrob(fig.coef(model.shame.simple, 
                                   "Severity and conflict", legend=FALSE), 
                          fig.coef(model.shame.regional, 
                                   "Severity, conflict, and regional instability", 
                                   legend=FALSE), 
                          fig.coef(model.shame.full.ctrl, 
                                   "All controls", ylab="Coefficient"), 
                          heights=c(2/10, 2.5/10, 5.5/10), ncol=1)
grid::grid.draw(plot.shame)

fig.save.cairo(plot.shame, filename="1-coefs-shame", 
               width=6, height=6)

#' ### CSRE across severity of events
new.data.severity <- model.shame.full.ctrl %>%
  map_df("model", .id="regime.type") %>%
  group_by(regime.type) %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icews.conflict.severity.abs)) %>%
  right_join(data_frame(icews.conflict.severity.abs = seq(0, 10, by=0.1), 
                        index = 1),
             by="index") %>% 
  select(-index)

plot.predict <- model.shame.full.ctrl %>% 
  map_df(~ augment(., newdata=new.data.severity), .id="regime.type.pred") %>%
  filter(regime.type == regime.type.pred) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         regime.type = factor(regime.type, 
                              levels=c("Democracy", "Autocracy"), 
                              labels=c("Democracies    ", "Autocracies"), 
                              ordered=TRUE))

plot.shame.severity.pred <- ggplot(plot.predict, 
                                   aes(x=icews.conflict.severity.abs, 
                                       y=pred, colour=regime.type)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=regime.type), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Average severity of conflictual events", 
       y="Predicted CSRE") + 
  scale_colour_manual(values=c("#BEDB3A", "#441152"), name=NULL) +
  scale_fill_manual(values=c("#BEDB3A", "#441152"), name=NULL, guide=FALSE) +
  theme_ath() 
plot.shame.severity.pred

fig.save.cairo(plot.shame.severity.pred, filename="1-shame-severity-pred", 
               width=5, height=3)

#' ### CSRE across percent of conflictual events
new.data.shame <- model.shame.full.ctrl %>%
  map_df("model", .id="regime.type") %>%
  group_by(regime.type) %>%
  summarise_each(funs(mean), -c(`as.factor(year.num)`)) %>%
  mutate(year.num = 2005,
         index = 1) %>%
  select(-c(cs_env_sum.lead, icews.pct.shame)) %>%
  right_join(data_frame(icews.pct.shame = seq(0, 0.75, by=0.05), 
                        index = 1),
             by="index") %>% 
  select(-index)

plot.predict <- model.shame.full.ctrl %>% 
  map_df(~ augment(., newdata=new.data.shame), .id="regime.type.pred") %>%
  filter(regime.type == regime.type.pred) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         regime.type = factor(regime.type, 
                              levels=c("Democracy", "Autocracy"), 
                              labels=c("Democracies    ", "Autocracies"), 
                              ordered=TRUE))

plot.shame.pct.pred <- ggplot(plot.predict, 
                              aes(x=icews.pct.shame, 
                                  y=pred, colour=regime.type)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=regime.type), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Percent of all events that are conflictual", 
       y="Predicted CSRE") + 
  scale_colour_manual(values=c("#BEDB3A", "#441152"), name=NULL) +
  scale_fill_manual(values=c("#BEDB3A", "#441152"), name=NULL, guide=FALSE) +
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
