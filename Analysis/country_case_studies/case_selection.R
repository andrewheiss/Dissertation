#' ---
#' title: "LNA-based case selection for SNA"
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

# Load libraries
library(printr)
library(tidyverse)
library(DT)
library(countrycode)
library(rstanarm)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

# Load data
models.bayes <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                                  "models_to_keep.rds"))

autocracies <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                                 "autocracies.rds"))

# General settings
my.seed <- 1234
set.seed(my.seed)


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
