#' ---
#' title: "Analysis for survey chapter"
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
#' bibliography: /Users/andrew/Dropbox/Readings/Papers.bib
#' csl: /Users/andrew/.pandoc/csl/american-political-science-association.csl
#' ...

#' ## Load clean data
#+ message=FALSE
knitr::opts_chunk$set(cache=TRUE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

library(plyr)  # Because of productplots
library(tidyverse)
library(forcats)
library(stringr)
library(productplots)
library(pander)
library(DT)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))
source(file.path(PROJHOME, "Analysis", "lib", "cat_analysis.R"))

# Reproducibility
my.seed <- 1234
set.seed(my.seed)

# Bayesian stuff
CHAINS <- 4
ITER <-2000
WARMUP <- 1000
options(mc.cores = 1)  # No need to parallelize with simple models

# Treat ordered factors as treatments in models
options(contrasts=rep("contr.treatment", 2))

# Load cleaned, country-based survey data (*with* the Q4\* loop)
survey.clean.all <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                      "survey_clean_all.rds"))

# Load cleaned, organization-based data (without the Q4 loop)
survey.orgs.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                       "survey_orgs_clean.rds"))

# Load cleaned, country-based data (only the Q4 loop)
survey.countries.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                            "survey_countries_clean.rds"))

# Load Robinson map projection
countries.ggmap <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                                     "countries110_robinson_ggmap.rds"))

# All possible countries (to fix the South Sudan issue)
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))

# Survey responses
great.none.dk <- c("A great deal", "A lot", "A moderate amount",
                   "A little", "None at all", "Don't know", "Not applicable")
great.none <- great.none.dk[1:5]
