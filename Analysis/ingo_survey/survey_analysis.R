#' ---
#' title: "Survey analysis"
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
#'     keep_md: yes
#' bibliography: /Users/andrew/Dropbox/Readings/Papers.bib
#' csl: /Users/andrew/.pandoc/csl/american-political-science-association.csl
#' ...

#' ## Load clean data
#+ message=FALSE
knitr::opts_chunk$set(fig.retina=2)

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggstance)
library(stringr)
library(pander)
library(magrittr)
library(DT)
library(scales)
library(countrycode)
library(tm)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

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



#' ## Testing hypotheses
#' 
#' My claim:
#' 
#' > If an INGO does not make one of these three adjustments—shifting its ideal
#' point, increasing its operational flexibility, or becoming essential for the
#' regime—it runs a high risk of expulsion.
#' 
#' What influences feelings of restriction? Main hypotheses:
#' 
#' - Normative principles and ideals
#'     - Issue area
#' - Instrumental flexibility
#'     - Size
#'     - Type of funding
#'     - Collaboration
#' - Alignment with regime preferences
#' 
