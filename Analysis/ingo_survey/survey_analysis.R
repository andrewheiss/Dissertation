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
knitr::opts_chunk$set(fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

library(plyr)  # Because of productplots
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggstance)
library(productplots)
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

# ------------------
# Useful functions
# ------------------
analyze.cat.var <- function(cat.table) {
  cat("Table counts\n")
  print(cat.table)
  
  cat("\nRow proporitions\n")
  print(prop.table(cat.table, margin=1))
  
  cat("\nColumn proporitions\n")
  print(prop.table(cat.table, margin=2))
  
  cat("\n\nChi-squared test for table\n")
  cat.table.chi <- chisq.test(cat.table) %>% print()
  
  cat("Cramer's V\n")
  vcd::assocstats(cat.table)$cramer %>% print()
  
  cat("\nComponents of chi-squared\n")
  components <- cat.table.chi$residuals^2 %>% print()
  round(1-pchisq(components, cat.table.chi$parameter), 3) %>% print()
}

#' ## Organizational characteristics and regime type
#' 
#' How do respondents differ across the regime types of the countries they work in?
#' 
#' ### Distribution of NGOs across regime types
#' 
#' Regime types of the home countries for each organization
home.regime.type <- survey.orgs.clean %>%
  group_by(home.regime.type) %>%
  summarise(num = n()) %>%
  mutate(prop = num / sum(num))
home.regime.type

#' Regime types of the target countries for each country-organization
work.regime.type <- survey.countries.clean %>%
  group_by(target.regime.type) %>%
  summarise(num = n())%>%
  mutate(prop = num / sum(num))
work.regime.type

#' Most NGOs are based in democracies (only 11% are headquartered in autocracies), but a third of them answered questions about their work in autocracies. 
#' 

#' ### Issues worked on across regime types

#' ### Activities across regime types

# TODO: Staffing, collaboration, funding, etc.

# TODO: Figure out how to deal with org-level regime type analysis, since organizations work in multiple countries and answered only for one. Proportion of countries they work in that are autocracies?


#' ## Relationships with governments across regime type
#' 
#' This analysis is more straightforward, since each country-organization 
#' response is limited to a single target country. The questions also deal with
#' the organization's specific actions in the country, not what they do in all 
#' countries.
#' 
#' 
#' ### Time spent working in the country
#' 
#' Most NGOs working in democracies have been there for 10+ years, while most
#' NGOs working in autocracies have only been there for 1-4 years, and the
#' differences between time worked in country across regime types are
#' significantly different from expected values. NGOs that work in autocracies
#' tend to have less of a legacy or history of working there, are possibly less
#' likely to have a history of working with the government.
#' 
df.time.country.regime <- survey.countries.clean %>%
  select(Q4.2, target.regime.type) %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = droplevels(Q4.2),
         Q4.2 = factor(Q4.2, levels=rev(levels(Q4.2))))

plot.time.country.regime <- prodplot(df.time.country.regime,
                                     ~ target.regime.type + Q4.2, mosaic("h"), 
                                     colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=5, fig.height=3
plot.time.country.regime

time.country.table <- survey.countries.clean %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = droplevels(Q4.2)) %>%
  xtabs(~ Q4.2 + target.regime.type, .)

analyze.cat.var(time.country.table)



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
