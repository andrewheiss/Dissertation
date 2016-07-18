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
  labs(title="Length of time in country, by regime type",
       subtitle="Q4.2: How long has your organization worked in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.time.country.regime

time.country.table <- survey.countries.clean %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = droplevels(Q4.2)) %>%
  xtabs(~ Q4.2 + target.regime.type, .)

analyze.cat.var(time.country.table)


#' ### How NGOs operate in country
#' 
#' NGOs in autocracies definitely pursue different operational strategies. The 
#' most common strategy for these NGOs is to provide funding to domestic NGOs, 
#' while the least common is to maintain an office staffed by foreigners
#' (difference is statistically significant). International NGOs seem to be
#' more likely to take a hands off approach to advocacy in target countries
#' that are autocracies.
#' 
what.do <- c("Maintain a physical office staffed primarily by foreigners",
             "Maintain a physical office staffed primarily by people from target_country",
             "Provide funding to domestic NGOs", "Partner with domestic NGOs")
what.do.short <- c("Maintain a physical office staffed\nprimarily by foreigners",
                   "Maintain a physical office staffed\nprimarily by people from target_country",
                   "Provide funding to domestic NGOs", "Partner with domestic NGOs")

df.operations.regime <- survey.countries.clean %>%
  unnest(Q4.3_value) %>%
  select(Q4.3_value, target.regime.type) %>%
  filter(!is.na(Q4.3_value), Q4.3_value != "Don't know") %>%
  mutate(Q4.3 = factor(Q4.3_value, levels=what.do, 
                       labels=what.do.short, ordered=TRUE))

plot.operations.regime <- prodplot(df.operations.regime,
                                     ~ target.regime.type + Q4.3, mosaic("h"), 
                                     colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="How INGOs work in the target country, by regime type",
       subtitle="Q4.3: What does your organization do in `target_country`?\n(multiple answers allowed)") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.operations.regime

operations.table <- survey.countries.clean %>%
  unnest(Q4.3_value) %>%
  filter(Q4.3_value != "Don't know") %>%
  xtabs(~ Q4.3_value + target.regime.type, .)

analyze.cat.var(operations.table)


#' ### Registration
#' 
#' There's a slight difference in how NGOs register across regimes, with more
#' NGOs registering in autocracies than in democracies, perhaps because they
#' are more likely to be *required* to register in autocracies. The difference
#' is not significant, though.
#' 
df.registered.regime <- survey.countries.clean %>%
  select(Q4.4, target.regime.type) %>%
  filter(Q4.4 != "Don't know") %>%
  mutate(Q4.4 = droplevels(Q4.4),
         Q4.4 = factor(Q4.4, levels=rev(levels(Q4.4))))

plot.registered.regime <- prodplot(df.registered.regime,
                                   ~ target.regime.type + Q4.4, mosaic("h"), 
                                   colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Registration status, by regime type",
       subtitle="Q4.4: Is your organization registered with the national government in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.registered.regime

registered.table <- survey.countries.clean %>%
  filter(Q4.4 != "Don't know") %>%
  mutate(Q4.4 = droplevels(Q4.4)) %>%
  xtabs(~ Q4.4 + target.regime.type, .)

analyze.cat.var(registered.table)


#' ## Contact with government across regime type
#' 
#' ### Frequency of contact with government
#' 
#' There's a dramatic difference in how NGOs report to the government in
#' autocracies. INGOs that work in autocracies report the most often and are
#' the least likely to never report, possibly reflective of stricter reporting
#' requirements.
#' 
freq.collapsed <- c("More than once a year", "Once a year", 
                    "Regularly but not often", "Never")

df.freq.report.regime <- survey.countries.clean %>%
  filter(!is.na(Q4.8.clean)) %>%
  mutate(Q4.8.collapsed = case_when(
    .$Q4.8.clean == "Once a week" ~ freq.collapsed[1],
    .$Q4.8.clean == "More than once a month,\nless than once a week" ~ freq.collapsed[1],
    .$Q4.8.clean == "Once a month" ~ freq.collapsed[1], 
    .$Q4.8.clean == "More than once a year,\nless than once a month" ~ freq.collapsed[1],
    .$Q4.8.clean == "Once a year" ~ freq.collapsed[2],
    .$Q4.8.clean == "As necessary/depends" ~ freq.collapsed[3],
    .$Q4.8.clean == "Once every 2+ years" ~ freq.collapsed[3],
    .$Q4.8.clean == "Never" ~ freq.collapsed[4],
    TRUE ~ NA_character_)
    ) %>%
  filter(!is.na(Q4.8.collapsed)) %>%
  mutate(Q4.8.collapsed = factor(Q4.8.collapsed, levels=rev(freq.collapsed), 
                                 ordered=TRUE))

plot.freq.report.regime <- prodplot(df.freq.report.regime,
                                   ~ target.regime.type + Q4.8.collapsed, mosaic("h"), 
                                   colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Frequency of reporting to government, by regime type",
       subtitle="Q4.8: How often is your organization required to report to the government of  `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.freq.report.regime

freq.report.table <- df.freq.report.regime %>%
  xtabs(~ Q4.8.collapsed + target.regime.type, .)

analyze.cat.var(freq.report.table)


#' ### Government involvement
#' 
#' Members of the government aren't typically directly invovled in INGO work,
#' but when they are, it is more likely to occur with INGOs working in
#' autocracies.
df.involvement.regime <- survey.countries.clean %>%
  select(Q4.9, target.regime.type) %>%
  filter(Q4.9 != "Don't know") %>%
  mutate(Q4.9 = droplevels(Q4.9),
         Q4.9 = factor(Q4.9, levels=rev(levels(Q4.9))))

plot.involvement.regime <- prodplot(df.involvement.regime,
                                   ~ target.regime.type + Q4.9, mosaic("h"), 
                                   colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Government involvement, by regime type",
       subtitle="Q4.9: Are members of the government or ruling party of `target_country` involved in your work?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.involvement.regime

involvement.table <- survey.countries.clean %>%
  filter(Q4.9 != "Don't know") %>%
  mutate(Q4.9 = droplevels(Q4.9)) %>%
  xtabs(~ Q4.9 + target.regime.type, .)

analyze.cat.var(involvement.table)


#' ## Relationship with the government across regime type
#' 
#' ### Positivity
#' 
#' Another huge difference. INGOs working in autocracies have worse
#' relationships with their host governments.
#' 
df.govt.positivity.regime <- survey.countries.clean %>%
  select(Q4.11, target.regime.type) %>%
  filter(Q4.11 != "Don't know", Q4.11 != "Prefer not to answer") %>%
  mutate(Q4.11 = droplevels(Q4.11),
         Q4.11 = factor(Q4.11, levels=rev(levels(Q4.11))))

plot.govt.positivity.regime <- prodplot(df.govt.positivity.regime,
                                        ~ target.regime.type + Q4.11, mosaic("h"),
                                        colour=NA) +
  aes(fill=target.regime.type, colour="white") +
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Relationship with the government, by regime type",
       subtitle="Q4.11: How would you characterize your organization’s relationship with the government of `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=5
plot.govt.positivity.regime

govt.positivity.regime.table <- survey.countries.clean %>%
  filter(Q4.11 != "Don't know", Q4.11 != "Prefer not to answer") %>%
  mutate(Q4.11 = droplevels(Q4.11)) %>%
  xtabs(~ Q4.11 + target.regime.type, .)

analyze.cat.var(govt.positivity.regime.table)


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
