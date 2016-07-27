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
#'     self_contained: no
#'     includes:
#'       after_body: ../html/add_home_link.html
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
# 
# NB: xtabs() and productplots::prodplot(..., mosaic()) need to be mirror
# images of each other to get the same plot as vcd::mosaic()
# 
# Example:
#   prodplot(df, ~ x1 + x2 + x3, mosaic())
#   xtabs(~ x3 + x2 + x1)
analyze.cat.var <- function(cat.table) {
  cat.table.chi <- chisq.test(ftable(cat.table))
  
  cat("Table counts\n")
  print(cat.table)
  
  cat("\nExpected values\n")
  expected.values <- cat.table.chi$expected
  
  # Add nice labels if possible
  if(length(dim(cat.table)) == length(dim(expected.values))) {
    dimnames(expected.values) <- dimnames(cat.table)
  }
  
  expected.values %>% print(method="col.compact")
  
  cat("\nRow proporitions\n")
  print(prop.table(cat.table, margin=1))
  
  cat("\nColumn proporitions\n")
  print(prop.table(cat.table, margin=2))
  
  cat("\nChi-squared test for table\n")
  cat.table.chi %>% print()
  
  cat("Cramer's V\n")
  vcd::assocstats(ftable(cat.table))$cramer %>% print()
  
  cat("\nPearson residuals\n",
      "2 is used as critical value by convention\n", sep="")
  pearson.residuals <- cat.table.chi$residuals %>% print(method="col.compact")
  
  cat("\nComponents of chi-squared\n",
      "Critical value (0.05 with ", 
      cat.table.chi$parameter, " df) is ", 
      round(qchisq(0.95, cat.table.chi$parameter), 2), "\n", sep="")
  components <- pearson.residuals^2 %>% print(method="col.compact")
  
  cat("\np for components\n")
  round(1-pchisq(components, cat.table.chi$parameter), 3) %>% print(method="col.compact")
}

#' ## Organizational characteristics
#' 
#' How do respondents differ across the regime types of the countries they work
#' in and the issues they work on?
#' 
#' ### Distribution of NGOs
#' 
#' #### Regime type
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

#' ### Issues worked on across regime type
#' 
#' There are differences in potential contentiousness across regime type. In
#' democracies, a quarter of INGOs work on more threatening issues, but in
#' autocracies, nearly 40% do, which is a lot more than expected. Seen
#' differently, across types of contentiousness, 70% of INGOs working on low
#' contention issues work in democracies, in contrast to 58% of high contention
#' INGOs.
#' 
#' This is most likely because autocracies are more in need of high contention
#' issues like human rights advocacy, human trafficking, conflict prevention,
#' and freedom of expression protection.
#' 
df.issue.regime <- survey.countries.clean %>%
  select(target.regime.type, potential.contentiousness)

plot.issue.regime <- prodplot(df.issue.regime,
                              ~ target.regime.type + 
                                potential.contentiousness, mosaic("h")) + 
  aes(fill=target.regime.type, linetype=potential.contentiousness) + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  scale_linetype_manual(values=c("blank", "dashed")) +
  guides(fill=FALSE, linetype=FALSE) +
  labs(title="Potential issue contentiousness across regime types",
       subtitle="Issue area of INGO + regime type of target country") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=3
plot.issue.regime

issue.regime.table <- survey.countries.clean %>%
  xtabs(~ target.regime.type + potential.contentiousness, .)

analyze.cat.var(issue.regime.table)


#' ### Activities

# TODO: Staffing, collaboration, funding, etc.

# TODO: Figure out how to deal with org-level regime type analysis, since organizations work in multiple countries and answered only for one. Proportion of countries they work in that are autocracies?


#' ## Relationships with governments
#' 
#' For regime-based questions, this analysis is more straightforward, since
#' each country-organization response is limited to a single target country.
#' The questions also deal with the organization's specific actions in the
#' country, not what they do in all countries.
#' 
#' 
#' ### Time spent working in the country
#' 
#' #### Regime type
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


#' #### Potential contentiousness
#' 
#' NGOs working on low contention issues tend to have worked in their
#' respecitve target countries for a long time. High contention issue NGOs are
#' more than expected / more likely to work in their target countries for 4
#' years or less. This may be because there's a burst of more contentious INGOs
#' being allowed, or that more contentious INGOs get kicked out more regularly
#' and can only stay in country for so long.
#' 
df.time.country.issue <- survey.countries.clean %>%
  select(Q4.2, potential.contentiousness) %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = droplevels(Q4.2),
         Q4.2 = factor(Q4.2, levels=rev(levels(Q4.2))))

plot.time.country.issue <- prodplot(df.time.country.issue,
                                     ~ potential.contentiousness + Q4.2, mosaic("h"), 
                                     colour=NA) + 
  aes(fill=potential.contentiousness, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Length of time in country, by potential contentiousness",
       subtitle="Q4.2: How long has your organization worked in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.time.country.issue

time.country.table.issue <- survey.countries.clean %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = droplevels(Q4.2)) %>%
  xtabs(~ Q4.2 + potential.contentiousness, .)

analyze.cat.var(time.country.table.issue)


#' #### Regime type + contentiousness
#' 
#' When accounting for both regime type and main issue area, an interesting
#' story emerges. Individually, I found that INGOs working in their target
#' countries for 5+ years were most likely to work on non-contentious issues
#' and work in democracies. Relatively few of the long-term INGOs work in
#' either autocracies or on more contentious issues. This remains the case when
#' accounting for both target-country regime type and main issue area. There
#' are fewer low contention, long-term INGOs working in autocracies than
#' expected and more low contention, long term INGOs working in democracies.
#' 
df.time.country.issue.regime <- survey.countries.clean %>%
  select(Q4.2, potential.contentiousness, target.regime.type) %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = droplevels(Q4.2),
         Q4.2 = factor(Q4.2, levels=rev(levels(Q4.2))))

plot.time.country.issue.regime <- prodplot(df.time.country.issue.regime,
                                    ~ target.regime.type + potential.contentiousness +
                                      Q4.2, mosaic("v")) + 
  aes(fill=target.regime.type, linetype=potential.contentiousness) + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  scale_linetype_manual(values=c("blank", "dashed")) +
  guides(fill=FALSE, linetype=FALSE) +
  labs(title="Length of time in country",
       subtitle="Issue area of INGO + regime type of target country") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.time.country.issue.regime

time.country.table.issue.regime <- survey.countries.clean %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = droplevels(Q4.2)) %>%
  xtabs(~ Q4.2 + potential.contentiousness + target.regime.type, .)

analyze.cat.var(time.country.table.issue.regime)


#' ### How NGOs operate in country
#' 
#' #### Regime type
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


#' #### Potential contentiousness
#' 
#' There's an overall significant difference in frequencies, driven primarily
#' by more high contention INGOs working with foreigners. The individual cell
#' effects, however, aren't particularly significant. In general, more INGOs
#' than expected use foreigners in their target countries).
#' 
df.operations.issue <- survey.countries.clean %>%
  unnest(Q4.3_value) %>%
  select(Q4.3_value, potential.contentiousness) %>%
  filter(!is.na(Q4.3_value), Q4.3_value != "Don't know") %>%
  mutate(Q4.3 = factor(Q4.3_value, levels=what.do, 
                       labels=what.do.short, ordered=TRUE))

plot.operations.issue <- prodplot(df.operations.issue,
                                  ~ potential.contentiousness + Q4.3, mosaic("h"), 
                                  colour=NA) + 
  aes(fill=potential.contentiousness, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="How INGOs work in the target country, by issue",
       subtitle="Q4.3: What does your organization do in `target_country`?\n(multiple answers allowed)") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.operations.issue

operations.table.issue <- survey.countries.clean %>%
  unnest(Q4.3_value) %>%
  filter(Q4.3_value != "Don't know") %>%
  xtabs(~ Q4.3_value + potential.contentiousness, .)

analyze.cat.var(operations.table.issue)


#' ### Registration
#' 
#' #### Regime type
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


#' #### Potential contentiousness
#' 
#' There is no significant difference in the registration status of low and
#' high contentious INGOs.
#' 
df.registered.issue <- survey.countries.clean %>%
  select(Q4.4, potential.contentiousness) %>%
  filter(Q4.4 != "Don't know") %>%
  mutate(Q4.4 = droplevels(Q4.4),
         Q4.4 = factor(Q4.4, levels=rev(levels(Q4.4))))

plot.registered.issue <- prodplot(df.registered.issue,
                                  ~ potential.contentiousness + Q4.4, mosaic("h"), 
                                  colour=NA) + 
  aes(fill=potential.contentiousness, colour="white") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Registration status, by issue",
       subtitle="Q4.4: Is your organization registered with the national government in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.registered.issue

registered.table.issue <- survey.countries.clean %>%
  filter(Q4.4 != "Don't know") %>%
  mutate(Q4.4 = droplevels(Q4.4)) %>%
  xtabs(~ Q4.4 + potential.contentiousness, .)

analyze.cat.var(registered.table.issue)



#' ## Contact with government
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


#' ### Which kind of officials, across regime type
#' 
#' TODO: Do this
#' 


#' ## Relationship with the government
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


#' ## NGO regulations and restrictions
#' 
#' ### Familiarity
#' 
#' Most INGOs working in democracies are either moderately familiar with
#' government regulations or not familiar at all. For INGOs working in
#' autocracies, familiarity with regulations appears to be more essential—most
#' are very or extremely familiar with regulations, and very few are unaware of
#' any of the laws governing their activities. The difference in proporitions
#' across groups is significant.
df.reg.familiarity.regime <- survey.countries.clean %>%
  select(Q4.13, target.regime.type) %>%
  filter(!is.na(Q4.13))

plot.reg.familiarity.regime <- prodplot(df.reg.familiarity.regime,
                                        ~ target.regime.type + Q4.13, mosaic("h"),
                                        colour=NA) +
  aes(fill=target.regime.type, colour="white") +
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Familiarity with regulations, by regime type",
       subtitle="Q4.13: How familiar is your organization with regulations for\ninternational nongovernmental organizations (NGOs) in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.reg.familiarity.regime

reg.familiarity.regime.table <- survey.countries.clean %>%
  xtabs(~ Q4.13 + target.regime.type, .)

analyze.cat.var(reg.familiarity.regime.table)


#' ### Frequency of change
#' 
#' Most NGOs don't know, and I didn't include an "other" category here. The
#' univariate distribution has a clear-ish trend, with most reporting "Rarely"
#' (and only a few "Never"). Ignoring "Never" and "Once a month" in the
#' bivariate analysis because of low cell counts, there's also a trend by
#' regime type—more INGOs working in autocracies see annual changes in
#' regulations, possibly reflecting a more volatile regulatory environment.
df.reg.change.regime <- survey.countries.clean %>%
  select(Q4.14, target.regime.type) %>%
  filter(!is.na(Q4.14))

plot.reg.change.regime <- prodplot(df.reg.change.regime,
                                        ~ target.regime.type + Q4.14, mosaic("h"),
                                        colour=NA) +
  aes(fill=target.regime.type, colour="white") +
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="Frequency of changes, by regime type",
       subtitle="Q4.14: How often do regulations for international NGOs in `target_country` change?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.reg.change.regime

reg.change.regime.table <- survey.countries.clean %>%
  xtabs(~ Q4.14 + target.regime.type, .)

analyze.cat.var(reg.change.regime.table)


#' ### How do they find out about changes?
#' 
#' INGOs working in different regimes hear about changes somewhat differently 
#' as well. Those working in autocracies are more likely to hear about changes 
#' in regulations directly from government officials, while those in 
#' demcoracies are way less likely to do so. Other NGOs are the most common 
#' source for both regime types, and all other categories are in the same order
#' and roughly the same proportion. This makes sense since INGOs already have
#' more regular contact with government officials in autocracies.
df.change.how.regime <- survey.countries.clean %>%
  unnest(Q4.15_value) %>%
  select(Q4.15 = Q4.15_value, target.regime.type) %>%
  filter(Q4.15 != "Don't know") %>%
  mutate(Q4.15 = factor(Q4.15))

levels(df.change.how.regime$Q4.15)[levels(df.change.how.regime$Q4.15) == "Newspapers, television, and other media"] <-
  "Newspapers, television,\nand other media"

plot.change.how.regime <- prodplot(df.change.how.regime,
                                   ~ target.regime.type + Q4.15, mosaic("h"),
                                   colour=NA) +
  aes(fill=target.regime.type, colour="white") +
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="How NGOs find out, by regime type",
       subtitle="Q4.15: How does your organization find out about changes to\nNGO regulations in `target_country`? (multiple answers allowed)") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.change.how.regime

plot.df.change.how.regime <- df.change.how.regime %>%
  group_by(target.regime.type, Q4.15) %>%
  summarise(num = n()) %>%
  mutate(perc = num / sum(num)) %>%
  arrange(perc) %>%
  ungroup() %>%
  mutate(Q4.15 = factor(Q4.15, levels=unique(Q4.15), ordered=TRUE))

plot.change.how.regime.bar <- ggplot(plot.df.change.how.regime, 
                                     aes(x=perc, y=Q4.15, 
                                         fill=target.regime.type)) +
  geom_barh(stat="identity") + 
  scale_fill_manual(values=c("grey80", "grey40")) +
  scale_x_continuous(labels=percent) +
  labs(x=NULL, y=NULL, title="Percentages by regime type") + 
  guides(fill=FALSE) +
  theme_ath() +
  facet_wrap(~ target.regime.type)

#+ fig.width=6, fig.height=4
plot.change.how.regime.bar

change.how.table <- survey.countries.clean %>%
  unnest(Q4.15_value) %>%
  filter(Q4.15_value != "Don't know") %>%
  xtabs(~ Q4.15_value + target.regime.type, .)

analyze.cat.var(change.how.table)


#' ### Effect of regulations
#' 
#' TODO: Do this


#' ### Effect of regulations in general
#' 
#' !!! It works !!!
#' 
#' INGOs that are restricted tend to work in autocracies and there's almost a perfect trend.
df.reg.effect.general.regime <- survey.countries.clean %>%
  select(Q4.17, target.regime.type) %>%
  filter(Q4.17 != "Don’t know") %>%
  mutate(Q4.17 = droplevels(Q4.17),
         Q4.17 = factor(Q4.17, levels=rev(levels(Q4.17))))

plot.reg.effect.general.regime <- prodplot(df.reg.effect.general.regime,
                                   ~ target.regime.type + Q4.17, mosaic("h"),
                                   colour=NA) +
  aes(fill=target.regime.type, colour="white") +
  scale_fill_manual(values=c("grey80", "grey40")) +
  guides(fill=FALSE) +
  labs(title="General restrictions, by regime type",
       subtitle="Q4.17: Overall, how is your organization's work affected by government regulations in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.reg.effect.general.regime

reg.effect.general.regime.table <- survey.countries.clean %>%
  xtabs(~ Q4.17 + target.regime.type, .)

analyze.cat.var(reg.effect.general.regime.table)


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
