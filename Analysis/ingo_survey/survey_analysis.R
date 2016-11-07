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
knitr::opts_chunk$set(cache=TRUE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

library(plyr)  # Because of productplots
library(tidyverse)
library(forcats)
library(stringr)
library(productplots)
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
  scale_linetype_manual(values=c("blank", "dashed")) +
  guides(fill=FALSE, linetype=FALSE) +
  labs(title="Potential issue contentiousness across regime types",
       subtitle="Issue area of INGO + regime type of target country") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

plot.issue.regime <- prodplot(df.issue.regime,
                              ~ target.regime.type + 
                                potential.contentiousness, mosaic("h"), 
                                     colour=NA) + 
  aes(fill=target.regime.type, linetype=potential.contentiousness) + 
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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

issue.regime.table.bayes <- survey.countries.clean %>%
  count(target.regime.type, potential.contentiousness) %>%
  group_by(target.regime.type) %>%
  mutate(total = sum(n)) %>%
  filter(potential.contentiousness == "High contention") %>%
  prop.test.bayes(., as.formula(cbind(n, total) ~ target.regime.type))

#+ fig.width=6, fig.height=3
grid.arrange(issue.regime.table.bayes$plot.groups,
             issue.regime.table.bayes$plot.diffs)

#+ results="asis"
pandoc.table(issue.regime.table.bayes$samples.summary)

#+ results="asis"
pandoc.table(issue.regime.table.bayes$diffs.summary)

#+ results="markup"
analyze.cat.var(issue.regime.table)


#' ## H~1~: Instrumental concerns
#' 
#' What is the relationship beween feelings of restriction and instrumental
#' concerns? How do respondents working in different regime types and on
#' different issues differ in the distribution of their instrumental
#' characteristics? Do those differences help drive restrictions?
#' 
#' Things to check against relationship with government (Q4.11), types of
#' regulation (Q4.16), overall level of restriction (Q4.17), changes in
#' programming (Q4.19), type of changes (Q4.21), and attempts at changing
#' regulations (Q4.23):
#' 
#' - Staffing (employees and volunteers)
#' - Collaboration (which kinds of institutions do they collaborate with +
#' number of different types of collaborative relationships (i.e. collaboration
#' only with governemnts vs. governments + IGOs + NGOs + businesses))
#' - Sources and mix of funding
#' - Time working in country
#' 
#' ### Employees
#' 
#' #### Relationship with government (Q4.11)
#' 
df.employees.relationship <- survey.clean.all %>%
  select(Q3.4.num, Q4.11, potential.contentiousness) %>%
  filter(!(Q4.11 %in% c("Don't know", "Prefer not to answer"))) %>%
  filter(!is.na(Q4.11)) %>%
  mutate(Q4.11 = droplevels(Q4.11),
         Q4.11 = factor(Q4.11, levels=rev(levels(Q4.11))))

df.employees.relationship.plot.means <- df.employees.relationship %>%
  group_by(Q4.11, potential.contentiousness) %>%
  summarise(average = mean(Q3.4.num, na.rm=TRUE),
            med = median(Q3.4.num, na.rm=TRUE),
            num = n())

#+ fig.width=6, fig.height=3
ggplot(df.employees.relationship, aes(y=Q4.11, x=Q3.4.num)) + 
  geom_violinh(na.rm=TRUE) +
  geom_point(alpha=0.2, size=0.5) +
  geom_point(data=df.employees.relationship.plot.means,
             aes(x=med, y=Q4.11)) +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:5)), labels=comma) + 
  labs(x="Number of employees", y=NULL,
       title="Relationship with government and # of employees") +
  theme_ath() + facet_wrap(~ potential.contentiousness)

#+ fig.width=6, fig.height=3
ggplot(df.employees.relationship.plot.means, 
       aes(x=med, y=Q4.11, fill=potential.contentiousness)) +
  geom_barh(stat="identity", position=position_dodgev()) + 
  scale_x_continuous(expand=c(0, 0)) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  labs(x="Median number of employees", y=NULL, 
       title="Median # of employees and relationship with government") +
  theme_ath()

#' #### Types of regulation (Q4.16)
#' 
df.employees.reg.types <- survey.clean.all %>%
  select(Q3.4.num, starts_with("Q4.16"), -dplyr::contains("TEXT"), 
         potential.contentiousness) %>%
  gather(regulation, response, starts_with("Q4.16")) %>%
  mutate(regulation = gsub("Q4\\.16_", "", regulation)) %>%
  filter(!(response %in% c("Don't know", "Not applicable"))) %>%
  filter(!is.na(response)) %>%
  mutate(response = factor(response, levels=great.none, ordered=TRUE))

df.employees.reg.types.plot.means <- df.employees.reg.types %>%
  group_by(regulation, response, potential.contentiousness) %>%
  summarise(average = mean(Q3.4.num, na.rm=TRUE),
            med = median(Q3.4.num, na.rm=TRUE),
            num = n())

#+ fig.width=9, fig.height=4
ggplot(df.employees.reg.types, aes(y=response, x=Q3.4.num, 
                                   fill=potential.contentiousness)) + 
  geom_violinh(na.rm=TRUE) + 
  geom_point(alpha=0.2, size=0.5) +
  geom_point(data=df.employees.reg.types.plot.means,
             aes(x=med, y=response)) +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:5)), labels=comma) + 
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  labs(x="Number of employees", y=NULL,
       title="Types of regulation, perceptions of restriction, and # of employees (logged)") +
  theme_ath() + 
  facet_wrap(~ regulation + potential.contentiousness, nrow=2)

#+ fig.width=6, fig.height=3
ggplot(df.employees.reg.types.plot.means, 
       aes(x=med, y=response, fill=potential.contentiousness)) +
  geom_barh(stat="identity", position=position_dodgev()) + 
  scale_x_continuous(expand=c(0, 0)) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  labs(x="Median number of employees", y=NULL, 
       title="Median # of employees and types of regulation") +
  theme_ath() + facet_wrap(~ regulation)

#' #### Overall perception of restriction (Q4.17)
#' 
df.employees.restriction <- survey.clean.all %>%
  select(Q3.4.num, Q4.17, potential.contentiousness) %>%
  filter(Q4.17 != "Don’t know") %>%
  mutate(Q4.17 = droplevels(Q4.17),
         Q4.17 = factor(Q4.17, levels=rev(levels(Q4.17))))

df.employees.restrictions.plot.means <- df.employees.restriction %>%
  group_by(Q4.17, potential.contentiousness) %>%
  summarise(average = mean(Q3.4.num, na.rm=TRUE),
            med = median(Q3.4.num, na.rm=TRUE),
            num = n())

#+ fig.width=6, fig.height=3
ggplot(df.employees.restriction, aes(y=Q4.17, x=Q3.4.num)) + 
  geom_violinh(na.rm=TRUE) +
  geom_point(alpha=0.2, size=0.5) +
  geom_point(data=df.employees.restrictions.plot.means,
             aes(x=med, y=Q4.17)) +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:5)), labels=comma) + 
  labs(x="Number of employees", y=NULL,
       title="Perceptions of restriction and # of employees") +
  theme_ath() + facet_wrap(~ potential.contentiousness)

#+ fig.width=6, fig.height=3
ggplot(df.employees.restrictions.plot.means, 
       aes(x=med, y=Q4.17, fill=potential.contentiousness)) +
  geom_barh(stat="identity", position=position_dodgev()) + 
  scale_x_continuous(expand=c(0, 0)) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  labs(x="Median number of employees", y=NULL, 
       title="Median # of employees and perceptions of restriction") +
  theme_ath()

#' #### Changes in programming (Q4.19)
#' 
df.employees.changes <- survey.clean.all %>%
  select(Q3.4.num, Q4.19, potential.contentiousness) %>%
  filter(Q4.19 != "Don't know") %>%
  mutate(Q4.19 = droplevels(Q4.19),
         Q4.19 = factor(Q4.19, levels=rev(levels(Q4.19))))

df.employees.changes.plot.means <- df.employees.changes %>%
  group_by(Q4.19, potential.contentiousness) %>%
  summarise(average = mean(Q3.4.num, na.rm=TRUE),
            med = median(Q3.4.num, na.rm=TRUE),
            num = n())

#+ fig.width=6, fig.height=3
ggplot(df.employees.changes, aes(y=Q4.19, x=Q3.4.num)) + 
  geom_violinh(na.rm=TRUE) +
  geom_point(alpha=0.2, size=0.5) +
  geom_point(data=df.employees.changes.plot.means,
             aes(x=med, y=Q4.19)) +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:5)), labels=comma) + 
  labs(x="Number of employees", y=NULL,
       title="Changes in programming and # of employees") +
  theme_ath() + facet_wrap(~ potential.contentiousness)

#+ fig.width=6, fig.height=3
ggplot(df.employees.changes.plot.means, 
       aes(x=med, y=Q4.19, fill=potential.contentiousness)) +
  geom_barh(stat="identity", position=position_dodgev()) + 
  scale_x_continuous(expand=c(0, 0)) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  labs(x="Median number of employees", y=NULL, 
       title="Median # of employees and changes in programming") +
  theme_ath()

#' #### Attempts to change programming (Q4.23)
#' 
df.employees.change.attempt <- survey.clean.all %>%
  select(Q3.4.num, Q4.23, potential.contentiousness) %>%
  filter(Q4.23 != "Don't know") %>%
  mutate(Q4.23 = droplevels(Q4.23),
         Q4.23 = factor(Q4.23, levels=rev(levels(Q4.23))))

df.employees.change.attempt.plot.means <- df.employees.change.attempt %>%
  group_by(Q4.23, potential.contentiousness) %>%
  summarise(average = mean(Q3.4.num, na.rm=TRUE),
            med = median(Q3.4.num, na.rm=TRUE),
            num = n())

#+ fig.width=6, fig.height=3
ggplot(df.employees.change.attempt, aes(y=Q4.23, x=Q3.4.num)) + 
  geom_violinh(na.rm=TRUE) +
  geom_point(alpha=0.2, size=0.5) +
  geom_point(data=df.employees.change.attempt.plot.means,
             aes(x=med, y=Q4.23)) +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:5)), labels=comma) + 
  labs(x="Number of employees", y=NULL,
       title="Changes in programming and # of employees") +
  theme_ath() + facet_wrap(~ potential.contentiousness)

#+ fig.width=6, fig.height=3
ggplot(df.employees.change.attempt.plot.means, 
       aes(x=med, y=Q4.23, fill=potential.contentiousness)) +
  geom_barh(stat="identity", position=position_dodgev()) + 
  scale_x_continuous(expand=c(0, 0)) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  labs(x="Median number of employees", y=NULL, 
       title="Median # of employees and changes in programming") +
  theme_ath()

#' ### Volunteers
#' 
#' #### Overall perception of restriction (Q4.17)
#' 
df.volunteers.restriction <- survey.clean.all %>%
  select(Q3.5.num, Q4.17, potential.contentiousness) %>%
  filter(Q4.17 != "Don’t know") %>%
  mutate(Q4.17 = droplevels(Q4.17),
         Q4.17 = factor(Q4.17, levels=rev(levels(Q4.17))))

df.volunteers.restrictions.plot.means <- df.volunteers.restriction %>%
  group_by(Q4.17, potential.contentiousness) %>%
  summarise(average = mean(Q3.5.num, na.rm=TRUE),
            med = median(Q3.5.num, na.rm=TRUE),
            num = n())

#+ fig.width=6, fig.height=3
ggplot(df.volunteers.restriction, aes(y=Q4.17, x=Q3.5.num)) + 
  geom_violinh(na.rm=TRUE) +
  geom_point(alpha=0.2, size=0.5) +
  geom_point(data=df.volunteers.restrictions.plot.means,
             aes(x=med, y=Q4.17)) +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:5)), labels=comma) + 
  labs(x="Number of volunteers", y=NULL,
       title="Perceptions of restriction and # of volunteers") +
  theme_ath() + facet_wrap(~ potential.contentiousness)

#+ fig.width=6, fig.height=3
ggplot(df.volunteers.restrictions.plot.means, 
       aes(x=med, y=Q4.17, fill=potential.contentiousness)) +
  geom_barh(stat="identity", position=position_dodgev()) + 
  scale_x_continuous(expand=c(0, 0)) +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  labs(x="Median number of volunteers", y=NULL, 
       title="Median # of volunteers and perceptions of restriction") +
  theme_ath()


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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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

issue.regime.table <- survey.countries.clean %>%
  xtabs(~ target.regime.type + potential.contentiousness, .)

time.country.table.bayes <- survey.countries.clean %>%
  filter(Q4.2 != "Don't know") %>%
  mutate(Q4.2 = fct_drop(Q4.2)) %>%
  count(Q4.2, target.regime.type) %>%
  group_by(Q4.2) %>%
  mutate(total = sum(n)) %>%
  filter(target.regime.type == "Autocracy") %>%
  prop.test.bayes(., as.formula(cbind(n, total) ~ Q4.2))

#+ fig.width=6, fig.height=5
grid.arrange(time.country.table.bayes$plot.groups,
             time.country.table.bayes$plot.diffs)

#+ results="asis"
pandoc.table(time.country.table.bayes$samples.summary)

#+ results="asis"
pandoc.table(time.country.table.bayes$diffs.summary)

#+ results="markup"
analyze.cat.var(issue.regime.table)


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
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("regime")) +
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
freq.change.collapsed <- c("Once a year+", "Once every few years", 
                           "Rarely or never", "Don't know")

df.freq.change <- survey.countries.clean %>%
  filter(!is.na(Q4.14)) %>%
  mutate(Q4.14.collapsed = case_when(
    .$Q4.14 == "Once a month" ~ freq.change.collapsed[1],
    .$Q4.14 == "Once a year" ~ freq.change.collapsed[1],
    .$Q4.14 == "Once every few years" ~ freq.change.collapsed[2], 
    .$Q4.14 == "Rarely" ~ freq.change.collapsed[3],
    .$Q4.14 == "Never" ~ freq.change.collapsed[3],
    .$Q4.14 == "Don't know" ~ freq.change.collapsed[4],
    TRUE ~ NA_character_)) %>%
  filter(!is.na(Q4.14.collapsed)) %>%
  mutate(Q4.14.collapsed = factor(Q4.14.collapsed, levels=rev(freq.change.collapsed), 
                                  ordered=TRUE))

#' #### Regime type
#' 
#' Most NGOs don't know, and I didn't include an "other" category here. The 
#' univariate distribution has a clear trend, with most reporting "Rarely" (and
#' only a few "Never"; "Never" and "Once a month" are collapsed because of low 
#' expected values). There's also a trend by regime type—more INGOs working in
#' autocracies see annual changes in regulations, possibly reflecting a more
#' volatile regulatory environment.
plot.reg.change.regime <- prodplot(df.freq.change,
                                        ~ target.regime.type + Q4.14.collapsed, mosaic("h"),
                                        colour=NA) +
  aes(fill=target.regime.type, colour="white") +
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Frequency of changes, by regime type",
       subtitle="Q4.14: How often do regulations for international NGOs in `target_country` change?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.reg.change.regime

reg.change.regime.table <- df.freq.change %>%
  xtabs(~ Q4.14.collapsed + target.regime.type, .)

analyze.cat.var(reg.change.regime.table)

#' #### Potential contentiousness
#' 
#' There's no difference in the frequency of changes across issue areas, which
#' is to be expected. Actual legal restrictions are a blunt instrument and
#' don't really target specific sectors of INGOs.
plot.reg.change.issue <- prodplot(df.freq.change,
                                  ~ potential.contentiousness + Q4.14.collapsed, mosaic("h"),
                                  colour=NA) +
  aes(fill=potential.contentiousness, colour="white") +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Frequency of changes, by issue",
       subtitle="Q4.14: How often do regulations for international NGOs in `target_country` change?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.reg.change.issue

reg.change.issue.table <- df.freq.change %>%
  xtabs(~ Q4.14.collapsed + potential.contentiousness, .)

analyze.cat.var(reg.change.issue.table)



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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
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
#' 


#' ### Effect of regulations in general
#' 
#' #### Regime type
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
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="General restrictions, by regime type",
       subtitle="Q4.17: Overall, how is your organization's work affected by government regulations\nin `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.reg.effect.general.regime

fig.save.cairo(plot.reg.effect.general.regime + theme_ath(10), 
               filename="3-restrictions-regime-type", 
               width=6, height=4)

reg.effect.general.regime.table <- survey.countries.clean %>%
  xtabs(~ Q4.17 + target.regime.type, .)

analyze.cat.var(reg.effect.general.regime.table)


#' #### Potential contentiousness
#' 
#' Differences in general restrictions by issue are less pronounced. There is a
#' significant overall difference, driven primarily by high contention INGOs
#' that report feeling extremely restricted. All levels of restriction are
#' relatively even, following a 65-35 split between low and high contention
#' INGOs.
#' 
df.reg.effect.general.issue <- survey.countries.clean %>%
  select(Q4.17, potential.contentiousness) %>%
  filter(Q4.17 != "Don’t know") %>%
  mutate(Q4.17 = droplevels(Q4.17),
         Q4.17 = factor(Q4.17, levels=rev(levels(Q4.17))))

plot.reg.effect.general.issue <- prodplot(df.reg.effect.general.issue,
                                          ~ potential.contentiousness + Q4.17, mosaic("h"),
                                          colour=NA) +
  aes(fill=potential.contentiousness, colour="white") +
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="General restrictions, by issue",
       subtitle="Q4.17: Overall, how is your organization's work affected by government regulations\nin `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=4
plot.reg.effect.general.issue

fig.save.cairo(plot.reg.effect.general.issue + theme_ath(10), 
               filename="3-restrictions-issue", 
               width=6, height=4)

reg.effect.general.issue.table <- survey.countries.clean %>%
  xtabs(~ Q4.17 + potential.contentiousness, .)

analyze.cat.var(reg.effect.general.issue.table)


#' #### Regime type + contentiousness
#' 
#' There are more unrestricted low contentious NGOs working in democracies than
#' expected and far fewer unrestricted low contentious NGOs in autocracies.
#' Additionally, there are fewer highly restricted INGOs in democracies
#' (especially high contentious ones), and far more highly restricted, highly
#' contentious INGOs working in autocracies than expected.
#' 

# Collapse very and extremely restriced because of low expected frequencies
restricted <- c("Not restricted at all", "Slightly restricted",
                "Moderately restricted", "Very or extremely restricted")

df.reg.effect.general.issue.regime <- survey.countries.clean %>%
  select(Q4.17, potential.contentiousness, target.regime.type) %>%
  filter(Q4.17 != "Don’t know") %>%
  mutate(Q4.17 = recode_factor(Q4.17, `Very restricted` = restricted[4],
                        `Extremely restricted` = restricted[4]),
         Q4.17 = factor(Q4.17, levels=restricted, ordered=TRUE))

plot.reg.effect.general.issue.regime <- prodplot(df.reg.effect.general.issue.regime,
                                          ~ target.regime.type + potential.contentiousness +
                                            Q4.17, mosaic("v")) +
  aes(fill=target.regime.type, linetype=potential.contentiousness) + 
  scale_fill_manual(values=ath.palette("regime")) +
  scale_linetype_manual(values=c("blank", "dashed")) +
  guides(fill=FALSE, linetype=FALSE) +
  labs(title="General restrictions, by issue and regime type",
       subtitle="Q4.17: Overall, how is your organization's work affected by government regulations in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=5
plot.reg.effect.general.issue.regime

reg.effect.general.regime.issue.table <- df.reg.effect.general.issue.regime %>%
  xtabs(~ Q4.17 + potential.contentiousness + target.regime.type, .)

analyze.cat.var(reg.effect.general.regime.issue.table)


#' ## Responses to regulations
#' 
#' ### Changes in programming
#' 
#' #### Regime type
#' 
#' Slightly more NGOs that work in autocracies have changed their programming
#' in the past decade, but not in a statistically significant way. In general,
#' most INGOs have not changed much, and the split is proportional across
#' regime type.
#' 
df.change.programming.regime <- survey.countries.clean %>%
  filter(!is.na(Q4.19)) %>%
  filter(Q4.19 != "Don't know") %>%
  mutate(Q4.19 = droplevels(Q4.19),
         Q4.19 = factor(Q4.19, levels=rev(levels(Q4.19)))) %>%
  select(Q4.19, target.regime.type)

plot.change.programming.regime <- prodplot(df.change.programming.regime,
                                    ~ target.regime.type + Q4.19, mosaic("h"), 
                                    colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Change in programming, by regime type",
       subtitle="Q4.19: Over the last 10 years, has your organization changed its mix of programming in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.change.programming.regime

change.programming.table <- df.change.programming.regime %>%
  xtabs(~ Q4.19 + target.regime.type, .)

analyze.cat.var(change.programming.table)


#' #### Potential contentiousness
#' 
#' Slightly fewer INGOs working on highly contentious issues have changed their
#' programming over the past decade, but again, the difference between expected
#' and actual is not significant.
#' 
df.change.programming.issue <- survey.countries.clean %>%
  filter(!is.na(Q4.19)) %>%
  filter(Q4.19 != "Don't know") %>%
  mutate(Q4.19 = droplevels(Q4.19),
         Q4.19 = factor(Q4.19, levels=rev(levels(Q4.19)))) %>%
  select(Q4.19, potential.contentiousness)

plot.change.programming.issue <- prodplot(df.change.programming.issue,
                                   ~ potential.contentiousness + Q4.19, mosaic("h"), 
                                   colour=NA) + 
  aes(fill=potential.contentiousness, colour="white") + 
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Change in programming, by issue",
       subtitle="Q4.19: Over the last 10 years, has your organization changed its mix of programming in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.change.programming.issue

change.programming.issue.table <- df.change.programming.issue %>%
  xtabs(~ Q4.19 + potential.contentiousness, .)

analyze.cat.var(change.programming.issue.table)


#' ### Changes in response to regulations
#' 
#' #### Regime type
labels.changes <- data_frame(levels=c("funding", "issues", "comm_govt", 
                                      "comm_donors", "locations", "country_office",
                                      "local_staff", "foreign_staff"),
                             labels=c("Changed sources of funding",
                                      "Changed issues worked on",
                                      "Changed communication with the government",
                                      "Changed communication with donors",
                                      "Changed locations worked in",
                                      "Changed location country office",
                                      "Used more local staff and/or volunteers",
                                      "Used more foreign staff and/or volunteers"))

df.changes.response <- survey.countries.clean %>%
  select(dplyr::contains("Q4.21"), -dplyr::contains("TEXT"),
         target.regime.type, potential.contentiousness) %>%
  gather(question, response, -c(target.regime.type, potential.contentiousness)) %>%
  mutate(question = str_replace(question, "Q4\\.21_", ""),
         question = factor(question, levels=labels.changes$levels,
                           labels=labels.changes$labels, ordered=TRUE)) %>%
  filter(!(response %in% c("Don't know", "Not applicable")))


show.output <- function(chunk) {
  current.question <- as.character(chunk$question[1])
  cat("\n")
  cat(paste0(rep("-", nchar(current.question) + 2), collapse=""))
  cat(paste0("\n", current.question, "\n"))
  cat(paste0(rep("-", nchar(current.question) + 2), collapse=""))
  cat("\n")
  
  regime.table <- chunk %>%
    xtabs(~ response + target.regime.type, .)
  
  issue.table <- chunk %>%
    xtabs(~ response + potential.contentiousness, .)
  
  # Use diff for the two column effect!
  # http://stackoverflow.com/a/9214177/120898
  tmp.regime <- tempfile()
  tmp.issue <- tempfile()
  
  cat(capture.output( analyze.cat.var(regime.table) ), sep="\n", file=tmp.regime)
  cat(capture.output( analyze.cat.var(issue.table) ), sep="\n", file=tmp.issue)
 
  # system does weeeeird stuff with knitr (see https://github.com/yihui/knitr/issues/1203),
  # so system(..., intern=TRUE) + cat(paste(..., collapse="\n)) does the trick
  system.output <- system(sprintf("diff -y -W 140 %s %s", tmp.regime, tmp.issue), intern=TRUE)
  
  cat(paste(system.output, collapse="\n"))
}

show.plots <- function(chunk) {
  current.question <- as.character(chunk$question[1])
  
  plot.regime <- prodplot(chunk,
                          ~ target.regime.type + response, mosaic("h"),
                          colour=NA) + 
    aes(fill=target.regime.type, colour="white") + 
    scale_fill_manual(values=ath.palette("regime"), name=NULL) +
    guides(fill=FALSE) +
    labs(title=current.question) +
    theme_ath() + theme(axis.title=element_blank(),
                        panel.grid=element_blank())
  
  plot.issue <- prodplot(chunk,
                         ~ potential.contentiousness + response, mosaic("h"),
                         colour=NA) + 
    aes(fill=potential.contentiousness, colour="white") + 
    scale_fill_manual(values=ath.palette("contention"), name=NULL) +
    guides(fill=FALSE) +
    labs(title=" ") +
    theme_ath() + theme(axis.title=element_blank(),
                        panel.grid=element_blank())

  # Can't do this because knitr chokes...
  # plot.both <- arrangeGrob(plot.regime, plot.issue, nrow=1)
  # grid::grid.draw(plot.both)
  
  grid.arrange(plot.regime, plot.issue, nrow=1)
}

# show.output(chunk)
# 


# This could be done with purrr, but I can't get it to work right (the output
# and plots go in two huge chunks instead of alternating between text and
# plots; if I put show.plots() in show.output(), only one plot shows up). So
# instead of using fancy R vectorization, yay loops. \(•◡•)/
#
# Something like this with purrr almost works:
# suppressWarnings(response.to.regulations <- df.changes.response %>%
#   split(.$question) %>%
#   map(~ show.output(.))) %>%
#   map(~ show.plots(.))

#+ fig.width=7, fig.height=2
for (Q in unique(df.changes.response$question)) {
  chunk <- filter(df.changes.response, question == Q)
  
  show.plots(chunk)
  suppressWarnings(show.output(chunk))
  cat("\n")
}


#' ### Discussions with government
#' 
#' #### Regime type
#' 
#' INGOs working in autocracies are far more likely to have discussed NGO
#' regulations with their host governments—the majority actually have (54% of
#' those responding "yes" work in autocracies; 32% of those working in
#' autocracies responded "yes"), and the difference is statistically
#' significant. This is likely because INGOs working in autocracies have more
#' of a reason to discuss the regulations, as they see more regulations in
#' general.
df.discuss.regime <- survey.countries.clean %>%
  filter(!is.na(Q4.22)) %>%
  filter(Q4.22 != "Don't know") %>%
  mutate(Q4.22 = droplevels(Q4.22),
         Q4.22 = factor(Q4.22, levels=rev(levels(Q4.22)))) %>%
  select(Q4.22, target.regime.type)

plot.discuss.regime <- prodplot(df.discuss.regime,
                                    ~ target.regime.type + Q4.22, mosaic("h"), 
                                    colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Discussions about regulations, by regime type",
       subtitle="Q4.22: Has your organization discussed NGO regulations with government officials in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.discuss.regime

discuss.table <- df.discuss.regime %>%
  xtabs(~ Q4.22 + target.regime.type, .)

analyze.cat.var(discuss.table)


#' #### Potential contentiousness
#' 
#' There's no difference across issue areas in whether or not an INGO discusses
#' regulations with the government—the same proportion (66% haven't discussed;
#' 20% have; 10% don't know) holds across both levels of potential
#' contentiousness.
df.discuss.issue <- survey.countries.clean %>%
  filter(!is.na(Q4.22)) %>%
  filter(Q4.22 != "Don't know") %>%
  mutate(Q4.22 = droplevels(Q4.22),
         Q4.22 = factor(Q4.22, levels=rev(levels(Q4.22)))) %>%
  select(Q4.22, potential.contentiousness)

plot.discuss.issue <- prodplot(df.discuss.issue,
                               ~ potential.contentiousness + Q4.22, mosaic("h"), 
                               colour=NA) + 
  aes(fill=potential.contentiousness, colour="white") + 
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Discussions about regulations, by issue",
       subtitle="Q4.22: Has your organization discussed NGO regulations with government officials in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.discuss.issue

discuss.issue.table <- df.discuss.issue %>%
  xtabs(~ Q4.22 + potential.contentiousness, .)

analyze.cat.var(discuss.issue.table)


#' ### Attempts to change regulations
#' 
#' #### Regime type
#' 
#' Most INGOs don't attempt to change regulations, but those that do are most
#' likely to work in autocracies (and the difference is significant).
#' Additionally, only 8% of INGOs working in democracies have attempted to
#' change regulations, while 16% of those working in autocracies have, despite
#' the fact that the legislative process is much more difficult to navigate in
#' autocracies. INGOs appear to be more likely to engage in the political
#' process to adjust regulations despite these challenges.
df.change.regs.regime <- survey.countries.clean %>%
  filter(!is.na(Q4.23)) %>%
  filter(Q4.23 != "Don't know") %>%
  mutate(Q4.23 = droplevels(Q4.23),
         Q4.23 = factor(Q4.23, levels=rev(levels(Q4.23)))) %>%
  select(Q4.23, target.regime.type)

plot.change.regs.regime <- prodplot(df.change.regs.regime,
                                ~ target.regime.type + Q4.23, mosaic("h"), 
                                colour=NA) + 
  aes(fill=target.regime.type, colour="white") + 
  scale_fill_manual(values=ath.palette("regime"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Attempts to change regulations, by regime type",
       subtitle="Q4.23: Has your organization tried to change NGO regulations in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.change.regs.regime

change.regs.table <- df.change.regs.regime %>%
  xtabs(~ Q4.23 + target.regime.type, .)

analyze.cat.var(change.regs.table)


#' #### Potential contentiousness
#' 
#' There's also a difference across issue area. INGOs working on highly
#' contentious issues are more likely to try to change regulations, similar to
#' regime type. The difference is statistically significant overall, but is not
#' driven by a single significant cell (though yes + high comes close).
df.change.regs.issue <- survey.countries.clean %>%
  filter(!is.na(Q4.23)) %>%
  filter(Q4.23 != "Don't know") %>%
  mutate(Q4.23 = droplevels(Q4.23),
         Q4.23 = factor(Q4.23, levels=rev(levels(Q4.23)))) %>%
  select(Q4.23, potential.contentiousness)

plot.change.regs.issue <- prodplot(df.change.regs.issue,
                               ~ potential.contentiousness + Q4.23, mosaic("h"), 
                               colour=NA) + 
  aes(fill=potential.contentiousness, colour="white") + 
  scale_fill_manual(values=ath.palette("contention"), name=NULL) +
  guides(fill=FALSE) +
  labs(title="Attempts to change regulations, by issue",
       subtitle="Q4.23: Has your organization tried to change NGO regulations in `target_country`?") +
  theme_ath() + theme(axis.title=element_blank(),
                      panel.grid=element_blank())

#+ fig.width=6, fig.height=2
plot.change.regs.issue

change.regs.issue.table <- df.change.regs.issue %>%
  xtabs(~ Q4.23 + potential.contentiousness, .)

analyze.cat.var(change.regs.issue.table)


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
