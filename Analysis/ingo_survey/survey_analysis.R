#' ---
#' title: "Survey analysis"
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
#'     keep_md: yes
#' bibliography: /Users/andrew/Dropbox/Readings/Papers.bib
#' csl: /Users/andrew/.pandoc/csl/american-political-science-association.csl
#' ...

#' # Load clean data and set everything up
#+ message=FALSE
library(dplyr)
library(ggplot2)
library(ggstance)
library(stringr)
library(pander)
library(magrittr)
library(DT)
library(scales)

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


#' # General questions
#'
#' ## How many NGOs responded?
nrow(survey.orgs.clean) 


#' ## How many respondents answered questions for more than one country?
survey.countries.clean %>% filter(loop.number > 1) %>% nrow %T>%
  {print(percent(. / nrow(survey.orgs.clean)))}


#' ## Who in the organization responded to the survey?
df.plot.respondents <- survey.orgs.clean %>%
  group_by(Q2.3) %>%
  summarise(num = n()) %>%
  arrange(num) %>% 
  mutate(question = factor(Q2.3, levels=Q2.3, ordered=TRUE))

plot.respondents <- ggplot(df.plot.respondents, aes(x=num, y=question)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0)) +
  labs(x="Number of respondents", y=NULL, 
       title="Who filled out the survey?",
       subtitle="Q2.3: What is your position in your organization?") +
  theme_ath()

plot.respondents

#' Lots of others. Who are the others?
# Table
position.in.org <- survey.orgs.clean %>%
  filter(!is.na(Q2.3_TEXT)) %>%
  mutate(position.in.org = str_to_title(Q2.3_TEXT)) %>% 
  group_by(position.in.org) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

datatable(position.in.org)

# Figure
df.plot.respondents.other <- position.in.org %>%
  filter(num >= 5) %>%
  mutate(question = factor(position.in.org, 
                           levels=rev(position.in.org), ordered=TRUE))

plot.respondents.other <- ggplot(df.plot.respondents.other,
                                 aes(x=num, y=question)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0)) +
  labs(x="Number of respondents", y=NULL,
       title="What are the positions of those who answered “Other”?",
       subtitle="Q2.3: What is your position in your organization?: Free responses to “Other”") +
  theme_ath()

plot.respondents.other


#' ## How are these NGOs distributed by HQ?
#' 
#' Where do these NGOs work? Count all the countries they selected + the countries they answered for
#' 
#' ## What do these NGOs do?
#' 
#' What kind of issues do the NGOs work on? Which issues do they work on the most?
#' 
#' What kinds of activities do these NGOs engage in?
#' 
#' How big are these NGOs? Staff? Volunteers?
#' 
#' Do NGOs collaborate?
#' 
#' Where does their funding come from?
#' 
#' ## Deeper principles (mission, vision, values)
#' 
#' Q3.9 - Q3.13
#' 

#' # Testing hypotheses
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
