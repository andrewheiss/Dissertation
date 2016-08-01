library(dplyr)
library(tidyr)
library(pander)

# Load cleaned, organization-based data (without the Q4 loop)
survey.orgs.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                       "survey_orgs_clean.rds"))

# Load cleaned, country-based data (only the Q4 loop)
survey.countries.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                            "survey_countries_clean.rds"))

# Introductory quesionts
export.intro <- function(org) {
  home.country <- org$Q2.2_country
  pandoc.header("Introductory questions", 1)
  pandoc.header("*Q2.1*: What is the name of your organization?", level=3)
  pandoc.p(org$Q2.1)
  
  pandoc.header("*Q2.2*: Where is your organization's headquarters?", level=3)
  pandoc.p(org$Q2.2_country)
  
  pandoc.header("*Q2.3*: What is your position in your organization?", level=3)
  pandoc.p(org$Q2.3)
  pandoc.p(paste0("Other: ", org$Q2.3_TEXT))
  
  pandoc.header(sprintf("*Q2.4*: Does your organization work in a country other than %s?", home.country), level=3)
  pandoc.p(org$Q2.4)
  
  pandoc.header(sprintf("Q2.5: Besides %s, where does your organization work?", home.country), level=3)
  pandoc.list(org$Q2.5_name, indent.level=-1)
}


# Organizational questions
export.organizational <- function(org) {
  pandoc.header("Organizational questions", 1)
  pandoc.header("*Q3.1*: Which issues does your organization focus on?", level=3)
  pandoc.list(org$Q3.1.clean_value, indent.level=-1)
  pandoc.p(paste0("Other: ", org$Q3.1_other_TEXT))
  
  pandoc.header("*Q3.2*: Which issue does your organization focus on *the most*?", level=3)
  pandoc.p(org$Q3.2.clean)
  
  pandoc.header("*Q3.3*: Please indicate how often your organization engages in each of these types of activities", level=3)
  
  org %>% select(starts_with("Q3.3")) %>%
    gather(key, answer) %>%
    separate(key, into=c("option", "key"), sep="_T") %>%
    mutate(key = ifelse(is.na(key), "response", "text")) %>%
    spread(key, answer) %>%
    mutate(text = ifelse(is.na(text), " ", text)) %>%
    pandoc.table()
  
  pandoc.header("*Q3.4*: Approximately how many full-time employees does your organization have?", level=3)
  pandoc.p(paste0(org$Q3.4, " (", org$Q3.4.num, ")"))
  
  pandoc.header("*Q3.5*: Approximately how many volunteers does your organization have?", level=3)
  pandoc.p(paste0(org$Q3.5, " (", org$Q3.5.num, ")"))
  
  pandoc.header("*Q3.6*: Does your organization collaborate with any of these organizations or institutions?", level=3)
  pandoc.list(org$Q3.6_value, indent.level=0)
  
  pandoc.header("*Q3.7*: Please list a few of the organizations or institutions you partner with most often?", level=3)
  pandoc.p(org$Q3.7)
  
  pandoc.header("*Q3.8*: How much of your organization's funding comes from each of these sources?", level=3)
  
  org %>% select(starts_with("Q3.8")) %>%
    gather(option, answer) %>%
    pandoc.table()
  
  pandoc.header("*Q3.9*: In general, what would you say your organization is trying to accomplish?", level=3)
  pandoc.p(org$Q3.9)
  
  pandoc.header("*Q3.10*: How is your organization's mission, vision, and values reflected in these objectives?", level=3)
  pandoc.p(org$Q3.10)
  
  pandoc.header("*Q3.11*: Have these objectives changed at all in the last 10 years? If so, how?", level=3)
  pandoc.p(org$Q3.11)
  
  pandoc.header("*Q3.12*: What are the major obstacles, if any, to reaching your organization's objectives?", level=3)
  pandoc.p(org$Q3.12)
  
  pandoc.header("*Q3.13*: Are there any changes that you would like to see in your organization's goals and strategies, now or in the future?", level=3)
  pandoc.p(org$Q3.13)
}


# Final questions
export.final <- function(org) {
  pandoc.header("Final questions", level=1)
  pandoc.header("*Q5.1*: Do you have any additional comments?", level=3)
  pandoc.p(org$Q5.1)
  
  pandoc.header("*Q5.2*: May I contact you for any follow up questions?", level=3)
  pandoc.p(org$Q5.2)
}


# Country-based questions
export.country <- function(org.country) {
  country.name <- org.country$Q4.1_name
  
  pandoc.header(sprintf("Questions about work in %s", country.name), level=1)
  pandoc.header("General questions", level=2)
  
  pandoc.header(sprintf("*Q4.2*: How long has your organization worked in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.2)
  
  pandoc.header(sprintf("*Q4.3*: What does your organization do in %s?", country.name), level=3)
  pandoc.list(org.country$Q4.3_value, indent.level=0)
  
  pandoc.header(sprintf("*Q4.4*: Is your organization registered with the national government in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.4)
  
  
  pandoc.header("Contact with government", level=2)
  pandoc.header(sprintf("*Q4.5*: About how often does your organization have contact with government or party officials in %s?", country.name), level=3)
  pandoc.p(paste0(org.country$Q4.5, " (", org.country$Q4.5.clean, ")"))
  
  pandoc.header(sprintf("*Q4.6*: What kind of government officials does your organization have contact with?", country.name), level=3)
  pandoc.list(org.country$Q4.6_value, indent.level=-1)
  pandoc.p(paste0("Other: ", org.country$Q4.6_other_TEXT))
  
  pandoc.header(sprintf("*Q4.7*: What kind of government officials does your organization have contact with *most often*?", country.name), level=3)
  pandoc.p(org.country$Q4.7)
  pandoc.p(paste0("Other: ", org.country$Q4.7_TEXT))
  
  pandoc.header(sprintf("*Q4.8*: How often is your organization required to report to the government of %s?", country.name), level=3)
  pandoc.p(paste0(org.country$Q4.8, " (", org.country$Q4.8.clean, ")"))
  
  pandoc.header(sprintf("*Q4.9*: Are members of the government or ruling party of %s involved in your work?", country.name), level=3)
  pandoc.p(org.country$Q4.9)
  
  pandoc.header(sprintf("*Q4.10*: How is the government of %s involved in your work?", country.name), level=3)
  pandoc.p(org.country$Q4.10)
  
  
  pandoc.header("Relationship with government", level=2)
  pandoc.header(sprintf("*Q4.11*: How would you characterize your organization's relationship with the government of %s?", country.name), level=3)
  pandoc.p(org.country$Q4.11)
  
  pandoc.header(sprintf("*Q4.12*: Briefly describe your organization's relationship with the government of %s?", country.name), level=3)
  pandoc.p(org.country$Q4.12)
  
  
  pandoc.header("NGO regulations and restrictions", level=2)
  pandoc.header(sprintf("*Q4.13*: How familiar is your organization with regulations for international nongovernmental organizations (NGOs) in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.13)
  
  pandoc.header(sprintf("*Q4.14*: How often do regulations for international NGOs in %s change?", country.name), level=3)
  pandoc.p(org.country$Q4.14)
  
  pandoc.header(sprintf("*Q4.15*: How does your organization find out about changes to NGO regulations in %s?", country.name), level=3)
  pandoc.list(org.country$Q4.15_value, indent.level=0)
  pandoc.p(paste0("Other: ", org.country$Q4.15_other_TEXT))
  
  pandoc.header(sprintf("*Q4.16*: How is your organization affected by the following types of legal regulations for international NGOs in %s?", country.name), level=3)

  org.country %>% select(starts_with("Q4.16")) %>%
    gather(key, answer) %>%
    separate(key, into=c("option", "key"), sep="_T") %>%
    mutate(key = ifelse(is.na(key), "response", "text")) %>%
    spread(key, answer) %>%
    mutate(text = ifelse(is.na(text), " ", text)) %>%
    pandoc.table()
  
  pandoc.header(sprintf("*Q4.17*: Overall, how is your organization's work affected by government regulations in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.17)
  
  pandoc.header(sprintf("*Q4.18*: How do the local laws and regulations in %s affect your organization's ability to pursue its mission?", country.name), level=3)
  pandoc.p(org.country$Q4.18)
  
  
  pandoc.header("Responses to regulations", level=2)
  pandoc.header(sprintf("*Q4.19*: Over the last 10 years, has your organization changed its mix of programming in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.19)
  
  pandoc.header(sprintf("*Q4.20*: How has your organization's mix of programming changed in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.20)
  
  pandoc.header(sprintf("*Q4.21*: Has your organization done any of the following in response to changes in government regulations in %s?", country.name), level=3)

  org.country %>% select(starts_with("Q4.21")) %>%
    gather(key, answer) %>%
    separate(key, into=c("option", "key"), sep="_T") %>%
    mutate(key = ifelse(is.na(key), "response", "text")) %>%
    spread(key, answer) %>%
    mutate(text = ifelse(is.na(text), " ", text)) %>%
    pandoc.table()
  
  pandoc.header(sprintf("*Q4.22*: Has your organization discussed NGO regulations with government officials in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.22)
  
  pandoc.header(sprintf("*Q4.23*: Has your organization tried to change NGO regulations in %s?", country.name), level=3)
  pandoc.p(org.country$Q4.23)
}


# Export all chunks together
export.response <- function(org) {
  export.intro(org)
  export.organizational(org)
  
  org.countries <- survey.countries.clean %>%
    filter(ResponseID == org$ResponseID)
  
  # This could be done with rowwise() + do(.), but do() makes the dataframe
  # chunk a kind of list, which messes things up. So instead we use an old
  # fashioned loop.
  for(i in 1:nrow(org.countries)) {
    export.country(org.countries[i,])
  }

  export.final(org)
}


# Export individual survey response
org <- survey.orgs.clean %>%
  # filter(ResponseID == "") %>%
  slice(1)

panderOptions('pandoc.binary', '/Users/andrew/.cabal/bin/pandoc')
output <- tempfile()
suppressWarnings(capture.output(export.response(org), file=output))
Pandoc.convert(output, footer=FALSE, proc.time=FALSE, options = "-s")
