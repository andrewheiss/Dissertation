library(dplyr)
library(tidyr)
library(readr)
library(stringi)
library(purrr)
library(feather)

# TODO: Replace Greenland with Anguilla
# TODO: Replace Aruba with Kosovo
# TODO: Replace Cayman Islands with Taiwan


# ------------------
# Useful functions
# ------------------
# Check if all the columns in a row are empty
# Return TRUE if yes
is.empty.loop <- function(row.to.check) {
  questions.answered <- row.to.check %>%
    select(starts_with("Q4")) %>%
    # Force everything to boolean; FALSE = missing value
    mutate_each(funs(!is.na(.))) %>%
    # Total number of questions answered in loop
    mutate(questions.answered = rowSums(.))
  
  # The row is empty if there are no TRUEs
  is.empty <- questions.answered$questions.answered == 0
  return(is.empty)
}

# Quiet as.numeric
as_num <- quietly(as.numeric)

# Use on the output of unite to get rid of NAs in list and match all the values
# with part from the column name
match_collapse <- function(x) {
  # Split string, convert to numeric, and remove NAs
  res <- as_num(stri_split_regex(x, ",")[[1]])
  res.collapsed <- paste(which(!is.na(res$result)), collapse=",")
  
  if (res.collapsed == "") {
    return(NA)
  } else {
    return(res.collapsed)
  }
}

# Look up a list of Qualtrics country IDs and return a list of clean country
# names, ISO3 codes, or COW codes
countries <- read_feather(file.path(PROJHOME, "Data", "Survey", "output",
                                    "survey_countries.feather"))

match_country_id <- function(x, id="name") {
  if (!id %in% c("name", "iso3", "cow")) {
    stop("Invalid 'id'")
  }
  
  df <- data_frame(`Qualtrics ID` = as.numeric(x)) %>%
    left_join(rename(countries, country_name = `Country name`, 
                     iso3 = ISO3, cow = `COW code`),
              by="Qualtrics ID")
  
  switch(id, 
         "name" = df$country_name,
         "iso3" = df$iso3,
         "cow" = df$cow)
}

# Calculate the length of a vector omitting the NAs
length_na <- function(x) {
  return(length(x[!is.na(x)]))
}

# Return the values of a vector based on its indices
# Example:
#   assign_value(c("a", "b", "c", "d"), c(2, 4))
#   [1] "b" "d"
assign_value <- function(x, values) {
  x <- as.numeric(x)
  values[x]
}

# Load a CSV exported from Qualtrics assigning column types from a separate file
read_qualtrics <- function(survey_file, col_file) {
  col_names.df <- read_csv(col_file)

  col_names <- col_names.df$col_name
  col_types <- paste(col_names.df$col_type, collapse="")
  
  survey.raw <- read_csv(survey_file, col_names=col_names, 
                         col_types=col_types, skip=3)
  
  survey.raw
}


# -----------------
# Variable values
# -----------------
yes.no <- function(x) {
  factor(x, levels=1:2, labels=c("Yes", "No"))
}

yes.no.dk <- function(x) {
  factor(x, levels=1:3, labels=c("Yes", "No", "Don't know"))
}

yndk.na <- c("Yes", "No", "Don't know", "Not applicable")

position.in.org <- c("Executive director", "Program officer", 
                     "Public relations officer", "Staff member", 
                     "Receptionist", "Other")

issues <- c("Development", "Human rights", "Environment", "Education", 
            "Disaster relief", "Freedom of expression", "Democracy assistance", 
            "Human trafficking", "Other")

always.never <- c("Always", "Most of the time", "About half the time",
                  "Sometimes", "Never")

always.never.dk <- c(always.never, "Don't know", "Not applicable")

collaboration <- c("Other nongovernmental organizations (NGOs)", 
                   "International organizations (IGOs)", "Governments", 
                   "Corporations or businesses", "Other", "Don't know", 
                   "We do not collaborate with other organizations or institutions")

great.none.dk <- c("A great deal", "A lot", "A moderate amount",
                   "A little", "None at all", "Don't know", "Not applicable")

how.long <- c("Less than 1 year", "1–4 years", "5–9 years",
              "10 years or more", "Don't know")

what.do <- c("Maintain a physical office staffed primarily by foreigners",
             "Maintain a physical office staffed primarily by people from target_country",
             "Provide funding to domestic NGOs",
             "Partner with domestic NGOs", "Don't know")

how.often <- c("Once a week", "Once a month", "Once a year", 
               "Once every 2+ years", "Never", "Don't know", "Other")

how.often.short <- c("Once a month", "Once a year", "Once every few years",
                     "Rarely", "Never", "Don't know")

govt.officials <- c("President or prime minister", "Member of parliament", 
                    "Head of a ministry", "Ministry staff", "Military",
                    "Police or internal security", "Other", 
                    "We have no contact with government officials", "Don't know")

pos.neg <- c("Extremely negative", "Somewhat negative",
             "Neither positive nor negative",
             "Somewhat positive", "Extremely positive",
             "Don't know", "Prefer not to answer")

familiar <- c("Extremely familiar", "Very familiar","Moderately familiar", 
              "Slightly familiar", "Not familiar at all", "Don't know")

how.find.out <- c("Government officials", "Other NGOs",
                  "Newspapers, television, and other media",
                  "The internet", "Other", "Don't know")

restricted <- c("Not restricted at all", "Slightly restricted",
                "Moderately restricted", "Very restricted",
                "Extremely restricted", "Don’t know")

stop.continue <- c("Answer questions about another country",
                   "Continue with survey's final questions")


# -----------------------------
# Load, munge, and clean data
# -----------------------------
# Load raw data
survey.v1 <- read_qualtrics(file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "INGOs_and_government_regulations.csv"),
                            file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "v1cols.csv"))
survey.v2 <- read_qualtrics(file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "INGOs_and_government_regulations_new.csv"),
                            file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "v2cols.csv"))

# Organization-level data
survey.v1.orgs <- survey.v1 %>%
  magrittr::set_colnames(gsub("#", "zzz", colnames(.))) %>%
  select(-starts_with("Q4")) %>%
  select(-c(ResponseSet, IPAddress, starts_with("Recipient"), 
            ExternalDataReference, starts_with("Location"))) %>%
  # Metadata
  mutate(survey.duration = EndDate - StartDate,
         Q1.1 = yes.no(Q1.1)) %>%
  #
  # Q2.x block
  left_join(rename(countries, Q2.2_country = `Country name`, Q2.2_iso3 = ISO3,
                   Q2.2_cow = `COW code`), 
            by=c("Q2.2" = "Qualtrics ID")) %>%
  mutate(Q2.3 = factor(Q2.3, levels=1:6, labels=position.in.org),
         Q2.4 = yes.no(Q2.4)) %>%
  unite(Q2.5, starts_with("Q2.5"), sep=",") %>%
  # Match values from unite() with column names, then remove NAs from resulting
  # list, then convert text list into actual list
  mutate(Q2.5 = stri_split(map_chr(Q2.5, match_collapse), regex=","),
         Q2.5_count = map(Q2.5, length_na),
         Q2.5_name = map(Q2.5, match_country_id, id="name"),
         Q2.5_iso3 = map(Q2.5, match_country_id, id="iso3"),
         Q2.5_cow = map(Q2.5, match_country_id, id="cow")) %>%
  #
  # Q3.x block
  unite(Q3.1, starts_with("Q3.1_"), -Q3.1_9_TEXT, sep=",") %>%
  rename(Q3.1_other_TEXT = Q3.1_9_TEXT) %>%
  mutate(Q3.1 = stri_split(map_chr(Q3.1, match_collapse), regex=","),
         Q3.1_value = map(Q3.1, assign_value, values=issues),
         Q3.2 = factor(Q3.2, levels=1:9, labels=issues)) %>%
  rename(Q3.3_aid = Q3.3zzz1_1, Q3.3_education = Q3.3zzz1_2,
         Q3.3_mobilize = Q3.3zzz1_3, Q3.3_advocacy = Q3.3zzz1_4,
         Q3.3_monitor = Q3.3zzz1_5,
         Q3.3_aid_dk = Q3.3zzz2_1, Q3.3_education_dk = Q3.3zzz2_2,
         Q3.3_mobilize_dk = Q3.3zzz2_3, Q3.3_advocacy_dk = Q3.3zzz2_4,
         Q3.3_monitor_dk = Q3.3zzz2_5,
         Q3.3_aid_TEXT = Q3.3zzz3_1_1_TEXT, Q3.3_education_TEXT = Q3.3zzz3_2_1_TEXT,
         Q3.3_mobilize_TEXT = Q3.3zzz3_3_1_TEXT, Q3.3_advocacy_TEXT = Q3.3zzz3_4_1_TEXT,
         Q3.3_monitor_TEXT = Q3.3zzz3_5_1_TEXT) %>%
  # Increment the "_dk" columns by 5 so they're on the same scale
  mutate_each(funs(. + 5), matches("Q3\\.3_.+_dk")) %>%
  # If they marked something in a "_dk" column, use it
  mutate(Q3.3_aid = ifelse(!is.na(Q3.3_aid_dk), 
                           Q3.3_aid_dk, Q3.3_aid),
         Q3.3_education = ifelse(!is.na(Q3.3_education_dk), 
                                 Q3.3_education_dk, Q3.3_education),
         Q3.3_mobilize = ifelse(!is.na(Q3.3_mobilize_dk), 
                                Q3.3_mobilize_dk, Q3.3_mobilize),
         Q3.3_advocacy = ifelse(!is.na(Q3.3_advocacy_dk), 
                                Q3.3_advocacy_dk, Q3.3_advocacy),
         Q3.3_monitor = ifelse(!is.na(Q3.3_monitor_dk), 
                               Q3.3_monitor_dk, Q3.3_monitor)) %>%
  # Get rid of don't know columns
  select(-matches("Q3\\.3_.+_dk")) %>%
  # Convert answers to factors
  mutate_each(funs(factor(., levels=1:7, labels=always.never.dk)),
              c(Q3.3_aid, Q3.3_education, Q3.3_mobilize, 
                Q3.3_advocacy, Q3.3_monitor)) %>%
  unite(Q3.6, starts_with("Q3.6_"), -Q3.6_5_TEXT, sep=",") %>%
  mutate(Q3.6 = stri_split(map_chr(Q3.6, match_collapse), regex=","),
         Q3.6_value = map(Q3.6, assign_value, values=collaboration)) %>%
  rename(Q3.6_other_TEXT = Q3.6_5_TEXT) %>%
  rename(Q3.8_individual = Q3.8zzz1_1, Q3.8_corporate = Q3.8zzz1_2,
         Q3.8_foundation = Q3.8zzz1_3, Q3.8_home_govt = Q3.8zzz1_4,
         Q3.8_host_govt = Q3.8zzz1_5, Q3.8_other = Q3.8zzz1_6,
         Q3.8_individual_dk = Q3.8zzz2_1, Q3.8_corporate_dk = Q3.8zzz2_2,
         Q3.8_foundation_dk = Q3.8zzz2_3, Q3.8_home_govt_dk = Q3.8zzz2_4,
         Q3.8_host_govt_dk = Q3.8zzz2_5, Q3.8_other_dk = Q3.8zzz2_6,
         Q3.8_other_TEXT = Q3.8zzz1_6_TEXT) %>%
  # Increment the "_dk" columns by 5 so they're on the same scale
  mutate_each(funs(. + 5), matches("Q3\\.8_.+_dk")) %>%
  # If they marked something in a "_dk" column, use it
  mutate(Q3.8_individual = ifelse(!is.na(Q3.8_individual_dk), 
                                  Q3.8_individual_dk, Q3.8_individual),
         Q3.8_corporate = ifelse(!is.na(Q3.8_corporate_dk), 
                                 Q3.8_corporate_dk, Q3.8_corporate),
         Q3.8_foundation = ifelse(!is.na(Q3.8_foundation_dk), 
                                  Q3.8_foundation_dk, Q3.8_foundation),
         Q3.8_home_govt = ifelse(!is.na(Q3.8_home_govt_dk), 
                                 Q3.8_home_govt_dk, Q3.8_home_govt),
         Q3.8_host_govt = ifelse(!is.na(Q3.8_host_govt_dk), 
                                 Q3.8_host_govt_dk, Q3.8_host_govt),
         Q3.8_other = ifelse(!is.na(Q3.8_other_dk), 
                             Q3.8_other_dk, Q3.8_other)) %>%
  # Get rid of don't know columns
  select(-matches("Q3\\.8_.+_dk"), -Q3.8zzz2_6_TEXT) %>%
  # Convert answers to factors
  mutate_each(funs(factor(., levels=1:7, labels=great.none.dk)),
              c(Q3.8_individual, Q3.8_corporate, Q3.8_foundation, 
                Q3.8_home_govt, Q3.8_host_govt, Q3.8_other)) %>%
  #
  # Q5.x block
  mutate(Q5.2 = yes.no(Q5.2),
         Q5.3 = yes.no(Q5.3))


# Country-level data
survey.v1.countries <- survey.v1 %>%
  magrittr::set_colnames(gsub("#", "zzz", colnames(.))) %>%
  select(ResponseID, starts_with("Q4")) %>%
  #
  # Split all Q4* columns into a key and value column
  gather(key, value, starts_with("Q4")) %>%
  #
  # Split the key column into two parts: question number and loop number.
  # Each column follows this pattern: Q4.2(1), Q4.3_1(2), etc., so the regex 
  # captures both parts: (Q4.2, 1), (Q4.3_1, 2), etc.
  extract(key, c("question", "loop.number"),
          c("(Q4.+)\\((\\d+)\\)")) %>%
  #
  # Make columns for each of the questions
  spread(question, value) %>%
  filter(!is.empty.loop(.)) %>%
  #
  # Clean up columns
  mutate(loop.number = as.numeric(loop.number),
         Q4.1_name = match_country_id(as.numeric(Q4.1), "name"),
         Q4.1_iso3 = match_country_id(as.numeric(Q4.1), "iso3"),
         Q4.1_cow = match_country_id(as.numeric(Q4.1), "cow")) %>%
  select(-Q4.1) %>%
  mutate(Q4.2 = factor(Q4.2, levels=1:5, labels=how.long)) %>%
  unite(Q4.3, starts_with("Q4.3_"), sep=",") %>%
  mutate(Q4.3 = stri_split(map_chr(Q4.3, match_collapse), regex=","),
         Q4.3_value = map(Q4.3, assign_value, values=what.do)) %>%
  mutate(Q4.4 = yes.no.dk(Q4.4),
         Q4.5 = factor(Q4.5, levels=1:7, labels=how.often)) %>%
  unite(Q4.6, starts_with("Q4.6"), -Q4.6_7_TEXT, sep=",") %>%
  mutate(Q4.6 = stri_split(map_chr(Q4.6, match_collapse), regex=","),
         Q4.6_value = map(Q4.6, assign_value, values=govt.officials)) %>%
  rename(Q4.6_other_TEXT = Q4.6_7_TEXT) %>%
  mutate(Q4.7 = factor(Q4.7, levels=1:9, labels=govt.officials),
         Q4.8 = factor(Q4.8, levels=1:7, labels=how.often),
         Q4.9 = yes.no.dk(Q4.9),
         Q4.11 = factor(Q4.11, levels=1:7, labels=pos.neg),
         Q4.13 = factor(Q4.13, levels=1:6, labels=familiar),
         Q4.14 = factor(Q4.14, levels=1:6, labels=how.often.short)) %>%
  unite(Q4.15, starts_with("Q4.15_"),-Q4.15_5_TEXT, sep=",") %>%
  mutate(Q4.15 = stri_split(map_chr(Q4.15, match_collapse), regex=","),
         Q4.15_value = map(Q4.15, assign_value, values=how.find.out)) %>%
  rename(Q4.15_other_TEXT = Q4.15_5_TEXT) %>%
  rename(Q4.16_registration = Q4.16zzz1_1, Q4.16_operations = Q4.16zzz1_2,
         Q4.16_speech = Q4.16zzz1_3, Q4.16_communications = Q4.16zzz1_4,
         Q4.16_assembly = Q4.16zzz1_5, Q4.16_resources = Q4.16zzz1_6,
         Q4.16_registration_dk = Q4.16zzz2_1, Q4.16_operations_dk = Q4.16zzz2_2,
         Q4.16_speech_dk = Q4.16zzz2_3, Q4.16_communications_dk = Q4.16zzz2_4,
         Q4.16_assembly_dk = Q4.16zzz2_5, Q4.16_resources_dk = Q4.16zzz2_6,
         Q4.16_registration_TEXT = Q4.16zzz3_1_1_TEXT, Q4.16_operations_TEXT = Q4.16zzz3_2_1_TEXT,
         Q4.16_speech_TEXT = Q4.16zzz3_3_1_TEXT, Q4.16_communications_TEXT = Q4.16zzz3_4_1_TEXT,
         Q4.16_assembly_TEXT = Q4.16zzz3_5_1_TEXT, Q4.16_resources_TEXT = Q4.16zzz3_6_1_TEXT) %>%
  # Increment the "_dk" columns by 5 so they're on the same scale
  mutate_each(funs(as.numeric(.) + 5), matches("Q4\\.16_.+_dk")) %>%
  # If they marked something in a "_dk" column, use it
  mutate(Q4.16_registration = ifelse(!is.na(Q4.16_registration_dk), 
                                     Q4.16_registration_dk, Q4.16_registration),
         Q4.16_operations = ifelse(!is.na(Q4.16_operations_dk), 
                                   Q4.16_operations_dk, Q4.16_operations),
         Q4.16_speech = ifelse(!is.na(Q4.16_speech_dk), 
                               Q4.16_speech_dk, Q4.16_speech),
         Q4.16_communications = ifelse(!is.na(Q4.16_communications_dk), 
                                       Q4.16_communications_dk, Q4.16_communications),
         Q4.16_assembly = ifelse(!is.na(Q4.16_assembly_dk), 
                                 Q4.16_assembly_dk, Q4.16_assembly),
         Q4.16_resources = ifelse(!is.na(Q4.16_resources_dk), 
                                  Q4.16_resources_dk, Q4.16_resources)) %>%
  # Get rid of don't know columns
  select(-matches("Q4\\.16_.+_dk")) %>%
  mutate_each(funs(factor(., levels=1:7, labels=great.none.dk)),
              c(Q4.16_registration, Q4.16_operations, Q4.16_speech, 
                Q4.16_communications, Q4.16_assembly, Q4.16_resources)) %>%
  mutate(Q4.17 = factor(Q4.17, levels=1:6, labels=restricted),
         Q4.19 = yes.no.dk(Q4.19)) %>%
  rename(Q4.21_funding = Q4.21zzz1_1, Q4.21_issues = Q4.21zzz1_2,
         Q4.21_comm_govt = Q4.21zzz1_3, Q4.21_comm_donors = Q4.21zzz1_4,
         Q4.21_locations = Q4.21zzz1_5, Q4.21_country_office = Q4.21zzz1_6,
         Q4.21_local_staff = Q4.21zzz1_7, Q4.21_foreign_staff = Q4.21zzz1_8,
         Q4.21_funding_dk = Q4.21zzz2_1, Q4.21_issues_dk = Q4.21zzz2_2,
         Q4.21_comm_govt_dk = Q4.21zzz2_3, Q4.21_comm_donors_dk = Q4.21zzz2_4,
         Q4.21_locations_dk = Q4.21zzz2_5, Q4.21_country_office_dk = Q4.21zzz2_6,
         Q4.21_local_staff_dk = Q4.21zzz2_7, Q4.21_foreign_staff_dk = Q4.21zzz2_8,
         Q4.21_funding_TEXT = Q4.21zzz3_1_1_TEXT,
         Q4.21_issues_TEXT = Q4.21zzz3_2_1_TEXT,
         Q4.21_comm_govt_TEXT = Q4.21zzz3_3_1_TEXT,
         Q4.21_comm_donors_TEXT = Q4.21zzz3_4_1_TEXT,
         Q4.21_locations_TEXT = Q4.21zzz3_5_1_TEXT,
         Q4.21_country_office_TEXT = Q4.21zzz3_6_1_TEXT,
         Q4.21_local_staff_TEXT = Q4.21zzz3_7_1_TEXT,
         Q4.21_foreign_staff_TEXT = Q4.21zzz3_8_1_TEXT) %>%
  # Increment the "_dk" columns by 2 so they're on the same scale
  mutate_each(funs(as.numeric(.) + 2), matches("Q4\\.21_.+_dk")) %>%
  # If they marked something in a "_dk" column, use it
  mutate(Q4.21_funding = ifelse(!is.na(Q4.21_funding_dk), 
                                Q4.21_funding_dk, Q4.21_funding),
         Q4.21_issues = ifelse(!is.na(Q4.21_issues_dk), 
                               Q4.21_issues_dk, Q4.21_issues),
         Q4.21_comm_govt = ifelse(!is.na(Q4.21_comm_govt_dk), 
                                  Q4.21_comm_govt_dk, Q4.21_comm_govt),
         Q4.21_comm_donors = ifelse(!is.na(Q4.21_comm_donors_dk), 
                                    Q4.21_comm_donors_dk, Q4.21_comm_donors),
         Q4.21_locations = ifelse(!is.na(Q4.21_locations_dk), 
                                  Q4.21_locations_dk, Q4.21_locations),
         Q4.21_country_office = ifelse(!is.na(Q4.21_country_office_dk), 
                                       Q4.21_country_office_dk, Q4.21_country_office),
         Q4.21_local_staff = ifelse(!is.na(Q4.21_local_staff_dk), 
                                    Q4.21_local_staff_dk, Q4.21_local_staff),
         Q4.21_foreign_staff = ifelse(!is.na(Q4.21_foreign_staff_dk), 
                                      Q4.21_foreign_staff_dk, Q4.21_foreign_staff)) %>%
  # Get rid of don't know columns
  select(-matches("Q4\\.21_.+_dk")) %>%
  mutate_each(funs(factor(., levels=1:4, labels=yndk.na)),
              c(Q4.21_funding, Q4.21_issues, Q4.21_comm_govt, 
                Q4.21_comm_donors, Q4.21_locations, Q4.21_country_office,
                Q4.21_local_staff, Q4.21_foreign_staff)) %>%
  mutate(Q4.22 = yes.no.dk(Q4.22),
         Q4.23 = yes.no.dk(Q4.23),
         Q4.24 = factor(Q4.24, levels=1:2, labels=stop.continue)) %>%
  select(ResponseID, loop.number, starts_with("Q4.1_"), Q4.2, Q4.3, Q4.3_value,
         Q4.4, starts_with("Q4.5"), starts_with("Q4.6"), starts_with("Q4.7"),
         starts_with("Q4.8"), Q4.9, Q4.10, Q4.11, Q4.12, Q4.13, Q4.14,
         starts_with("Q4.15"), starts_with("Q4.16"), Q4.17, Q4.18, Q4.19,
         Q4.20, starts_with("Q4.21"), Q4.22, Q4.23, Q4.24)
