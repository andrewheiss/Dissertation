library(tidyverse)
library(magrittr)
library(stringi)
library(stringr)
library(zoo)
library(countrycode)


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

# Load data from tracking database to get data on database provenance
db.email <- src_sqlite(path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "final_list.db"))

dbs <- tbl(db.email, "full_list") %>% collect() %>%
  separate(id_org, c("database", "id.in.db")) %>%
  select(index_org, database)

completed <- tbl(db.email, "survey_completed") %>% collect() %>%
  left_join(dbs, by=c("fk_org"="index_org")) %>%
  select(ResponseID = qualtrics_id, database) %>%
  filter(ResponseID != "") %>%
  group_by(ResponseID) %>%
  slice(1) %>%
  ungroup()


# Look up a list of Qualtrics country IDs and return a list of clean country
# names, ISO3 codes, or COW codes
countries.raw <- readRDS(file.path(PROJHOME, "Data", "Survey", "output",
                                   "survey_countries.rds"))

# Change Greenland > Anguilla; Aruba > Kosovo; Cayman Islands > Taiwan
countries.manual.fixes <- data_frame(`Country name` = c("Anguilla", 
                                                        "Kosovo", "Taiwan",
                                                        "Hong Kong SAR, China",
                                                        "Serbia", "West Bank and Gaza"),
                                     ISO3 = c("AIA", "XKX", "TWN", "HKG", "SRB", "PSE"),
                                     `COW code` = c(1022, 347, 713, 715, 340, 669),
                                     `Qualtrics ID` = c(74, 10, 36, 83, 163, 209))

countries <- countries.raw %>%
  filter(!(`Qualtrics ID` %in% countries.manual.fixes$`Qualtrics ID`)) %>%
  bind_rows(countries.manual.fixes)

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

how.often.extra <- c("Don't know", "Never", "Rarely", "Once every 2+ years", 
                     "As necessary/depends", "Once a year",
                     "More than once a year,\nless than once a month",
                     "Once a month", 
                     "More than once a month,\nless than once a week",
                     "Once a week", "More than once a week")

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


# ----------------------------------------------------------
# -----------------------------
# Load, munge, and clean data
# -----------------------------
# ----------------------------------------------------------
# Load raw data
survey.v1 <- read_qualtrics(file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "INGOs_and_government_regulations.csv"),
                            file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "v1cols.csv"))
survey.v2 <- read_qualtrics(file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "INGOs_and_government_regulations_new.csv"),
                            file.path(PROJHOME, "Data", "Survey", "raw_data", 
                                      "v2cols.csv")) %>%
  filter(!(ResponseID %in% c("R_12564WimpZZKn5A", "R_3kwdyUr7vHd8qiE")))


# -------------------------
# Organization-level data
# -------------------------
# Original version of the survey
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
  # Assume that missing values = not applicable
  mutate_each(funs(ifelse(is.na(.), 7, .)),
              c(Q3.3_aid, Q3.3_education, Q3.3_mobilize, 
                Q3.3_advocacy, Q3.3_monitor)) %>%
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
  # Assume that missing values = not applicable
  mutate_each(funs(ifelse(is.na(.), 7, .)),
              c(Q3.8_individual, Q3.8_corporate, Q3.8_foundation, 
                Q3.8_home_govt, Q3.8_host_govt, Q3.8_other)) %>%
  # Get rid of don't know columns
  select(-matches("Q3\\.8_.+_dk"), -Q3.8zzz2_6_TEXT) %>%
  # Convert answers to factors
  mutate_each(funs(factor(., levels=1:7, labels=great.none.dk)),
              c(Q3.8_individual, Q3.8_corporate, Q3.8_foundation, 
                Q3.8_home_govt, Q3.8_host_govt, Q3.8_other)) %>%
  #
  # Q5.x block
  mutate(Q5.2 = yes.no(Q5.2),
         Q5.3 = yes.no(Q5.3)) %>%
  mutate(survey = "First")

# Final version of the survey
survey.v2.orgs <- survey.v2 %>%
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
         Q3.3_aid_TEXT = Q3.3zzz2_1_1_TEXT, Q3.3_education_TEXT = Q3.3zzz2_2_1_TEXT,
         Q3.3_mobilize_TEXT = Q3.3zzz2_3_1_TEXT, Q3.3_advocacy_TEXT = Q3.3zzz2_4_1_TEXT,
         Q3.3_monitor_TEXT = Q3.3zzz2_5_1_TEXT) %>%
  # Assume that missing values = not applicable
  mutate_each(funs(ifelse(is.na(.), 7, .)),
              c(Q3.3_aid, Q3.3_education, Q3.3_mobilize, 
                Q3.3_advocacy, Q3.3_monitor)) %>%
  # Convert answers to factors
  mutate_each(funs(factor(., levels=1:7, labels=always.never.dk)),
              c(Q3.3_aid, Q3.3_education, Q3.3_mobilize, 
                Q3.3_advocacy, Q3.3_monitor)) %>%
  unite(Q3.6, starts_with("Q3.6_"), -Q3.6_5_TEXT, sep=",") %>%
  mutate(Q3.6 = stri_split(map_chr(Q3.6, match_collapse), regex=","),
         Q3.6_value = map(Q3.6, assign_value, values=collaboration)) %>%
  rename(Q3.6_other_TEXT = Q3.6_5_TEXT) %>%
  rename(Q3.8_individual = Q3.8_1, Q3.8_corporate = Q3.8_2,
         Q3.8_foundation = Q3.8_3, Q3.8_home_govt = Q3.8_4,
         Q3.8_host_govt = Q3.8_5, Q3.8_other = Q3.8_6,
         Q3.8_other_TEXT = Q3.8_6_TEXT) %>%
  # For whatever reason, the levels in Q3.8* aren't continuous; they include 
  # (1, 3-8) and skip 2
  # Assume that missing values = not applicable
  mutate_each(funs(ifelse(is.na(.), 8, .)),
              c(Q3.8_individual, Q3.8_corporate, Q3.8_foundation, 
                Q3.8_home_govt, Q3.8_host_govt, Q3.8_other)) %>%
  mutate_each(funs(factor(., levels=c(1, 3:8), labels=great.none.dk)),
              c(Q3.8_individual, Q3.8_corporate, Q3.8_foundation, 
                Q3.8_home_govt, Q3.8_host_govt, Q3.8_other)) %>%
  #
  # Q5.x block
  mutate(Q5.2 = yes.no(Q5.2),
         Q5.3 = yes.no(Q5.3)) %>%
  mutate(survey = "Final")


# --------------------
# Country-level data
# --------------------
# Original version of the survey
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
  tidyr::extract(key, c("question", "loop.number"),
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
  # Assume that missing values = not applicable
  mutate_each(funs(ifelse(is.na(.), 7, .)),
              c(Q4.16_registration, Q4.16_operations, Q4.16_speech,
                Q4.16_communications, Q4.16_assembly, Q4.16_resources)) %>%
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
  # Assume that missing values = not applicable
  mutate_each(funs(ifelse(is.na(.), 4, .)),
              c(Q4.21_funding, Q4.21_issues, Q4.21_comm_govt, 
                Q4.21_comm_donors, Q4.21_locations, Q4.21_country_office,
                Q4.21_local_staff, Q4.21_foreign_staff)) %>%
  # Get rid of don't know columns
  select(-matches("Q4\\.21_.+_dk")) %>%
  mutate_each(funs(factor(., levels=1:4, labels=yndk.na)),
              c(Q4.21_funding, Q4.21_issues, Q4.21_comm_govt, 
                Q4.21_comm_donors, Q4.21_locations, Q4.21_country_office,
                Q4.21_local_staff, Q4.21_foreign_staff)) %>%
  mutate(Q4.22 = yes.no.dk(Q4.22),
         Q4.23 = yes.no.dk(Q4.23),
         Q4.24 = factor(Q4.24, levels=1:2, labels=stop.continue)) %>%
  mutate(survey = "First")


# Final version of the survey
survey.v2.countries <- survey.v2 %>%
  magrittr::set_colnames(gsub("#", "zzz", colnames(.))) %>%
  select(ResponseID, starts_with("Q4")) %>%
  #
  # Split all Q4* columns into a key and value column
  gather(key, value, starts_with("Q4")) %>%
  #
  # Split the key column into two parts: question number and loop number.
  # Each column follows this pattern: Q4.2(1), Q4.3_1(2), etc., so the regex 
  # captures both parts: (Q4.2, 1), (Q4.3_1, 2), etc.
  tidyr::extract(key, c("question", "loop.number"),
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
  rename(Q4.16_registration = Q4.16_1, Q4.16_operations = Q4.16_2,
         Q4.16_speech = Q4.16_3, Q4.16_communications = Q4.16_4,
         Q4.16_assembly = Q4.16_5, Q4.16_resources = Q4.16_6,
         Q4.16_registration_TEXT = Q4.16a, Q4.16_operations_TEXT = Q4.16b,
         Q4.16_speech_TEXT = Q4.16c, Q4.16_communications_TEXT = Q4.16d,
         Q4.16_assembly_TEXT = Q4.16e, Q4.16_resources_TEXT = Q4.16f) %>%
  # Assume that missing values = not applicable
  # For whatever reason, the levels in Q4.16* aren't continuous; they include 
  # (1, 3-8) and skip 2
  mutate_each(funs(ifelse(is.na(.), 8, .)),
              c(Q4.16_registration, Q4.16_operations, Q4.16_speech,
                Q4.16_communications, Q4.16_assembly, Q4.16_resources)) %>%
  mutate_each(funs(factor(., levels=c(1, 3:8), labels=great.none.dk)),
              c(Q4.16_registration, Q4.16_operations, Q4.16_speech, 
                Q4.16_communications, Q4.16_assembly, Q4.16_resources)) %>%
  mutate(Q4.17 = factor(Q4.17, levels=1:6, labels=restricted),
         Q4.19 = yes.no.dk(Q4.19)) %>%
  rename(Q4.21_funding = Q4.21_1, Q4.21_issues = Q4.21_2,
         Q4.21_comm_govt = Q4.21_3, Q4.21_comm_donors = Q4.21_4,
         Q4.21_locations = Q4.21_5, Q4.21_country_office = Q4.21_6,
         Q4.21_local_staff = Q4.21_7, Q4.21_foreign_staff = Q4.21_8,
         Q4.21_funding_TEXT = Q4.21a, Q4.21_issues_TEXT = Q4.21b,
         Q4.21_comm_govt_TEXT = Q4.21c, Q4.21_comm_donors_TEXT = Q4.21d,
         Q4.21_locations_TEXT = Q4.21e, Q4.21_country_office_TEXT = Q4.21f,
         Q4.21_local_staff_TEXT = Q4.21g, Q4.21_foreign_staff_TEXT = Q4.21h) %>%
  # Same here. Q4.21* is missing 2 as a level
  # Assume that missing values = not applicable
  mutate_each(funs(ifelse(is.na(.), 5, .)),
              c(Q4.21_funding, Q4.21_issues, Q4.21_comm_govt, 
                Q4.21_comm_donors, Q4.21_locations, Q4.21_country_office,
                Q4.21_local_staff, Q4.21_foreign_staff)) %>%
  mutate_each(funs(factor(., levels=c(1, 3:5), labels=yndk.na)),
              c(Q4.21_funding, Q4.21_issues, Q4.21_comm_govt, 
                Q4.21_comm_donors, Q4.21_locations, Q4.21_country_office,
                Q4.21_local_staff, Q4.21_foreign_staff)) %>%
  mutate(Q4.22 = yes.no.dk(Q4.22),
         Q4.23 = yes.no.dk(Q4.23),
         Q4.24 = factor(Q4.24, levels=1:2, labels=stop.continue)) %>%
  mutate(survey = "Final")


# ----------------------------------------------------------------------
# -----------------------------------
# Deal with partials and duplicates
# -----------------------------------
# ----------------------------------------------------------------------
survey.orgs.all <- survey.v1.orgs %>%
  bind_rows(survey.v2.orgs)

survey.countries.all <- survey.v1.countries %>%
  bind_rows(survey.v2.countries) 

partials <- survey.orgs.all %>%
  left_join(survey.countries.all, by=c("ResponseID", "survey")) %>%
  distinct(ResponseID, .keep_all=TRUE) %>%
  filter(Finished == 0) %>%
  # Threshold for partialness determined in Analysis/ingo_survey/completion_rates.R
  # At least 31 questions answered + more than 12 questions answered in the Q4 loop
  mutate(num.answered = rowSums(!is.na(select(., starts_with("Q")))),
         num.answered.loop = rowSums(!is.na(select(., starts_with("Q4"))))) %>%
  filter(num.answered >= 31, num.answered.loop > 12)

# Create clean dataframes with complete and valid partial responses
survey.orgs.clean <- survey.orgs.all %>%
  # Only INGOs
  filter(Q2.4 == "Yes") %>%
  # Completes and valid partials
  filter(Finished == 1 | ResponseID %in% unique(partials$ResponseID)) %>%
  select(ResponseID, StartDate, EndDate, Finished, Status, 
         survey, survey.duration, Q1.1, Q2.1, Q2.2, starts_with("Q2.2_"), 
         starts_with("Q2.3"), Q2.4, Q2.4, starts_with("Q2.5"), 
         starts_with("Q3"), starts_with("Q5"), Q6.1, Q7.1) %>%
  left_join(completed, by="ResponseID") %>%
  mutate(database = ifelse(is.na(database), "unknown", database),
         complete = Finished == 1)

# Check for duplicates
duplicates <- survey.orgs.clean %>% group_by(Q2.1) %>% filter(n() > 1) %>%
  ungroup() %>%
  mutate(num.answered = rowSums(!is.na(select(., starts_with("Q"))))) %>%
  arrange(Q2.1) %>%
  select(num.answered, everything())

# Remove duplicates by hand based on duplicateness of organization name and
# respondent position. If two different positions took the survey (secretary +
# CEO), keep both. When there are duplicates, keep the most complete version
# (i.e. num.answered is highest)
duplicates.to.remove <- c("R_YX1hTAf6tyU0uKR", "R_3qkcPw5yq7OXdC3", 
                          "R_0090udYN7iJyZzz", "R_1rv3XB7IMwhvimY", 
                          "R_qUsJ4yoqFxJbQc1", "R_xFotgZUftQNYRNL", 
                          "R_12AnwDO20ylpr8Y", "R_2YhA66WSwhBGij5", 
                          "R_25KGf8lnLcWBi4t", "R_1NsD5DZORpTuP09", 
                          "R_1r35atiHGBTG2Mr", "R_3iO6djbKRSpsHmz", 
                          "R_3PzzfiVWgQwdX1h", "R_sgIvZaOeNU7etrj", 
                          "R_2qBKcjLLCZDbwpk")

survey.orgs.clean.final <- survey.orgs.clean %>%
  filter(!ResponseID %in% duplicates.to.remove)

partials.clean <- partials %>%
  filter(!ResponseID %in% duplicates.to.remove)

survey.countries.clean <- survey.countries.all %>%
  filter(ResponseID %in% survey.orgs.clean.final$ResponseID) %>%
  select(ResponseID, loop.number, survey, starts_with("Q4.1_"), Q4.2, Q4.3, 
         Q4.3_value, Q4.4, starts_with("Q4.5"), starts_with("Q4.6"), 
         starts_with("Q4.7"), starts_with("Q4.8"), Q4.9, Q4.10, Q4.11, Q4.12, 
         Q4.13, Q4.14, starts_with("Q4.15"), starts_with("Q4.16"), Q4.17, 
         Q4.18, Q4.19, Q4.20, starts_with("Q4.21"), Q4.22, Q4.23, Q4.24)


# ------------------------------------------------------
# ---------------------------
# Deal with hand-coded data
# ---------------------------
# ------------------------------------------------------
#
# ---------------------------
# Issues worked on the most
# ---------------------------
# In Q3.1, organzations were asked to choose which issues they worked on. One
# of the options was a free response "other" answer. Rather than carry this
# response forward (because they might write a really long response), when
# asked in Q3.2 which issue they worked on *the most*, one of the options was
# also "other" (though not a free response), standing in place for whatever
# they filled in as their other answer in Q3.1.
# 
# These other responses need to be hand coded. Using NLP or ML or something 
# would be cool, but there seemingly aren't enough responses to make that 
# useful. Regardless, responses do appear to cluster into recognizable
# categories.
survey.orgs.clean.final %>%
  filter(!is.na(Q3.1_other_TEXT)) %>%
  select(ResponseID, Q2.1, Q2.2_country, Q3.1_other_TEXT) %>%
  mutate(Q3.2.manual = "") %>%
  arrange(Q3.1_other_TEXT) %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "other_issues_WILL_BE_OVERWRITTEN.csv"))

# A handful of organizations left the issue area blank, but that's easiliy
# imputable based on websites and Q3.1
survey.orgs.clean.final %>%
  filter(is.na(Q3.1_other_TEXT) & (Q3.2 == "Other" | is.na(Q3.2))) %>%
  select(ResponseID, Q2.1, Q2.2_country, Q3.1_value) %>%
  mutate(Q3.2.manual = "", Q3.1_value = paste(Q3.1_value)) %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "missing_issues_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSVs
other.issues.raw <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                       "handcoded_survey_stuff",
                                       "other_issues.csv")) %>%
  select(ResponseID, Q3.2.manual)

missing.issues.raw <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                         "handcoded_survey_stuff",
                                         "missing_issues.csv")) %>%
  select(ResponseID, Q3.2.manual)

other.issues.all <- bind_rows(other.issues.raw, missing.issues.raw)

# Add the recoded issue to the list in Q3.1_value
all.issues.clean <- survey.orgs.clean.final %>%
  left_join(other.issues.all, by="ResponseID") %>%
  unnest(Q3.1_value) %>%
  group_by(ResponseID) %>%
  mutate(Q3.1.replaced = ifelse(Q3.1_value == "Other", Q3.2.manual, Q3.1_value)) %>%
  # In case the recoded "Other" matches an existing issue
  distinct(Q3.1.replaced) %>%  
  # Collapse all options into a single string, which is then split into a list
  mutate(Q3.1.clean_value = paste(Q3.1.replaced, collapse=","),
         Q3.1.clean_value = stri_split(Q3.1.clean_value, regex=",")) %>%
  # Keep the first row in each group, since all rows are identical
  slice(1) %>% select(-Q3.1.replaced)

# Replace "Other" with the recoded issue in Q3.2
main.issue.clean <- survey.orgs.clean.final %>%
  select(ResponseID, Q3.1_value, Q2.1, Q3.1_other_TEXT, Q3.2) %>%
  left_join(other.issues.all, by="ResponseID") %>%
  mutate(Q3.2.clean = ifelse(Q3.2 == "Other", Q3.2.manual, as.character(Q3.2)),
         Q3.2.clean = ifelse(is.na(Q3.2) & !is.na(Q3.2.manual), Q3.2.manual, Q3.2.clean)) %>%
  select(ResponseID, Q3.2.clean)


# Collapse main clean issue into high/low regime contentiousness
# This is based partially on Bush:2015's typology of regime compatibility:
# "programs that the target-country leaders view as unlikely to threaten their
# imminent survival by causing regime collapse or overthrow"
main.issue.clean %>%
  group_by(Q3.2.clean) %>%
  summarise(num = n()) %>%
  ungroup() %>% arrange(desc(num)) %>%
  mutate(potential.contentiousness = "") %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "contentiousness_WILL_BE_OVERWRITTEN.csv"))

contentiousness <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                      "handcoded_survey_stuff",
                                      "contentiousness.csv")) %>%
  select(Q3.2.clean, potential.contentiousness) %>%
  mutate(potential.contentiousness = factor(potential.contentiousness,
                                            levels=c("Low", "High"),
                                            labels=c("Low contention",
                                                     "High contention"),
                                            ordered=TRUE))


# --------------------------
# Employees and volunteers
# --------------------------
# The number of employees and volunteers requires some cleaning since it was an
# open text field in case they wanted to explain more about the number of
# employees. If the response is only numeric, count it as numeric. Otherwise,
# export responses that aren't purely numeric to CSV for hand coding.
#
# Hand coding rules:
#    - If they give a range, take the average (100-200 = 150; 10-15 = 12)
#    - If they say "thousands" or "many" use 5-based numbers, like 5,000 or 5

# CSV to work with by hand
survey.orgs.clean.final %>%
  select(ResponseID, Q3.4) %>%
  mutate(is.num = !str_detect(Q3.4, "[^0-9\\.,]"),
         Q3.4.num.manual = 0) %>%
  filter(is.num == FALSE) %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "employees_count_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSV
employees.clean <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                      "handcoded_survey_stuff",
                                      "employees_count.csv")) %>%
  select(ResponseID, Q3.4.num.manual)

# Combine the automatic and manual columns
employees.num <- survey.orgs.clean.final %>%
  select(ResponseID, Q3.4) %>%
  mutate(Q3.4.num.auto = as_num(Q3.4)$result) %>%
  left_join(employees.clean, by="ResponseID") %>%
  rowwise() %>%
  mutate(Q3.4.num = ifelse(!is.na(Q3.4.num.auto) | !is.na(Q3.4.num.manual),
                           sum(Q3.4.num.auto, Q3.4.num.manual, na.rm=TRUE),
                           NA)) %>%
  select(ResponseID, Q3.4.num)

# CSV to work with by hand
survey.orgs.clean.final %>%
  select(ResponseID, Q3.5) %>%
  mutate(is.num = !str_detect(Q3.5, "[^0-9\\.,]"),
         Q3.5.num.manual = 0) %>%
  filter(is.num == FALSE) %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "volunteers_count_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSV
volunteers.clean <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                       "handcoded_survey_stuff",
                                       "volunteers_count.csv")) %>%
  select(ResponseID, Q3.5.num.manual)

# Combine the automatic and manual columns
volunteers.num <- survey.orgs.clean.final %>%
  select(ResponseID, Q3.5) %>%
  mutate(Q3.5.num.auto = as_num(Q3.5)$result) %>%
  left_join(volunteers.clean, by="ResponseID") %>%
  rowwise() %>%
  mutate(Q3.5.num = ifelse(!is.na(Q3.5.num.auto) | !is.na(Q3.5.num.manual),
                           sum(Q3.5.num.auto, Q3.5.num.manual, na.rm=TRUE),
                           NA)) %>%
  select(ResponseID, Q3.5.num)


# -----------------------------
# Collaborative relationships
# -----------------------------
# CSV to work with by hand
survey.orgs.clean.final %>%
  filter(!is.na(Q3.6_other_TEXT)) %>%
  select(ResponseID, Q3.6_other_TEXT) %>%
  mutate(Q3.6_other.manual = "") %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "collaborations_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSV
collaborations.clean.raw <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                               "handcoded_survey_stuff",
                                               "collaborations.csv")) %>%
  select(ResponseID, Q3.6_other.manual)

collaboration.clean <- survey.orgs.clean.final %>%
  select(ResponseID, Q3.6_value) %>%
  unnest(Q3.6_value) %>%
  # Get rid of original "Other values"
  filter(Q3.6_value != "Other") %>%
  # Merge in hand-coded others
  left_join(collaborations.clean.raw, by="ResponseID") %>%
  mutate(Q3.6_other.manual = stri_split(Q3.6_other.manual, regex=",")) %>%
  unnest(Q3.6_other.manual) %>%
  mutate(Q3.6_other.manual = str_trim(Q3.6_other.manual),
         Q3.6_value = str_trim(Q3.6_value)) %>%
  group_by(ResponseID) %>%
  # Combine existing values with hand-coded values
  summarise(Q3.6_clean = list(na.omit(unique(c(Q3.6_value, Q3.6_other.manual))))) %>%
  rowwise() %>%
  # Count how many types of collaboration each organization has
  mutate(Q3.6_num = length(Q3.6_clean)) %>%
  ungroup() %>%
  # Clean up these counts. If they explicitly do not collaborate, set count to 
  # 0. If they don't now and it's their only response, set count to 0
  mutate(Q3.6_num = ifelse(str_detect(as.character(Q3.6_clean), "We do not"), 
                           0, Q3.6_num),
         Q3.6_num = ifelse(str_detect(as.character(Q3.6_clean), "Don't know") & 
                             Q3.6_num == 1, 0, Q3.6_num))


# ---------
# Funding
# ---------
# CSV to work with by hand
survey.orgs.clean.final %>%
  filter(!is.na(Q3.8_other_TEXT)) %>%
  select(ResponseID, Q3.8_other_TEXT) %>%
  mutate(Q3.8_other.manual = "") %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "funding_WILL_BE_OVERWRITTEN.csv"))

funding.not.others <- survey.orgs.clean.final %>%
  select(ResponseID, starts_with("Q3.8"), -starts_with("Q3.8_other")) %>%
  gather(key, value, -ResponseID) %>%
  mutate(key = str_replace(key, "Q3.8_", ""),
         key = recode(key,
                      corporate = "Corporate donations", 
                      foundation = "Foundation donations",
                      home_govt = "Grants from home country", 
                      host_govt = "Grants from host country",
                      individual = "Individual donations")) %>%
  filter(!(value %in% c("Not applicable", "Don't know")))

# Read in clean CSV
funding.clean.raw <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                        "handcoded_survey_stuff",
                                        "funding.csv")) %>%
  select(ResponseID, Q3.8_other.manual)

funding.others <- survey.orgs.clean.final %>%
  select(ResponseID, starts_with("Q3.8_other")) %>%
  left_join(funding.clean.raw, by="ResponseID") %>%
  select(-Q3.8_other_TEXT) %>%
  # Not all respondents consistently indicated the amount of funding from other
  # sources, so if they respond "None at all," "Don't know," or "Not
  # applicable" and they included an "Other" source, I assume "A little"
  mutate(Q3.8_other = ifelse(!is.na(Q3.8_other.manual) & 
                               Q3.8_other %in% c("Not applicable", 
                                                 "None at all", "Don't know") &
                               Q3.8_other.manual != "None",
                             "A little", as.character(Q3.8_other)),
         Q3.8_other.manual = ifelse(is.na(Q3.8_other.manual), "Other", Q3.8_other.manual)) %>%
  separate_rows(Q3.8_other.manual, sep="; ") %>%
  select(ResponseID, key = Q3.8_other.manual, value = Q3.8_other) %>%
  filter(!(value %in% c("Not applicable", "Don't know"))) %>%
  mutate(flag = TRUE)

funding.clean <- bind_rows(funding.not.others, funding.others) %>%
  # Some of the handcoded others fit in the set categories and there can
  # sometimes be mismatches as a result (e.g. an organization might say "None
  # at all" to corporate donations but then say "A great deal" to "Other" and
  # specify corporate donations). So, I group the merged data by ResponseID and
  # key and take only the top value of whatever answers there are. The
  # handcoded other data frame has a flag column to indicate that it was
  # handcoded—before taking the top row, I sort by that column so that the
  # response from the other data frame takes precedence.
  group_by(ResponseID, key) %>% arrange(flag) %>% slice(1) %>% ungroup() %>%
  select(-flag) 

funding.clean.num <- funding.clean %>%
  filter(value != "None at all") %>%
  group_by(ResponseID) %>%
  summarise(Q3.8.num = n())

funding.clean <- funding.clean %>%
  left_join(funding.clean.num, by="ResponseID") %>%
  nest(key, value, .key = "Q3.8")


# --------------------------------------
# Frequency of contact with government
# --------------------------------------
# Hand coding rules:
#    - Assume following numeric values for frequency:
#        - -1: Don't know
#        - 0: Never
#        - 1: Once every 2+ years
#        - 2: Once a year
#        - 3: Once a month
#        - 4: Once a week
#    - Add these new values:
#        - 0.5: Rarely
#        - 1.5: Unclear, like "as necessary" or "depends" or "occasionally"
#        - 2.5: More than once a year, less than once a month (also "often",
#               "regularly")
#        - 3.5: More than once a month, less than once a week
#        - 4.5: More than once a week

# CSV to work with by hand
survey.countries.clean %>%
  select(ResponseID, loop.number, Q4.5_TEXT) %>%
  filter(!is.na(Q4.5_TEXT)) %>%
  mutate(Q4.5.manual = 0) %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "frequency_govt_contact_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSV
govt.freq.clean <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                       "handcoded_survey_stuff",
                                       "frequency_govt_contact.csv")) %>%
  select(ResponseID, loop.number, Q4.5.manual)

# Combine the automatic and manual columns
govt.freq.fixed <- survey.countries.clean %>%
  select(ResponseID, loop.number, Q4.5) %>%
  mutate(Q4.5.num = case_when(
    .$Q4.5 == "Don't know" ~ -1,
    .$Q4.5 == "Never" ~ 0,
    .$Q4.5 == "Once every 2+ years" ~ 1,
    .$Q4.5 == "Once a year" ~ 2,
    .$Q4.5 == "Once a month" ~ 3,
    .$Q4.5 == "Once a week" ~ 4
  )) %>%
  left_join(govt.freq.clean, by=c("ResponseID", "loop.number")) %>%
  rowwise() %>%
  mutate(Q4.5.num = ifelse(!is.na(Q4.5) | !is.na(Q4.5.num),
                           sum(Q4.5.num, Q4.5.manual, na.rm=TRUE),
                           NA)) %>%
  mutate(Q4.5.clean = factor(Q4.5.num, levels=c(-1, seq(0, 4.5, 0.5)),
                             labels=how.often.extra, ordered=TRUE)) %>%
  select(ResponseID, loop.number, Q4.5.clean)


# --------------------------------------
# Frequency of reporting to government
# --------------------------------------
# Hand coding rules: Use same rules as contact with government above

# CSV to work with by hand
survey.countries.clean %>%
  select(ResponseID, loop.number, Q4.8_TEXT) %>%
  filter(!is.na(Q4.8_TEXT)) %>%
  mutate(Q4.8.manual = 0) %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "frequency_govt_report_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSV
govt.freq.report.clean <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                             "handcoded_survey_stuff",
                                             "frequency_govt_report.csv")) %>%
  select(ResponseID, loop.number, Q4.8.manual)

# Combine the automatic and manual columns
govt.freq.report.fixed <- survey.countries.clean %>%
  select(ResponseID, loop.number, Q4.8) %>%
  mutate(Q4.8.num = case_when(
    .$Q4.8 == "Don't know" ~ -1,
    .$Q4.8 == "Never" ~ 0,
    .$Q4.8 == "Once every 2+ years" ~ 1,
    .$Q4.8 == "Once a year" ~ 2,
    .$Q4.8 == "Once a month" ~ 3,
    .$Q4.8 == "Once a week" ~ 4
  )) %>%
  left_join(govt.freq.report.clean, by=c("ResponseID", "loop.number")) %>%
  rowwise() %>%
  mutate(Q4.8.num = ifelse(!is.na(Q4.8) | !is.na(Q4.8.num),
                           sum(Q4.8.num, Q4.8.manual, na.rm=TRUE),
                           NA)) %>%
  mutate(Q4.8.clean = factor(Q4.8.num, levels=c(-1, seq(0, 4.5, 0.5)),
                             labels=how.often.extra, ordered=TRUE)) %>%
  select(ResponseID, loop.number, Q4.8.clean)


# --------------------------
# Reactions to regulations
# --------------------------
reactions <- survey.countries.clean %>%
  select(ResponseID, loop.number, starts_with("Q4.21"), -dplyr::contains("_TEXT")) %>%
  gather(reaction, value, -c(ResponseID, loop.number)) %>%
  group_by(ResponseID, loop.number) %>%
  summarise(Q4.21_yes.total = sum(value == "Yes"),
            Q4.21_no.total = sum(value == "No"),
            Q4.21_na.total = sum(value == "Not applicable"),
            Q4.21_total.possible = 8 - Q4.21_na.total) %>%
  mutate(Q4.21_percent.changed = ifelse(Q4.21_total.possible != 0, 
                                        Q4.21_yes.total / Q4.21_total.possible,
                                        0))


# --------------------------------------------------
# -------------------------
# Deal with external data
# -------------------------
# --------------------------------------------------
# CSRE-related data
full.data <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.rds"))

# Get most recent CSRE, average CSRE over past 10 years, gwf.ever.autocracy,
# most recent Polity, average Polity over past 10 years
external.data <- full.data %>%
  select(cowcode, country, year.num, autocracy = gwf.binary, 
         csre = cs_env_sum, polity = e_polity2) %>%
  filter(year.num >= 2005)

# These external datasets don't cover all COW codes. For the models predicting
# the CSRE, this is okay, since these missing countries tend to be missing a
# ton of other variables too, so they're dropped. But with the survey analysis,
# I don't want to discount dozens of responses because they're not matched in
# the external data. So, I hand-code my own subjectivish autocracy measure for
# countries that GWF don't cover.
#
# Get a list of all COW codes present in the survey (home and target countries)
cows.all.survey <- unique(c(survey.orgs.clean.final$Q2.2_cow,
                            survey.countries.clean$Q4.1_cow))
  
# List of all COW codes in V-Dem
cows.in.external <- external.data %>%
  distinct(cowcode)

# Determine which countries don't have regime type data
cows.no.regime.type <- external.data %>%
  group_by(cowcode) %>%
  mutate(no.gwf = all(is.na(autocracy))) %>%
  ungroup() %>%
  filter(no.gwf) %>%
  distinct(cowcode)

cows.in.survey.not.in.vdem <- data_frame(cowcode = cows.all.survey) %>%
  filter(!(cowcode %in% cows.in.external$cowcode)) %>% na.omit()

cows.missing <- c(cows.no.regime.type$cowcode, cows.in.survey.not.in.vdem$cowcode)

# Determine these by hand based on Freedom House and Polity scores:
data_frame(cowcode = cows.missing) %>%
  mutate(country_name = countrycode(cowcode, "cown", "country.name")) %>%
  mutate(autocracy = "", reason = "") %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "manual_regime_types_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSV
manual.regime.types <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                          "handcoded_survey_stuff",
                                          "manual_regime_types.csv")) %>%
  select(cowcode, autocracy.manual = autocracy)

# Summarize external variables from V-Dem (captures most countries in survey)
external.data.most <- external.data %>% 
  left_join(manual.regime.types, by="cowcode") %>%
  mutate(autocracy.final = ifelse(is.na(autocracy) & !is.na(autocracy.manual),
                                  autocracy.manual, as.character(autocracy))) %>%
  group_by(cowcode) %>%
  mutate(csre.fixed = na.locf(csre, na.rm=FALSE),  # Forward-fill CSRE
         polity.fixed = na.locf(polity, na.rm=FALSE),  # Forward-fill polity
         ever.autocracy = any(autocracy.final == "Autocracy", na.rm=TRUE)) %>%
  summarise(csre.mean = mean(csre, na.rm=TRUE),
            csre.last = last(csre.fixed),
            ever.autocracy = last(ever.autocracy),
            polity.mean = mean(polity, na.rm=TRUE),
            polity.last = last(polity.fixed))

# TODO: Use actual polity for these stragglers instead of NA
# These are the countries that aren't in V-Dem but are in the survey
external.data.stragglers <- manual.regime.types %>%
  filter(!(cowcode %in% external.data.most$cowcode)) %>%
  group_by(cowcode) %>%
  summarise(csre.mean = NA, csre.last = NA, 
            ever.autocracy = any(autocracy.manual == "Autocracy"),
            polity.mean = NA, polity.last = NA)

# Finally create clean external dataframe to merge in
external.data.summary <- bind_rows(external.data.most,
                                   external.data.stragglers) %>%
  mutate(regime.type = factor(ever.autocracy, levels=c(FALSE, TRUE), 
                              labels=c("Democracy", "Autocracy")))

external.data.home <- external.data.summary %>%
  magrittr::set_colnames(paste0("home.", colnames(.)))

external.data.target <- external.data.summary %>%
  magrittr::set_colnames(paste0("target.", colnames(.)))


# ----------------------------------------------------
# --------------------------
# Create final survey data
# --------------------------
# Add clean anonymized IDs
randomly <- function(x) {
  sample(xtfrm(x))
}

clean.ids <- survey.orgs.clean.final %T>%
  {set.seed(1234)} %>%
  distinct(ResponseID) %>%
  arrange(randomly(ResponseID)) %>%
  mutate(clean.id = as.integer(seq(from=sample(1000:2000, 1),
                                   length.out=n()))) %T>%
  write_csv(., file.path(PROJHOME, "Data", "Survey", 
                         "raw_data", "clean_ids.csv"))


# Merge in hand-coded and external data
survey.orgs.clean.final.for.realz <- survey.orgs.clean.final %>%
  left_join(all.issues.clean, by="ResponseID") %>%
  left_join(main.issue.clean, by="ResponseID") %>%
  left_join(contentiousness, by="Q3.2.clean") %>%
  left_join(employees.num, by="ResponseID") %>%
  left_join(volunteers.num, by="ResponseID") %>%
  left_join(funding.clean, by="ResponseID") %>%
  left_join(collaboration.clean, by="ResponseID") %>%
  left_join(external.data.home, by=c("Q2.2_cow"="home.cowcode")) %>%
  left_join(clean.ids, by="ResponseID") %>%
  select(clean.id, everything(), -ResponseID)

survey.countries.clean.for.realz <- survey.countries.clean %>%
  left_join(govt.freq.fixed, by=c("ResponseID", "loop.number")) %>%
  left_join(govt.freq.report.fixed, by=c("ResponseID", "loop.number")) %>%
  left_join(reactions, by=c("ResponseID", "loop.number")) %>%
  left_join(external.data.target, by=c("Q4.1_cow"="target.cowcode")) %>%
  left_join(clean.ids, by="ResponseID") %>%
  select(clean.id, everything(), -ResponseID) %>%
  # Add issue area data
  left_join(left_join(select(survey.orgs.clean.final.for.realz, 
                             clean.id, main.issue = Q3.2.clean), 
                      contentiousness, 
                      by=c("main.issue" = "Q3.2.clean")),
            by="clean.id")

# Merge in hand-coded data created later
survey.orgs.clean.final.for.realz %>%
  filter(!is.na(Q3.12)) %>%
  arrange(clean.id) %>%
  select(clean.id, Q3.12) %>%
  mutate(obstacle = "") %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "obstacles_WILL_BE_OVERWRITTEN.csv"))

obstacles.coded <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                      "handcoded_survey_stuff",
                                      "obstacles.csv")) %>%
  select(clean.id, Q3.12_obstacle = obstacle)

survey.orgs.clean.final.for.realz <- survey.orgs.clean.final.for.realz %>%
  left_join(obstacles.coded, by="clean.id")


# Combine with country-level data
survey.clean.all <- survey.orgs.clean.final.for.realz %>%
  left_join(survey.countries.clean.for.realz,
            by=c("clean.id", "survey", "potential.contentiousness"))


# ------------------------
# Save all these things!
# --------------------------
saveRDS(survey.orgs.all,
        file=file.path(PROJHOME, "Data", "data_processed",
                       "survey_orgs_all.rds"))

saveRDS(partials.clean,
        file=file.path(PROJHOME, "Data", "data_processed",
                       "survey_partials.rds"))

saveRDS(survey.clean.all,
        file=file.path(PROJHOME, "Data", "data_processed",
                       "survey_clean_all.rds"))

saveRDS(survey.orgs.clean.final.for.realz, 
        file=file.path(PROJHOME, "Data", "data_processed", 
                       "survey_orgs_clean.rds"))

saveRDS(survey.countries.clean.for.realz, 
        file=file.path(PROJHOME, "Data", "data_processed", 
                       "survey_countries_clean.rds"))
