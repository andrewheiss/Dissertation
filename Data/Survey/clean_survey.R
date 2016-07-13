library(dplyr)
library(tidyr)
library(readr)
library(stringi)
library(stringr)
library(purrr)
library(feather)


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
countries.raw <- read_feather(file.path(PROJHOME, "Data", "Survey", "output",
                                        "survey_countries.feather"))

# Change Greenland > Anguilla; Aruba > Kosovo; Cayman Islands > Taiwan
countries.manual.fixes <- data_frame(`Country name` = c("Anguilla", 
                                                        "Kosovo", "Taiwan"),
                                     ISO3 = c("AIA", "XKX", "TWN"),
                                     `COW code` = c(1022, 347, 713),
                                     `Qualtrics ID` = c(74, 10, 36))

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
  filter(ResponseID != "R_12564WimpZZKn5A")


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


# ----------------------------
# Final combined survey data
# ----------------------------
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
  select(ResponseID, Q4.5_TEXT) %>%
  filter(!is.na(Q4.5_TEXT)) %>%
  mutate(Q4.5.manual = 0) %>%
  write_csv(file.path(PROJHOME, "Data", "data_processed",
                      "handcoded_survey_stuff",
                      "frequency_govt_contact_WILL_BE_OVERWRITTEN.csv"))

# Read in clean CSV
govt.freq.clean <- read_csv(file.path(PROJHOME, "Data", "data_processed",
                                       "handcoded_survey_stuff",
                                       "frequency_govt_contact.csv")) %>%
  select(ResponseID, Q4.5.manual)

# Combine the automatic and manual columns
govt.freq.fixed <- survey.countries.clean %>%
  select(ResponseID, Q4.5) %>%
  mutate(Q4.5.num = case_when(
    .$Q4.5 == "Don't know" ~ -1,
    .$Q4.5 == "Never" ~ 0,
    .$Q4.5 == "Once every 2+ years" ~ 1,
    .$Q4.5 == "Once a year" ~ 2,
    .$Q4.5 == "Once a month" ~ 3,
    .$Q4.5 == "Once a week" ~ 4
  )) %>%
  left_join(govt.freq.clean, by="ResponseID") %>%
  rowwise() %>%
  mutate(Q4.5.num = ifelse(!is.na(Q4.5) | !is.na(Q4.5.num),
                           sum(Q4.5.num, Q4.5.manual, na.rm=TRUE),
                           NA)) %>%
  mutate(Q4.5.clean = factor(Q4.5.num, levels=c(-1, seq(0, 4.5, 0.5)),
                             labels=how.often.extra, ordered=TRUE)) %>%
  select(ResponseID, Q4.5.clean)


# --------------------------
# Merge in hand-coded data
# --------------------------
survey.orgs.clean.final.for.realz <- survey.orgs.clean.final %>%
  left_join(employees.num, by="ResponseID") %>%
  left_join(volunteers.num, by="ResponseID")

survey.countries.clean.for.realz <- survey.countries.clean %>%
  left_join(govt.freq.fixed, by="ResponseID")

# Combine with country-level data
survey.clean.all <- survey.orgs.clean.final.for.realz %>%
  left_join(survey.countries.clean.for.realz, by=c("ResponseID", "survey"))


# ------------------------
# Save all these things!
# --------------------------
# Not using feather because it can't handle list columns yet
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
