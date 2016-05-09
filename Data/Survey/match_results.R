library(dplyr)
library(readr)
library(feather)
library(readODS)

countries <- read_feather(file.path(PROJHOME, "Data", "Survey", "output",
                                    "survey_countries.feather"))

# Pilot-ish survey
# Qualtrics isn't letting me download the full raw CSV file, so this is a
# subset, which uses a slightly different structure and needs to be rearranged
# results.colnames <- c("Q2.1", "Q2.2", "Q2.4", "ResponseID", "EndDate", "Finished", "Q5.4")
# 
# survey.raw <- read_csv(results.file, col_names=results.colnames, skip=2)
# 
# responses.matching <- survey.raw %>%
#   select(ResponseID, EndDate, Finished, Q2.1, Q2.2, Q2.4, Q5.4) %>%
#   arrange(EndDate)

results.file <- file.path(PROJHOME, "Data", "Survey", "raw_data", 
                          "INGOs_and_government_regulations.csv")

results.colnames <- read_csv(results.file, col_names=FALSE, n_max=1) %>%
  c %>% unlist %>% unname

survey.raw <- read_csv(results.file, col_names=results.colnames, skip=3)

responses.matching.pilot <- survey.raw %>%
  mutate(Finished = Finished == 1,
         Q2.4 = Q2.4 == 1) %>%
  left_join(select(countries, c(`Country name`, `Qualtrics ID`)),
            by=c("Q2.2" = "Qualtrics ID")) %>%
  select(ResponseID, EndDate, Finished, `Organization name` = Q2.1, `Country name`, 
         `Is INGO` = Q2.4, `Contact` = Q5.4)

# Final survey
results.file <- file.path(PROJHOME, "Data", "Survey", "raw_data", 
                          "INGOs_and_government_regulations_new.csv")

results.colnames <- read_csv(results.file, col_names=FALSE, n_max=1) %>%
  c %>% unlist %>% unname

survey.raw <- read_csv(results.file, col_names=results.colnames, skip=3)

responses.matching.full <- survey.raw %>%
  mutate(Finished = Finished == 1,
         Q2.4 = Q2.4 == 1) %>%
  left_join(select(countries, c(`Country name`, `Qualtrics ID`)),
            by=c("Q2.2" = "Qualtrics ID")) %>%
  select(ResponseID, EndDate, Finished, `Organization name` = Q2.1, `Country name`, 
         `Is INGO` = Q2.4, `Contact` = Q5.4)

responses.matching <- bind_rows(responses.matching.pilot,
                                responses.matching.full)

responses.matching.completed <- responses.matching %>%
  filter(Finished == TRUE)


# Load database and get list of already-recorded surveys
db.email <- src_sqlite(path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "final_list.db"))
email.full <- tbl(db.email, "full_list") %>% collect()
completed <- tbl(db.email, "survey_completed") %>% collect()

unknown.completed <- read_ods(file.path(PROJHOME, "Data", "Survey", 
                                        "sql_csvs", "sql_queries.ods"),
                              sheet="unknown_completed")

responses.to.be.matched <- responses.matching.completed %>%
  filter(!(ResponseID %in% c(completed$qualtrics_id, unknown.completed$qualtrics_id)))

write_csv(responses.to.be.matched,
          file.path(PROJHOME, "Data", "Survey", "output",
                    "responses_to_be_matched.csv"))

# Lookup table
org.lookup <- email.full %>%
  select(index_org, id_org, org_name, email, org_url, country_hq)

write_csv(org.lookup,
          file.path(PROJHOME, "Data", "Survey", "output",
                    "org_lookup.csv"))
