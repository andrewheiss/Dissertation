library(dplyr)
library(readr)
library(tidyr)
library(stringr)

all.emails.raw <- read_csv(file.path(PROJHOME, "Data", "data_raw", 
                                     "NGO lists", "Clean", "all_emails.csv"))

duplicates <- all.emails.raw %>%
  group_by(email) %>%
  filter(n() > 1)

duplicates.to.ignore <- c("tip_315", "tip_18")

all.emails.final <- all.emails.raw %>%
  filter(!(id_org %in% duplicates.to.ignore)) %>%
  mutate(org_name_email = ifelse(is.na(org_name), "your organization", org_name))

# all.emails.final %>%
#   write_csv(path=file.path(PROJHOME, "Data", "Survey", "list", "final_list.csv"),
#             na="")

set.seed(1234)
first.round <- all.emails.final %>%
  sample_n(100)

first.round %>%
  write_csv(path=file.path(PROJHOME, "Data", "Survey", "list", 
                           "groups", "first_round_raw.csv"),
            na="")

first.round <- read_csv(file.path(PROJHOME, "Data", "Survey", 
                                  "list", "groups", "first_round_done.csv"))
first.round.ids <- first.round$id_org

set.seed(1234)
round.2 <- all.emails.final %>%
  filter(!(id_org %in% first.round.ids)) %>%
  sample_n(1000)

round.2 %>%
  write_csv(path=file.path(PROJHOME, "Data", "Survey", "list", 
                           "groups", "round_2_raw.csv"),
            na="")

round.2 <- read_csv(file.path(PROJHOME, "Data", "Survey", 
                              "list", "groups", "round_2_done.csv"))
round.2.ids <- round.2$id_org

set.seed(1234)
round.3 <- all.emails.final %>%
  filter(!(id_org %in% c(first.round.ids, round.2.ids))) %>%
  sample_n(1000)

round.3 %>%
  write_csv(path=file.path(PROJHOME, "Data", "Survey", "list", 
                           "groups", "round_3_raw.csv"),
            na="")
round.3 <- read_csv(file.path(PROJHOME, "Data", "Survey", 
                              "list", "groups", "round_3_done.csv"))
round.3.ids <- round.3$id_org

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Ugh. This is the most inefficient way ever to assign groups. 
# Here's a better way.

# Collect all the ids from previous rounds
round.1.ids.df <- data_frame(id_org = first.round.ids, group = 1)
round.2.ids.df <- data_frame(id_org = round.2.ids, group = 2)
round.3.ids.df <- data_frame(id_org = round.3.ids, group = 3)

already.assigned.ids <- bind_rows(round.1.ids.df, round.2.ids.df, round.3.ids.df)

# Filter out the organizations from rounds 1-3 and assign the rest to groups of
# roughly 1000 organizations
set.seed(1234)
not.assigned.ids <- all.emails.final %>%
  filter(!(id_org %in% already.assigned.ids$id_org)) %>%
  rowwise() %>%
  mutate(group = sample(4:28, 1, replace=TRUE)) %>%
  select(id_org, group)

# Make a master lookup table of all organization ids and groups
all.ids <- bind_rows(already.assigned.ids, not.assigned.ids)

# Merge group assignments into full e-mail list
# id_org isn't unique for whatever reason, so get unique combinations of id,
# group, and e-mail with distinct
all.emails.final.assigned <- all.emails.final %>%
  left_join(all.ids, by="id_org") %>%
  distinct(id_org, group, .keep_all=TRUE) %>%
  distinct(id_org, email, .keep_all=TRUE)

# Check final group size
all.emails.final.assigned %>% group_by(group) %>% summarise(total = n())

# Save final CSV
all.emails.final.assigned %>%
  write_csv(path=file.path(PROJHOME, "Data", "Survey", "list",
                           "final_list_assigned_WILL_BE_OVERWRITTEN.csv"),
            na="")

# Save individual group lists
writer <- function(df) {
  filename <- paste0("round_", unique(df$group), "_raw.csv")
  readr::write_csv(df, path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "groups", filename),
                   na="")
  return(df)
}

all.emails.final.assigned %>%
  select(id_org, org_name_email, email, group) %>%
  group_by(group) %>%
  do(writer(.))
