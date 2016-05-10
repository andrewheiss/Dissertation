library(dplyr)
library(tidyr)
library(readr)
library(stringr)


# ----------------------------------------------------------
# Clean bounces, blocks, unsubscribes, and invalid e-mails
# ----------------------------------------------------------
db.email <- src_sqlite(path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "final_list.db"))
removed <- tbl(db.email, "remove") %>% collect()
bounced <- tbl(db.email, "bounces") %>% collect()
email.full <- tbl(db.email, "full_list") %>% collect() %>%
  rowwise() %>%
  mutate(domain = str_match(email, "@(.*)$")[2]) %>%
  ungroup()

blocks.raw <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
                                      "suppressions", "suppression_blocks.csv")) %>%
  mutate(dead_domain = ifelse(str_detect(reason, "MX"), TRUE, FALSE))

# Dead domains
dead.domains <- blocks.raw %>%
  filter(dead_domain == TRUE) %>%
  rowwise() %>%
  mutate(domain = str_match(email, "@(.*)$")[2]) %>%
  select(dead_domain, domain) %>% 
  ungroup()

ids.to.remove.dead.domain <- email.full %>%
  mutate(dead_domain = ifelse(domain %in% dead.domains$domain, TRUE, FALSE)) %>%
  filter(dead_domain == TRUE) %>%
  filter(!(index_org %in% removed$fk_org))

sql.command <- ids.to.remove.dead.domain %>%
  mutate(remove = 1, remove_notes = "Domain is dead; has no mail server") %>%
  select(fk_org = index_org, remove, remove_notes)
write_csv(sql.command, 
          file.path(PROJHOME, "Data", "Survey",
                    "sql_csvs", "suppression_blocks.csv"))


# Bounces (in both blocks.raw and bounces.raw)
bounces.blocks <- blocks.raw %>% filter(dead_domain == FALSE) %>% select(-dead_domain)
bounces.raw <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
                                       "suppressions", "suppression_bounces.csv"))

bounces.all <- bind_rows(bounces.raw, bounces.blocks) %>%
  left_join(select(email.full, index_org, email), by="email")

bounces.unknown <- bounces.all %>% filter(is.na(index_org))

ids.to.remove.bounces <- email.full %>%
  filter(index_org %in% bounces.all$index_org) %>%
  filter(!(index_org %in% bounced$fk_org))

sql.command <- ids.to.remove.bounces %>%
  mutate(hard_bounce = 1, email_dead = 0, email_bounce = 0) %>%
  select(fk_org = index_org, hard_bounce, email_dead, email_bounce)
write_csv(sql.command, 
          file.path(PROJHOME, "Data", "Survey",
                    "sql_csvs", "bounces.csv"))


# Unsubcribed
unsub.raw <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
                                     "suppressions", "suppression_unsubscribes.csv"))

ids.to.remove.unsub <- email.full %>%
  filter(email %in% unsub.raw$email) %>%
  filter(!(index_org %in% removed$fk_org))

sql.command <- ids.to.remove.unsub %>%
  mutate(remove = 1, remove_notes = "Unsubscribed") %>%
  select(fk_org = index_org, remove, remove_notes)
write_csv(sql.command, 
          file.path(PROJHOME, "Data", "Survey",
                    "sql_csvs", "unsubscribes.csv"))


# Invalid addresses
invalid.raw <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
                                       "suppressions", "suppression_invalid_emails.csv"))

ids.to.remove.invalid <- email.full %>%
  filter(email %in% invalid.raw$email) %>%
  filter(!(index_org %in% removed$fk_org))

sql.command <- ids.to.remove.invalid %>%
  mutate(remove = 1, remove_notes = "Invalid address") %>%
  select(fk_org = index_org, remove, remove_notes)
write_csv(sql.command, 
          file.path(PROJHOME, "Data", "Survey",
                    "sql_csvs", "invalid.csv"))


# -----------------------------
# Export email sending groups
# -----------------------------
db.email <- src_sqlite(path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "final_list.db"))
email.full <- tbl(db.email, "full_list") %>% collect()
removed <- tbl(db.email, "remove") %>% collect()
bounced <- tbl(db.email, "bounces") %>% collect()
completed <- tbl(db.email, "survey_completed") %>% collect()

# Super clean e-mails from Email Hippo
hippo.raw <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
                                     "suppressions", "hippo_check.csv"))

hippo.clean <- hippo.raw %>% filter(Status == "Ok")


current.group <- 28

ids.to.skip <- c(removed$fk_org, bounced$fk_org, completed$fk_org)
ids.clean <- hippo.clean$index_org

to.send <- email.full %>%
  filter(!(index_org %in% ids.to.skip)) %>%
  filter(index_org %in% ids.clean) %>%
  filter(group == current.group) %>%
  mutate(email = strsplit(as.character(email), ",")) %>%
  unnest(email) %>%
  select(index_org, id_org, org_name_email, email, group)

filename <- paste0("round_", current.group, "_super_clean.csv")
write_csv(to.send, path=file.path(PROJHOME, "Data", "Survey", "list",
                                  "groups_clean_hippo", filename),
          na="")

# all.groups <- email.full %>%
#   filter(!(index_org %in% ids.to.skip)) %>%
#   filter(!(index_org %in% to.send.hippo$id_org)) %>%
#   filter(!(group %in% as.character(1:8))) %>%
#   mutate(email = strsplit(as.character(email), ",")) %>%
#   unnest(email) %>%
#   select(index_org, id_org, org_name_email, email, group)
# write_csv(all.groups, "~/Desktop/hippo_check.csv")


# # Email Hippo
# hippo <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
#                                  "suppressions", "hippo_check.csv"))
# 
# hippo.dead <- hippo %>%
#   filter(Status == "Bad") %>%
#   filter(AdditionalStatusInfo != "MailboxDoesNotExist") 
# 
# ids.to.remove.hippo.dead <- email.full %>%
#   filter(index_org %in% hippo.dead$index_org) %>%
#   filter(!(index_org %in% bounced$fk_org)) %>%
#   filter(!(index_org %in% removed$fk_org))
# 
# hippo.sql.dead <- ids.to.remove.hippo.dead %>%
#   mutate(remove = 1, remove_notes = "Hippo: domain dead") %>%
#   select(fk_org = index_org, remove, remove_notes)
# write_csv(hippo.sql.dead, "~/Desktop/mail_cleaning/hippo_domain_dead.csv")
# 
# hippo.email.dead <- hippo %>%
#   filter(Status == "Bad") %>%
#   filter(AdditionalStatusInfo == "MailboxDoesNotExist") 
# 
# ids.to.remove.hippo.email.dead <- email.full %>%
#   filter(index_org %in% hippo.email.dead$index_org) %>%
#   filter(!(index_org %in% bounced$fk_org)) %>%
#   filter(!(index_org %in% removed$fk_org))
# 
# hippo.sql.dead.email <- ids.to.remove.hippo.email.dead %>%
#   mutate(remove = 1, remove_notes = "Hippo: e-mail doesn't exist") %>%
#   select(fk_org = index_org, remove, remove_notes)
# write_csv(hippo.sql.dead.email, "~/Desktop/mail_cleaning/hippo_email_dead.csv")

# hippo.clean <- hippo %>%
#   filter(Status != "Bad")
# write_csv(hippo.clean, "~/Desktop/round_9_clean_hippo_clean.csv")



# ------------------------
# 1st round of reminders
# ------------------------
db.email <- src_sqlite(path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "final_list.db"))
email.full <- tbl(db.email, "full_list") %>% collect()
removed <- tbl(db.email, "remove") %>% collect()
bounced <- tbl(db.email, "bounces") %>% collect()
completed <- tbl(db.email, "survey_completed") %>% collect()

wants.to.call <- read_ods(file.path(PROJHOME, "Data", "Survey", 
                                    "sql_csvs", "sql_queries.ods"),
                          sheet="wants_to_call")

contact.so.else  <- read_ods(file.path(PROJHOME, "Data", "Survey", 
                                       "sql_csvs", "sql_queries.ods"),
                             sheet="contact_someone_else")

# Super clean e-mails from Email Hippo
hippo.raw <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
                                     "suppressions", "hippo_check.csv"))

hippo.clean <- hippo.raw %>% filter(Status == "Ok")

# Filter out Mailgun unsubscribes and bounces
# mg.unsubs <- read_csv(file.path(PROJHOME, "Data", "Survey",
#                                 "suppressions", "mg_unsubscribes.csv"))
# 
# mg.bounces <- read_csv(file.path(PROJHOME, "Data", "Survey",
#                                  "suppressions", "mg_bounces.csv"))
# 
# unsub.sql <- mg.unsubs %>%
#   left_join(select(email.full, index_org, email), by="email") %>%
#   filter(!is.na(index_org)) %>%
#   filter(!(index_org %in% removed$fk_org)) %>%
#   mutate(remove = 1, remove_notes = "Unsubscribed") %>%
#   select(fk_org = index_org, remove, remove_notes)
# 
# write_csv(unsub.sql,
#           file.path(PROJHOME, "Data", "Survey",
#                     "sql_csvs", "mg_unsubscribes.csv"))
# 
# bounces.sql <- mg.bounces %>%
#   left_join(select(email.full, index_org, email), by="email") %>%
#   filter(!is.na(index_org)) %>%
#   filter(!(index_org %in% bounced$fk_org)) %>%
#   mutate(hard_bounce = 1, email_dead = 0, email_bounce = 0) %>%
#   select(fk_org = index_org, hard_bounce, email_dead, email_bounce)
# 
# write_csv(bounces.sql,
#           file.path(PROJHOME, "Data", "Survey",
#                     "sql_csvs", "mg_bounces.csv"))


ids.to.skip <- c(removed$fk_org, bounced$fk_org, completed$fk_org,
                 wants.to.call$index_org, contact.so.else$fk_org)

reminders <- email.full %>%
  filter(!(index_org %in% ids.to.skip)) %>%
  filter(index_org %in% ids.clean) %>%
  filter(group %in% as.character(10:28)) %>%
  mutate(email = strsplit(as.character(email), ",")) %>%
  unnest(email) %>%
  select(index_org, id_org, org_name_email, email, group)


# Check these at Email Hippo
#
#       $$$$$$$$$$$$
#       $          $
#       $    :(    $
#       $          $
#       $$$$$$$$$$$$
#
# write_csv(reminders, path="~/Desktop/early_groups.csv")
# 
# 
# # Filter out dead addresses
# hippo.early <- read_csv(file=file.path(PROJHOME, "Data", "Survey",
#                                        "suppressions", "hippo_early_groups.csv"))
#  
# hippo.clean <- hippo.early %>% filter(Status == "Ok")
# 
# reminders.1.8 <- reminders %>%
#   filter(index_org %in% hippo.clean$index_org) %>%
#   filter(group %in% as.character(1:8))


# Save individual group lists
writer <- function(df) {
  filename <- paste0("group_", unique(df$group), "_clean.csv")
  readr::write_csv(df, path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "groups_reminders", filename),
                   na="")
  return(df)
}

reminders %>%
  group_by(group) %>%
  do(writer(.))
