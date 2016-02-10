library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(jsonlite)

# Path to data files
data.path <- file.path(PROJHOME, "Data", "data_raw", "NGO lists")

# ------------------
# Useful functions
# ------------------
# Concatenate a list or vector in a dataframe cell. Use with rowwise():
#
# data_frame(x = seq_len(3)) %>% 
#   mutate(y = replicate(3, c("a", "b", "c"), simplify=FALSE)) %>%
#   rowwise() %>%
#   mutate(y1 = concat.list(y))
#
concat.list <- function(x) {
  return(paste(x, collapse=", "))
}

# Super super crude way to filter out things that aren't e-mail addresses.
# Remove any string that doesn't have an @ in it.
#
# clean.email("asdf")
# clean.email("asdf@asdf.com")
# clean.email(NA)
#
clean.email <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  
  if (!str_detect(x, "@")) {
    return(NA_character_)
  } else {
    return(x)
  }
}

# -----------------------------------------
# Yearbook of International Organizations
# -----------------------------------------
# Load the full database
yio.db <- src_sqlite(path=file.path(data.path, "Raw", "yio.db"))

# Convert all the tables to R dataframes with collect() just so I don't have 
# to deal with raw SQL statements
yio.orgs <- tbl(yio.db, "organizations_final") %>% collect()
yio.subjects <- tbl(yio.db, "subjects") %>% collect()
yio.orgs_subjects <- tbl(yio.db, "orgs_subjects") %>% collect()
yio.contacts <- tbl(yio.db, "contacts") %>% collect()
yio.orgs_contacts <- tbl(yio.db, "orgs_contacts") %>% collect()

# Excluded organization types
i.to.ignore <- c("J", "H", "R", "S", "T", "U")
ii.to.ignore <- c("c", "d", "e", "g", "s")
iii.to.ignore <- c("Alumni and Veterans", "European Union Bodies", "FAO Bodies", 
                   "ILO Bodies", "NATO Bodies", "Parliaments", "Political Parties", 
                   "Treaties", "United Nations Bodies", "WHO Bodies", 
                   "Corporations, Companies", "Intergovernmental Communities")

yio.orgs.clean <- yio.orgs %>%
  # Ignore organizations that are classified as an excluded type
  filter(!(type_i_dir %in% i.to.ignore),
         !grepl(paste0(ii.to.ignore, collapse="|"), type_ii_dir),
         !(type_iii_dir %in% iii.to.ignore)) %>%
  select(id_org:country_hq, subject_dir, aims, activities, 
         type_i_dir:type_iii_dir, url_id, last_news) %>%
  # Join contact info table
  left_join(yio.orgs_contacts, by=c("id_org" = "fk_org")) %>%
  left_join(yio.contacts, by=c("fk_contact" = "id_contact"))

yio.orgs.email <- yio.orgs.clean %>%
  select(-fk_contact) %>%
  filter(!is.na(contact_email))

write_csv(yio.orgs.clean, path=file.path(data.path, "Clean", "yio_clean.csv"))
write_csv(yio.orgs.email, path=file.path(data.path, "Clean", "yio_emails.csv"))


# ----------------------------------------
# Directory of Development Organizations
# ----------------------------------------
# GAH STUPID EXCEL AND ITS DESTRUCTION OF UNICODE
fixes <- c("aÌ\\?" = "á", "a\\?"  = "á", "aÌ"    = "á", "aÌƒ"   = "ã",
           "aƒ"    = "ã", "aÌˆ"   = "ä", "aÌ€"   = "à", "eÌ\\?" = "é",
           "Ì\\?E" = "É", "e\\?"  = "é", "eÌ€"   = "è", "eÌ‚"   = "ê", 
           "eÌ"    = "è", "i\\?"  = "í", "iÌ\\?" = "í", "iÌˆ"   = "ï",
           "iˆ"    = "ï", "IÌ‚"   = "Î", "oÌ\\?" = "ó", "o\\?"  = "ó",
           "oÌ€"   = "ò", "OÌˆ"   = "Ö", "oÌ‚"   = "ô", "oÌƒ"   = "õ",
           "uÌ\\?" = "ú", "u\\?"  = "ú", "uÌ"    = "ú", "uÌˆ"   = "ü",
           "â€œ"   = "“", "â€\\?" = "”", "â€“"   = "—", "nÌƒ"   = "ñ",
           "ZÌŒ"   = "Ž", "cÌ§"   = "ç", "cÌ"    = "ç", "UÌ\\?" = "Ú",
           "AÌ\\?" = "Á", "E\\?"  = "E", "EÌ\\?" = "É", "OÌ\\?" = "Ó",
           "ú\\?c" = "úc", "O\\?N" = "ÓN", "A\\?" = "Á",
           "p\\?r" = "pér", "B\\?r"   = "Bær", "Q Ì\\?eqchi Ì\\?" = "Q'eqchi'")

ddo.clean <- read_excel(file.path(data.path, "Raw", "ddo_resaved.xlsx"), 
                        sheet="Final NGO Contact List") %>%
  mutate(full_text = str_replace_all(full_text, fixes),
         full_name = str_replace_all(full_name, fixes),
         address = str_replace_all(address, fixes))

ddo.emails <- ddo.clean %>%
  filter(!is.na(email))

write_csv(ddo.clean, path=file.path(data.path, "Clean", "ddo_clean.csv"))
write_csv(ddo.emails, path=file.path(data.path, "Clean", "ddo_emails.csv"))


# ---------------------------------------------------------
# UN Integrated Civil Society Organizations System (iCSO)
# ---------------------------------------------------------
icso.path <- file.path(data.path, "Raw", "un-icso-clean.json")

# TinyDB names each entry with an index, and jsonlite chokes on named entries,
# so we have to unname everything
icso.raw.json <- fromJSON(icso.path)$`_default`
names(icso.raw.json) <- NULL

# As it stands now, individual strings are saved as one-element lists, not
# single entries, which makes R's dataframes choke. The easiest way to fix this
# is to use toJSON(..., auto_unbox=TRUE) and unbox everything. So, convert the
# parsed list back to JSON back to a list.
icso.raw.df <- fromJSON(toJSON(icso.raw.json, auto_unbox=TRUE), 
                        simplifyDataFrame=TRUE, flatten=TRUE)

icso.clean <- icso.raw.df %>%
  filter(!(geographic_scope %in% c("Local", "National")),
         !is.na(geographic_scope)) %>%
  filter(!is.na(hq_email) | !is.na(preferred_email))

# MAYBE: Check that area of activity is different from HQ address?
# No. That's unreliable. Plus, in the case of Amnesty Egypt, Chad, Chile, and
# Australia, they're pseudo-branches of the main Amnesty and still count as
# INGOs, even though their HQ countries and target countries are the same.

# Combine hq_ and preferred_email into one variable and remove duplicates
icso.emails <- icso.clean %>%
  gather(email_type, email, hq_email, preferred_email) %>%
  distinct(email) %>%
  select(org_id, organizations_name, hq_address, languages, 
         email, email_type, year_established, web_site, 
         geographic_scope, country_geographical_area_of_activity,
         number_and_type_of_members, mission_statement) %>%
  rowwise() %>%
  mutate(languages = concat.list(languages),
         country_geographical_area_of_activity = 
           concat.list(country_geographical_area_of_activity))

write_csv(icso.emails, path=file.path(data.path, "Clean", "un-icso_emails.csv"))


# ------------------------------------
# Global Anti-Human Trafficking NGOs
# ------------------------------------
tip.raw <- read_excel(file.path(data.path, "Raw", "tip-ngos.xlsx"),
                      sheet="Main list", na=".")

tip.clean <- tip.raw %>%
  filter(!str_detect(`Survey status`, "dead")) %>%
  select(ID, `Organization name`, `Clean name`,
         Country, ISO3, Region, Website, Description,
         `Email 1`, `Email 2`, `Email 3`, `Contact email`,
         `Contact name`)

tip.emails <- tip.clean %>%
  rowwise() %>%
  mutate_each(funs(clean.email), contains("mail")) %>%
  gather(email_type, email, contains("mail")) %>%
  distinct(email)

write_csv(tip.emails, path=file.path(data.path, "Clean", "tip-ngos_emails.csv"))
