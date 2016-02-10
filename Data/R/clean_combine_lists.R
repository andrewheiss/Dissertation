library(dplyr)
library(readr)
library(readxl)
library(stringr)

# Path to data files
data.path <- file.path(PROJHOME, "Data", "data_raw", "NGO lists")


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
icso.path <- file.path(data.path, "Raw", "un-icso.json")
