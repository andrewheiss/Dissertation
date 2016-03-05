library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(countrycode)


#-----------------------
# Load full ICEWS data
#-----------------------
clean.files.dir <- file.path(PROJHOME, "Data", "data_raw", 
                             "External", "ICEWS", "cleaned")
original.files.dir <- file.path(PROJHOME, "Data", "data_raw","External",
                                "ICEWS", "study_28075", "Data")

# If the raw ICEWS files haven't been cleaned yet, do that.
if (length(list.files(clean.files.dir)) == 0) {
  bin <- file.path(PROJHOME, "Data", "Python", "fix_icews.py")
  system(paste("python3", bin, original.files.dir, clean.files.dir))
}

# Load all events from the cleaned, escaped data
# Each year is saved as a zipped .tab file. This applies readr::read_tsv to
# each file and then combines them all into one huge dataframe with
# dplyr::bind_rows
all.events <- list.files(clean.files.dir) %>%
  file.path(clean.files.dir, .) %>%
  lapply(., FUN=read_tsv) %>%
  bind_rows

# Check that there aren't any gaps in coverage by plotting daily event counts
# plot.data <- all.events %>%
#   group_by(`Event Date`) %>%
#   summarize(Count = n())
# 
# ggplot(plot.data, aes(x=`Event Date`, y=Count)) + 
#   geom_bar(stat="identity") + 
#   labs(title="All ICEWS events") + 
#   theme_light(10)


# Load CAMEO QuadClass categories
# Original, old mapping:
#   http://eventdata.parusanalytics.com/papers.dir/Gerner.APSA.02.pdf
# New mapping: 
#   http://phoenixdata.org/description
cameo.categories <- read_csv(file.path(PROJHOME, "Data", "data_raw", 
                                       "External", "ICEWS", 
                                       "cameo_categories.csv")) %>%
  select(cameo.root = `Root CAMEO`, cameo.description = Description,
         cameo.category = `Quad Class Description`) %>%
  mutate(cameo.category.agg = ifelse(str_detect(cameo.category, "Conflict"), 
                                     "Conflict",
                                     ifelse(str_detect(cameo.category, 
                                                       "Cooperation"), 
                                            "Cooperation", cameo.category)))


# Filter the raw data and merge in CAMEO categories
events.filtered <- all.events %>%
  # Only interstate events
  filter(`Source Country` != `Target Country`) %>%
  # Only state actors
  filter(str_detect(`Source Sectors`, "Government|Military") |
           str_detect(`Target Sectors`, "Government|Military")) %>%
  # Categorize CAMEO roots
  mutate(cameo.root = as.integer(str_sub(`CAMEO Code`, 1, 2))) %>%
  left_join(cameo.categories, by="cameo.root") %>%
  # Other miscellaneous variables
  mutate(event.year = year(`Event Date`))

events.aggregated <- events.filtered %>%
  filter(cameo.category != "Neutral") %>%
  group_by(`Target Country`, event.year, cameo.category.agg) %>%
  # group_by(`Target Country`, event.year) %>%
  summarise(num = n(), severity = mean(Intensity)) %>%
  ungroup()

events.conflict.coop <- events.aggregated %>%
  gather(variable, value, 
         -c(`Target Country`, event.year, cameo.category.agg)) %>%
  unite(temp, cameo.category.agg, variable) %>%
  spread(temp, value, fill=0) %>%
  mutate(icews.num.events = Cooperation_num + Conflict_num,
         icews.net.num = Cooperation_num - Conflict_num,
         icews.pct.shame = Conflict_num / (icews.num.events),
         icews.net.severity = Cooperation_severity - Conflict_severity,
         icews.conflict.severity.abs = abs(Conflict_severity),
         cowcode = countrycode(`Target Country`, "country.name", "cown"),
         cowcode = ifelse(`Target Country` == "Serbia", 345, cowcode),
         cowcode = ifelse(`Target Country` == "Hong Kong", 715, cowcode)) %>%
  filter(icews.num.events > 50) %>% 
  select(-`Target Country`)

# INGO conflictual/cooperative events
events.filtered.ingos <- all.events %>%
  # Only interstate events
  filter(`Source Country` != `Target Country`) %>%
  # Only INGOs targeting state actors
  filter(str_detect(`Source Sectors`, "Nongovernmental Organization \\(International\\)"),
         str_detect(`Target Sectors`, "Government|Military")) %>%
  # Categorize CAMEO roots
  mutate(cameo.root = as.integer(str_sub(`CAMEO Code`, 1, 2))) %>%
  left_join(cameo.categories, by="cameo.root") %>%
  # Other miscellaneous variables
  mutate(event.year = year(`Event Date`))

events.aggregated.ingos <- events.filtered.ingos %>%
  filter(cameo.category != "Neutral") %>%
  group_by(`Target Country`, event.year, cameo.category.agg) %>%
  summarise(num = n(), severity = mean(Intensity)) %>%
  ungroup()

events.conflict.coop.ingos <- events.aggregated.ingos %>%
  gather(variable, value, 
         -c(`Target Country`, event.year, cameo.category.agg)) %>%
  unite(temp, cameo.category.agg, variable) %>%
  spread(temp, value, fill=0) %>%
  mutate(icews.num.events.ingos = Cooperation_num + Conflict_num,
         icews.net.num.ingos = Cooperation_num - Conflict_num,
         icews.pct.shame.ingos = Conflict_num / (icews.num.events.ingos),
         icews.net.severity.ingos = Cooperation_severity - Conflict_severity,
         icews.conflict.severity.abs.ingos = abs(Conflict_severity),
         cowcode = countrycode(`Target Country`, "country.name", "cown"),
         cowcode = ifelse(`Target Country` == "Serbia", 345, cowcode),
         cowcode = ifelse(`Target Country` == "Hong Kong", 715, cowcode)) %>%
  rename(Cooperation_num.ingos = Cooperation_num, 
         Cooperation_severity.ingos = Cooperation_severity,
         Conflict_num.ingos = Conflict_num,
         Conflict_severity.ingos = Conflict_severity) %>%
  select(-`Target Country`)


# Save aggregated data
saveRDS(events.conflict.coop, 
        file=file.path(PROJHOME, "Data", "data_processed",
                       "icews_aggregated.Rds"))

saveRDS(events.conflict.coop.ingos, 
        file=file.path(PROJHOME, "Data", "data_processed",
                       "icews_aggregated_ingos.Rds"))
