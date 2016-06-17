library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(lubridate)
library(stringr)
library(ggplot2)
library(countrycode)
library(testthat)


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
all.events.raw <- list.files(clean.files.dir) %>%
  file.path(clean.files.dir, .) %>%
  lapply(., FUN=read_tsv) %>%
  bind_rows %>%
  mutate(event.year = year(`Event Date`))

# Check that there aren't any gaps in coverage by plotting daily event counts
# plot.data <- all.events.raw %>%
#   group_by(`Event Date`) %>%
#   summarize(Count = n())
# 
# ggplot(plot.data, aes(x=`Event Date`, y=Count)) +
#   geom_bar(stat="identity") +
#   labs(title="All ICEWS events") +
#   theme_light(10)

all.events.interstate <- all.events.raw %>%
  # Only interstate events
  filter(`Source Country` != `Target Country`) %>%
  # Assume that anything without a sector is government-based (i.e. "The United
  # States condemned China")
  mutate(`Source Sectors` = ifelse(is.na(`Source Sectors`),
                                   "Government imputed", `Source Sectors`),
         `Target Sectors` = ifelse(is.na(`Target Sectors`),
                                   "Government imputed", `Target Sectors`)) %>%
  mutate(`Source Sectors list` = str_split(`Source Sectors`, ","),
         `Target Sectors list` = str_split(`Target Sectors`, ",")) %>%
  # Create flags with regex because rowwise() + any(... %in% ...) is really
  # really really really really slow
  mutate(govt.source = str_detect(`Source Sectors`,
                                  "Government"),
         govt.target = str_detect(`Target Sectors`,
                                  "Government,|Government$|Government imputed"),
         ingo.source = str_detect(`Source Sectors`,
                                  "Nongovernmental Organization"),
         party.source = str_detect(`Source Sectors`, "Parties"))


baseline.all.events.target <- all.events.raw %>%
  group_by(`Target Country`, event.year) %>%
  summarise(all.events.target = n()) %>%
  ungroup() %>%
  mutate(cowcode.target = countrycode(`Target Country`, "country.name", "cown")) %>%
  filter(!is.na(`Target Country`)) %>%  # Get rid of missing countries
  mutate(cowcode.target = case_when(
    .$`Target Country` == "Hong Kong" ~ as.integer(715),
    .$`Target Country` == "Serbia" ~ as.integer(340),
    .$`Target Country` == "Occupied Palestinian Territory" ~ as.integer(669),
    TRUE ~ .$cowcode.target
  )) %>%
  filter(!is.na(cowcode.target))  # Get rid of tiny and non-statey states

baseline.all.events.source <- all.events.raw %>%
  group_by(`Source Country`, event.year) %>%
  summarise(all.events.source = n()) %>%
  ungroup() %>%
  mutate(cowcode.source = countrycode(`Source Country`, "country.name", "cown")) %>%
  filter(!is.na(`Source Country`)) %>%  # Get rid of missing countries
  mutate(cowcode.source = case_when(
    .$`Source Country` == "Hong Kong" ~ as.integer(715),
    .$`Source Country` == "Serbia" ~ as.integer(340),
    .$`Source Country` == "Occupied Palestinian Territory" ~ as.integer(669),
    TRUE ~ .$cowcode.source
  )) %>%
  filter(!is.na(cowcode.source))  # Get rid of tiny and non-statey states

baseline.govt.govt <- all.events.interstate %>%
  filter(govt.source, govt.target) %>%
  group_by(`Target Country`, event.year) %>%
  summarise(govt.govt.events = n()) %>%
  ungroup() %>%
  mutate(cowcode.target = countrycode(`Target Country`, "country.name", "cown")) %>%
  filter(!is.na(`Target Country`)) %>%  # Get rid of missing countries
  mutate(cowcode.target = case_when(
    .$`Target Country` == "Hong Kong" ~ as.integer(715),
    .$`Target Country` == "Serbia" ~ as.integer(340),
    .$`Target Country` == "Occupied Palestinian Territory" ~ as.integer(669),
    TRUE ~ .$cowcode.target
  )) %>%
  filter(!is.na(cowcode.target))  # Get rid of tiny and non-statey states

baseline.ingo.govt <- all.events.interstate %>%
  filter(ingo.source, !party.source, govt.target) %>%
  group_by(`Target Country`, event.year) %>%
  summarise(ingo.govt.events = n()) %>%
  ungroup() %>%
  mutate(cowcode.target = countrycode(`Target Country`, "country.name", "cown")) %>%
  filter(!is.na(`Target Country`)) %>%  # Get rid of missing countries
  mutate(cowcode.target = case_when(
    .$`Target Country` == "Hong Kong" ~ as.integer(715),
    .$`Target Country` == "Serbia" ~ as.integer(340),
    .$`Target Country` == "Occupied Palestinian Territory" ~ as.integer(669),
    TRUE ~ .$cowcode.target
  )) %>%
  filter(!is.na(cowcode.target))  # Get rid of tiny and non-statey states

baseline.panel <- baseline.all.events.target %>%
  select(-`Target Country`) %>%
  complete(cowcode.target, event.year, fill=list(all.events.target = 0)) %>%
  left_join(select(baseline.all.events.source, -`Source Country`),
            by=c("cowcode.target" = "cowcode.source", "event.year")) %>%
  left_join(select(baseline.govt.govt, -`Target Country`),
            by=c("cowcode.target", "event.year")) %>%
  left_join(select(baseline.ingo.govt, -`Target Country`),
            by=c("cowcode.target", "event.year")) %>%
  mutate(govt.govt.events = ifelse(is.na(govt.govt.events),
                                   0, govt.govt.events),
         ingo.govt.events = ifelse(is.na(ingo.govt.events),
                                   0, ingo.govt.events))


# Global protests
#
# CAMEO protest types:
#   140: General political dissent
#   141x: Demonstrate or rally
#   142x: Hunger strike
#   143x: Strike or boycott
#   144x: Obstruct passage, block
#   145x: Protest violently or riot
protests.all <- all.events.raw %>%
  filter(str_detect(`CAMEO Code`, "14\\d+")) %>%
  mutate(protest.type = ifelse(str_detect(`CAMEO Code`, "145\\d*"), 
                               "protests.violent", "protests.nonviolent"))

protests.by.country <- protests.all %>%
  group_by(`Source Country`, protest.type, event.year) %>%
  summarise(num.protests = n()) %>%
  ungroup() %>%
  spread(protest.type, num.protests) %>%
  mutate(cowcode = countrycode(`Source Country`, "country.name", "cown")) %>%
  filter(!is.na(`Source Country`)) %>%  # Get rid of missing countries
  mutate(cowcode = case_when(
    .$`Source Country` == "Hong Kong" ~ as.integer(715),
    .$`Source Country` == "Serbia" ~ as.integer(340),
    .$`Source Country` == "Occupied Palestinian Territory" ~ as.integer(669),
    TRUE ~ .$cowcode
  )) %>%
  filter(!is.na(cowcode))  # Get rid of tiny and non-statey states

protests.by.country.panel <- protests.by.country %>%
  complete(cowcode, event.year, fill=list(protests.violent = 0,
                                          protests.nonviolent = 0)) %>%
  select(-`Source Country`)

expect_equal(nrow(protests.by.country.panel),
             nrow(tidyr::expand(protests.by.country, cowcode, event.year)))


# SOMEDAY: Global violence
#   19*: Fight
#   20*: Unconventional mass violence
# This would be cool to do someday, but it's pretty involved since it's just
# raw violence, not modelled or mapped to any larger events like revolutions or
# insurgencies, which is what the EOI data does (with fancy underlying Bayesian
# models to define those events)

# Global events of interest
eois <- read_csv(file.path(PROJHOME, "Data", "data_raw", "External", "ICEWS", 
                           "study_28119", "gtds_2001.to.may.2014.csv")) %>%
  mutate(year.month = ymd(paste0(year, month, "-01"))) %>%
  mutate_each(funs(as.logical), c(dpc, erv, ic, ins, reb)) %>%
  rowwise() %>%
  mutate(any.crisis = any(c(dpc, erv, ic, ins, reb))) %>%  # Convert to boolean
  ungroup() %>%
  select(ccode, country, year, year.month, any.crisis,
         domestic.political.crisis = dpc, ethnic.religious.violence = erv,
         international.conflict = ic, insurgency = ins, rebellion = reb)

eois.yearly <- eois %>%
  group_by(ccode, year) %>%
  # Calucalte the binary presence of an EOI, the number of months of an EOI in
  # a year, and the percent of the year with an EOI
  summarise_each(funs(bin = any, total = sum, pct = sum(.)/12),
                 -c(country, year.month))


# Shaming
#   11*: Disapprove (111 = criticize or denounce)
#
# Interstate shaming
shaming.states <- all.events.interstate %>%
  filter(str_detect(`CAMEO Code`, "11\\d+")) %>%
  filter(govt.source, govt.target)

shaming.states.by.country <- shaming.states %>%
  group_by(`Target Country`, event.year) %>%
  summarise(shaming.events.states = n()) %>%
  ungroup() %>%
  mutate(cowcode = countrycode(`Target Country`, "country.name", "cown")) %>%
  filter(!is.na(`Target Country`)) %>%  # Get rid of missing countries
  mutate(cowcode = case_when(
    .$`Target Country` == "Hong Kong" ~ as.integer(715),
    .$`Target Country` == "Serbia" ~ as.integer(340),
    .$`Target Country` == "Occupied Palestinian Territory" ~ as.integer(669),
    TRUE ~ .$cowcode
  )) %>%
  filter(!is.na(cowcode))  # Get rid of tiny and non-statey states


# INGO shaming
shaming.ingos <- all.events.interstate %>%
  filter(str_detect(`CAMEO Code`, "11\\d+")) %>%
  filter(ingo.source, !party.source, govt.target)

shaming.ingos.by.country <- shaming.ingos %>%
  group_by(`Target Country`, event.year) %>%
  summarise(shaming.events.ingos = n()) %>%
  ungroup() %>%
  mutate(cowcode = countrycode(`Target Country`, "country.name", "cown")) %>%
  filter(!is.na(`Target Country`)) %>%  # Get rid of missing countries
  mutate(cowcode = case_when(
    .$`Target Country` == "Hong Kong" ~ as.integer(715),
    .$`Target Country` == "Serbia" ~ as.integer(340),
    .$`Target Country` == "Occupied Palestinian Territory" ~ as.integer(669),
    TRUE ~ .$cowcode
  )) %>%
  filter(!is.na(cowcode))  # Get rid of tiny and non-statey states


shaming.by.country.panel <- shaming.states.by.country %>%
  complete(cowcode, event.year, fill=list(shaming.events.states = 0)) %>%
  select(-`Target Country`) %>%
  left_join(select(shaming.ingos.by.country, -`Target Country`),
            by=c("cowcode", "event.year")) %>%
  mutate(shaming.events.ingos = ifelse(is.na(shaming.events.ingos), 
                                       0, shaming.events.ingos))

expect_equal(nrow(shaming.by.country.panel),
             nrow(tidyr::expand(shaming.states.by.country, 
                                cowcode, event.year=1995:2015)))


# Full panel of everything with NAs zeroed out
full.icews.panel <- baseline.panel %>%
  rename(cowcode = cowcode.target) %>%
  left_join(protests.by.country.panel, by=c("cowcode", "event.year")) %>%
  left_join(shaming.by.country.panel, by=c("cowcode", "event.year")) %>%
  # Convert NAs to 0
  mutate_each(funs(ifelse(is.na(.), 0, .)), -c(cowcode, event.year)) %>%
  # Create percent variables
  # SOMEDAY: make better variables with intensity measure from Caerus:
  #          https://github.com/caerusassociates/EventIntensity
  # SOMEDAY: Ignore country/years with fewer than X reported events? Is that 
  #          okay, or will it bias against underreported places? But how much 
  #          do protests in the Marshall Islands or Palau affect anyone, 
  #          especially when they (and anything else) never happen/are reported?
  mutate(ignore.because.small = all.events.target <= 10 | 
           all.events.source <= 10) %>%
  mutate(all.events.target.log = log1p(all.events.target),
         all.events.source.log = log1p(all.events.source),
         protests.all = protests.violent + protests.nonviolent,
         protests.all.log = log1p(protests.all),
         protests.violent.log = log1p(protests.violent),
         protests.nonviolent.log = log1p(protests.nonviolent),
         protests.all.pct.all = protests.all / all.events.source,
         protests.violent.pct.all = protests.violent / all.events.source,
         protests.violent.pct.protests = protests.violent / protests.all,
         protests.nonviolent.pct.all = protests.nonviolent / all.events.source,
         shaming.events.states.log = log1p(shaming.events.states),
         shaming.events.ingos.log = log1p(shaming.events.ingos),
         shaming.states.pct.govt = shaming.events.states / govt.govt.events,
         shaming.ingos.pct.ingo = shaming.events.ingos / ingo.govt.events,
         shaming.states.pct.all = shaming.events.states / all.events.target,
         shaming.ingos.pct.all = shaming.events.ingos / all.events.target) %>%
  # Convert NaNs to 0
  mutate_each(funs(ifelse(is.nan(.), 0, .)), -c(cowcode, event.year))

# Save full panel data
write_feather(full.icews.panel,
              file.path(PROJHOME, "Data", "data_processed",
                        "icews_panel.feather"))

# And EOI data
write_feather(eois.yearly,
              file.path(PROJHOME, "Data", "data_processed",
                        "icews_eois.feather"))

# ---------
# Sandbox
# ---------
# SOMEDAY: Expulsion of aid agencies and NGOs (1663)
# 1663 isn't ever used, but NGO (and media) expulsions are in 166
# expulsions <- all.events %>%
#   filter(str_detect(`CAMEO Code`, "166"))

# Just for kicks
# most.violent.protests <- full.icews.panel %>%
#   arrange(desc(protests.violent)) %>%
#   slice(1:10) %>%
#   mutate(country.name = countrycode(cowcode, "cown", "country.name"),
#          iso2 = tolower(countrycode(cowcode, "cown", "iso2c")),
#          xlab = paste0(country.name, " (", event.year, ")"),
#          xlab = factor(xlab, levels=rev(xlab), ordered=TRUE))
# 
# ggplot(most.violent.protests, aes(y=xlab, x=protests.violent)) +
#   ggstance::geom_barh(stat="identity") + 
#   labs(y=NULL, x="Number of violent protest reports", 
#        title="Country-years with most violent protests",
#        caption="Source: ICEWS Event Data") +
#   ggflags::geom_flag(aes(y=xlab, x=1, country=iso2), size=7)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#
# Older stuff with CAMEO conflictual/cooperative events and Goldstein scores
# Load CAMEO QuadClass categories
# Original, old mapping:
#   http://eventdata.parusanalytics.com/papers.dir/Gerner.APSA.02.pdf
# New mapping: 
#   http://phoenixdata.org/description
# cameo.categories <- read_csv(file.path(PROJHOME, "Data", "data_raw", 
#                                        "External", "ICEWS", 
#                                        "cameo_categories.csv")) %>%
#   select(cameo.root = `Root CAMEO`, cameo.description = Description,
#          cameo.category = `Quad Class Description`) %>%
#   mutate(cameo.category.agg = ifelse(str_detect(cameo.category, "Conflict"), 
#                                      "Conflict",
#                                      ifelse(str_detect(cameo.category, 
#                                                        "Cooperation"), 
#                                             "Cooperation", cameo.category)))
# 
# # Filter the raw data and merge in CAMEO categories
# events.filtered <- all.events %>%
#   # Only interstate events
#   filter(`Source Country` != `Target Country`) %>%
#   # Only state actors
#   filter(str_detect(`Source Sectors`, "Government|Military") |
#            str_detect(`Target Sectors`, "Government|Military")) %>%
#   # Categorize CAMEO roots
#   mutate(cameo.root = as.integer(str_sub(`CAMEO Code`, 1, 2))) %>%
#   left_join(cameo.categories, by="cameo.root") %>%
#   # Other miscellaneous variables
#   mutate(event.year = year(`Event Date`))
# 
# events.aggregated <- events.filtered %>%
#   filter(cameo.category != "Neutral") %>%
#   group_by(`Target Country`, event.year, cameo.category.agg) %>%
#   # group_by(`Target Country`, event.year) %>%
#   summarise(num = n(), severity = mean(Intensity)) %>%
#   ungroup()
# 
# events.conflict.coop <- events.aggregated %>%
#   gather(variable, value, 
#          -c(`Target Country`, event.year, cameo.category.agg)) %>%
#   unite(temp, cameo.category.agg, variable) %>%
#   spread(temp, value, fill=0) %>%
#   mutate(icews.num.events = Cooperation_num + Conflict_num,
#          icews.net.num = Cooperation_num - Conflict_num,
#          icews.pct.shame = Conflict_num / (icews.num.events),
#          icews.net.severity = Cooperation_severity - Conflict_severity,
#          icews.conflict.severity.abs = abs(Conflict_severity),
#          cowcode = countrycode(`Target Country`, "country.name", "cown"),
#          cowcode = ifelse(`Target Country` == "Serbia", 340, cowcode),
#          cowcode = ifelse(`Target Country` == "Hong Kong", 715, cowcode)) %>%
#   filter(icews.num.events > 50) %>% 
#   select(-`Target Country`)
# 
# # INGO conflictual/cooperative events
# events.filtered.ingos <- all.events %>%
#   # Only interstate events
#   filter(`Source Country` != `Target Country`) %>%
#   # Only INGOs targeting state actors
#   filter(str_detect(`Source Sectors`, "Nongovernmental Organization \\(International\\)"),
#          str_detect(`Target Sectors`, "Government|Military")) %>%
#   # Categorize CAMEO roots
#   mutate(cameo.root = as.integer(str_sub(`CAMEO Code`, 1, 2))) %>%
#   left_join(cameo.categories, by="cameo.root") %>%
#   # Other miscellaneous variables
#   mutate(event.year = year(`Event Date`))
# 
# events.aggregated.ingos <- events.filtered.ingos %>%
#   filter(cameo.category != "Neutral") %>%
#   group_by(`Target Country`, event.year, cameo.category.agg) %>%
#   summarise(num = n(), severity = mean(Intensity)) %>%
#   ungroup()
# 
# events.conflict.coop.ingos <- events.aggregated.ingos %>%
#   gather(variable, value, 
#          -c(`Target Country`, event.year, cameo.category.agg)) %>%
#   unite(temp, cameo.category.agg, variable) %>%
#   spread(temp, value, fill=0) %>%
#   mutate(icews.num.events.ingos = Cooperation_num + Conflict_num,
#          icews.net.num.ingos = Cooperation_num - Conflict_num,
#          icews.pct.shame.ingos = Conflict_num / (icews.num.events.ingos),
#          icews.net.severity.ingos = Cooperation_severity - Conflict_severity,
#          icews.conflict.severity.abs.ingos = abs(Conflict_severity),
#          cowcode = countrycode(`Target Country`, "country.name", "cown"),
#          cowcode = ifelse(`Target Country` == "Serbia", 340, cowcode),
#          cowcode = ifelse(`Target Country` == "Hong Kong", 715, cowcode)) %>%
#   rename(Cooperation_num.ingos = Cooperation_num, 
#          Cooperation_severity.ingos = Cooperation_severity,
#          Conflict_num.ingos = Conflict_num,
#          Conflict_severity.ingos = Conflict_severity) %>%
#   select(-`Target Country`)
# 
# 
# # Save aggregated data
# write_feather(events.conflict.coop,
#               file.path(PROJHOME, "Data", "data_processed",
#                         "icews_aggregated.feather"))
# 
# write_feather(events.conflict.coop.ingos, 
#               file.path(PROJHOME, "Data", "data_processed",
#                         "icews_aggregated_ingos.feather"))
