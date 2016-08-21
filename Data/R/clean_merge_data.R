# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(feather)
library(lubridate)
library(countrycode)
library(WDI)
library(testthat)

# Map libraries
# You must install geos (http://trac.osgeo.org/geos/) and 
# gdal (http://www.gdal.org/) first. 
# Easy to do on OS X: 
#   brew install geos gdal
# Then install these packages from source
#   install.packages(c("rgeos", "rgdal"), type="source")
library(rgdal)
library(rgeos)
library(spdep)
library(maptools)


# ------------------
# Useful functions
# ------------------
# What it says on the proverbial tin: convert -999 to NA
fix.999 <- function(x) {
  ifelse(x == -999, NA, x)
}

# Same. Convert "NA" to NA
fix.NA <- function(x) {
  ifelse(x == "NA", NA, x)
}

# Calculate the number of years since an election based on a boolean vector 
# indicating if the running total should be increased
calc.years.since.comp <- function(x) {
  rle.x <- rle(x)$lengths
  as.numeric(unlist(sapply(split(x, rep(seq(along = rle.x), rle.x)), cumsum)))
}

# Holy crap this is complicated.
# If a competitive election has happened already, then figure out if the 
# running total of years since election needs to be increased. This marks years 
# *before* any competitive election as missing. For example, if country A 
# doesn't hold an election until 1960, the years from 1945-59 will be NA.
get.increase <- function(x) {
  has.been.competitive <- FALSE
  increase <- logical(length(x))
  for(i in 1:length(x)) {
    if(!is.na(x[i]) & x[i] == TRUE) {
      has.been.competitive <- TRUE
    } else {
      increase[i] <- NA
    }
    
    if(has.been.competitive) {
      if(x[i] == FALSE | is.na(x[i])) {
        increase[i] <- TRUE
      } else {
        increase[i] <- FALSE
      }
    }
  }
  return(increase)
}

# Combine every combination of rows from two dataframes
# Function from http://stackoverflow.com/a/21911221/120898
expand.grid.df <- function(...) {
  Reduce(function(...) merge(..., by=NULL), list(...))
}


# The ICRG composite political risk score uses a 0-100 scale with ordinal
# cutpoints. Removing variables from the score (like external conflict) changes
# the maximum of the scale and makes the cutpoints incorrect. This function
# rescales a given variable to a 0-100 scale using the formula:
#   (x - min(x)) / (max(x) - min(x)) * 100
# min(x) and max(x) are hardcoded in as 0 and 82 (removing 12 points for
# external conflict and 6 for democratic accountability)
icrg.rescale <- function(x, lowest=0, highest=82) {
  return((x - lowest) / (highest - lowest) * 100)
}


# ----------------------------
# Load all sorts of datasets
# ----------------------------
# Gleditsch Ward codes
gw.states <- read_tsv("http://privatewww.essex.ac.uk/~ksg/data/iisystem.dat",
                      col_names = c("gwcode", "cowc", "country",
                                    "date.start", "date.end"),
                      locale = locale(encoding="windows-1252"))
gw.microstates <- read_tsv("http://privatewww.essex.ac.uk/~ksg/data/microstatessystem.dat",
                           col_names = c("gwcode", "cowc", "country",
                                         "date.start", "date.end"),
                           locale = locale(encoding="windows-1252"))

gw.codes <- bind_rows(gw.states, gw.microstates) %>%
  mutate(date.start = dmy(date.start), date.end = dmy(date.end)) %>%
  mutate(iso2 = countrycode(gwcode, "cown", "iso2c"),
         iso3 = countrycode(gwcode, "cown", "iso3c")) %>%
  mutate(iso2 = case_when(
           .$gwcode == 340 ~ "RS",
           .$gwcode == 347 ~ "XK",
           .$gwcode == 678 ~ "YE",
           TRUE ~ .$iso2),
         iso3 = case_when(
           .$gwcode == 340 ~ "SRB",
           .$gwcode == 347 ~ "XKX",
           .$gwcode == 678 ~ "YEM",
           TRUE ~ .$iso3)) %>%
  mutate(iso2 = ifelse(is.na(iso2), 
                       countrycode(country, "country.name", "iso2c"), iso2),
         iso3 = ifelse(is.na(iso3), 
                       countrycode(country, "country.name", "iso3c"), iso3)) %>%
  # Get rid of old, tiny, and non-statey states
  filter(!is.na(iso2))

gw.lookup <- gw.codes %>%
  select(gwcode, country) %>%
  group_by(gwcode) %>%
  slice(n()) %>%  # Get the most recent country definition
  ungroup()

# Gleditsch and Ward do not include a bunch of countries, so I create my own
# codes for relevant countries that appear in other datasets:
#
#   - Somaliland: 521
#   - Palestine (West Bank): 667
#   - Palestine (Gaza): 668
#   - Palestine (both): 669
#   - Hong Kong: 715
#
# Also, following Gleditsch and Ward, I treat Serbia as 340 and Serbia &
# Montenegro as a continuation of Yugoslavia, or 345.
# http://privatewww.essex.ac.uk/~ksg/data/iisyst_casedesc.pdf


# Varieties of Democracy
# https://v-dem.net/en/
# readr::read_csv chokes on the raw V-Dem file, so read.csv it is :(
# But only temporarily/once. Read the file in with read.csv then save as RDS
vdem.original.location <- file.path(PROJHOME, "Data", "data_raw", 
                                    "External", "V-Dem", "v6.1",
                                    "V-Dem-DS-CY+Others-v6.1.csv")
vdem.rds <- file.path(PROJHOME, "Data", "data_processed",
                      "V-Dem-DS-CY+Others-v6.1.rds")

if (!file.exists(vdem.rds)) {
  vdem.raw.slow <- read.csv(vdem.original.location,
                            stringsAsFactors=FALSE)
  
  saveRDS(vdem.raw.slow, vdem.rds)
  rm(vdem.raw.slow)
}

vdem.raw <- readRDS(vdem.rds) %>%
  # Missing COW codes
  mutate(COWcode = case_when(
    .$country_text_id == "SML" ~ as.integer(521),
    .$country_text_id == "PSE" ~ as.integer(667),
    .$country_text_id == "PSG" ~ as.integer(668),
    TRUE ~ .$COWcode))

testthat::expect_equal(nrow(vdem.raw), 16675)
testthat::expect_equal(ncol(vdem.raw), 3224)

# COW issues
# vdem.raw %>% filter(is.na(COWcode)) %>% select(country_name) %>% unique

# Extract civil society-related variables
vdem.cso <- vdem.raw %>% select(country_name, year, cowcode = COWcode, 
                                e_polity2, v2x_frassoc_thick, v2xcs_ccsi,
                                starts_with("v2cseeorgs"), 
                                starts_with("v2csreprss"), 
                                starts_with("v2cscnsult"),
                                starts_with("v2csprtcpt"), 
                                starts_with("v2csgender"), 
                                starts_with("v2csantimv")) %>%
  mutate(v2csreprss_ord = factor(v2csreprss_ord, 
                                 labels=c("Severely", "Substantially", 
                                          "Moderately", "Weakly", "No"),
                                 ordered=TRUE),
         cs_env_sum = v2csreprss + v2cseeorgs,
         polity_ord = cut(e_polity2, breaks=c(-Inf, -6, 5, Inf),
                          labels=c("Autocracy", "Anocracy", "Democracy"),
                          ordered_result=TRUE),
         polity_ord2 = cut(e_polity2, breaks=c(-Inf, 0, Inf),
                           labels=c("Autocracy", "Democracy"),
                           ordered_result=TRUE)) %>%
  group_by(cowcode) %>%
  mutate(v2csreprss_ord.lead = lead(v2csreprss_ord),
         v2csreprss.lead = lead(v2csreprss),
         v2cseeorgs.lead = lead(v2cseeorgs),
         cs_env_sum.lead = lead(cs_env_sum)) %>%
  ungroup() %>%
  filter(year > 1990)  # ICRG starts at 1991, so only include data after then


# International Country Risk Guide (ICRG)
# https://epub.prsgroup.com/list-of-all-variable-definitions
# http://www.prsgroup.com/wp-content/uploads/2012/11/icrgmethodology.pdf
# http://library.duke.edu/data/collections/icrg
icrg.cols <- list(icrg.stability="Government Stability", 
                  icrg.socioeconomic="Socioeconomic Conditions", 
                  icrg.investment="Investment Profile", 
                  icrg.internal="Internal Conflict", 
                  icrg.external="External Conflict", 
                  icrg.corruption="Corruption", 
                  icrg.military="Military in Politics", 
                  icrg.religion="Religion in Politics", 
                  icrg.law="Law and Order", 
                  icrg.ethnic="Ethnic Tensions", 
                  icrg.accountability="Democratic Accountability", 
                  icrg.bureau="Bureaucracy Quality")

# Loop through each sheet in the Excel file, convert the sheet to long, and
# append to all.dfs, which will later be combined into one super long dataframe
# and spread out to 12 individual variables
all.dfs <- list()
for (i in 1:length(icrg.cols)) {
  new.name <- as.character(names(icrg.cols[i]))

  df <- read_excel(file.path(PROJHOME, "Data", "data_raw", "External", "ICRG",
                             "3BResearchersDataset2015.xls"), 
                   sheet=as.character(icrg.cols[i]), skip=7) %>%
    gather(year.num, score, -Country) %>%
    mutate(var.name = new.name)

  all.dfs[[new.name]] <- df
}

icrg.all <- bind_rows(all.dfs) %>%
  mutate(score = as.numeric(score),
         year.num = as.numeric(year.num)) %>%
  spread(var.name, score) %>%
  mutate(year.actual = ymd(paste0(year.num, "-01-01")),
         # Help out countrycode's regex
         Country = ifelse(Country == "Korea, DPR", "North Korea", Country),
         # Country variables
         cowcode = countrycode(Country, "country.name", "cown"),
         country.name = countrycode(Country, "country.name", "country.name"),
         iso = countrycode(country.name, "country.name", "iso3c")) %>%
  # Add unofficial COW codes for these countries
  mutate(cowcode = case_when(
    .$Country == "Hong Kong" ~ as.integer(715),
    .$Country == "New Caledonia" ~ as.integer(1012),
    .$Country == "Serbia *" ~ as.integer(340),
    .$Country == "Serbia & Montenegro *" ~ as.integer(345),
    TRUE ~ .$cowcode)) %>%
  mutate(country.name = ifelse(Country == "Serbia & Montenegro *",
                               "Yugoslavia", country.name),
         iso = ifelse(Country == "Serbia & Montenegro *", "YUG", iso)) %>%
  # Deal with duplicate COWs (Russia, Germany, and Serbia) where names change
  # by collapsing all rows and keeping the max value
  group_by(year.num, cowcode) %>%
  mutate_each(funs(max(., na.rm=TRUE)), starts_with("icrg")) %>%
  filter(!(Country %in% c("Serbia & Montenegro *", "USSR", "West Germany"))) %>%
  ungroup() %>%
  mutate(subregion = countrycode(iso, "iso3c", "region"),
         region = countrycode(iso, "iso3c", "continent")) %>%
  mutate(subregion = case_when(
           .$iso == "TWN" ~ "Eastern Asia",
           .$iso == "CSK" ~ "Eastern Europe",
           .$iso == "DDR" ~ "Western Europe",
           TRUE ~ .$subregion
         ),
         region = case_when(
           .$iso == "TWN" ~ "Asia",
           .$iso == "CSK" ~ "Europe",
           .$iso == "DDR" ~ "Europe",
           TRUE ~ .$region
         )) %>%
  # group_by(subregion, year.num) %>%
  # mutate(icrg.stability.region = ifelse(!is.na(icrg.stability), 
  #                                       mean(icrg.stability, na.rm=TRUE), 
  #                                       as.numeric(NA)),
  #        num.states.in.region = n()) %>%
  # ungroup()
  mutate(icrg.pol.risk = icrg.stability + icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.external + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + 
           icrg.accountability + icrg.bureau,
         icrg.pol.risk.internal = icrg.stability + icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + icrg.bureau,
         icrg.pol.risk.internal.nostab = icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + icrg.bureau,
         icrg.pol.risk.internal.scaled = icrg.rescale(icrg.pol.risk.internal),
         icrg.pol.risk.internal.nostab.scaled = icrg.rescale(icrg.pol.risk.internal.nostab),
         icrg.pol.grade = cut(icrg.pol.risk, 
                              c(0, 49.99, 59.99, 69.99, 79.99, Inf),
                              labels=c("Very High", "High", "Moderate", 
                                       "Low", "Very Low"),
                              ordered_result=TRUE, include.lowest=TRUE),
         icrg.pol.grade.internal = cut(icrg.pol.risk.internal.scaled, 
                                       c(0, 49.99, 59.99, 69.99, 79.99, Inf),
                                       labels=c("Very High", "High", "Moderate", 
                                                "Low", "Very Low"),
                                       ordered_result=TRUE, include.lowest=TRUE))


icrg.global <- icrg.all %>%
  group_by(year.num) %>%
  summarise(icrg.pol.risk.global = mean(icrg.pol.risk, na.rm=TRUE))

icrg.regional <- icrg.all %>%
  group_by(year.num, region) %>%
  summarise(icrg.pol.risk.regional = mean(icrg.pol.risk, na.rm=TRUE))

icrg.subregional <- icrg.all %>%
  group_by(year.num, subregion) %>%
  summarise(icrg.pol.risk.subregional = mean(icrg.pol.risk, na.rm=TRUE))

# Get the group mean excluding the value of the current row by recalculating
# the mean: sum all the values in the group, subtract the current value, and
# divide by the group size minus 1
# Via http://stackoverflow.com/q/35858876/120898
# (loo = leave one out)
icrg.loo <- icrg.all %>%
  group_by(year.num, region) %>%
  mutate(icrg.pol.risk.regional.loo = (sum(icrg.pol.risk, na.rm=TRUE) -
                                         icrg.pol.risk) / (n() - 1)) %>%
  group_by(year.num, subregion) %>%
  mutate(icrg.pol.risk.subregional.loo = (sum(icrg.pol.risk, na.rm=TRUE) -
                                            icrg.pol.risk) / (n() - 1)) %>%
  ungroup() %>%
  select(year.num, cowcode, contains("loo"))

icrg.all.with.aggregates <- icrg.all %>%
  left_join(icrg.loo, by=c("year.num", "cowcode")) %>%
  left_join(icrg.global, by="year.num") %>%
  left_join(icrg.regional, by=c("year.num", "region")) %>%
  left_join(icrg.subregional, by=c("year.num", "subregion"))


# ICRG monthly data
# Capture output because of all the DEFINEDNAME output that read_excel makes
# See https://github.com/hadley/readxl/issues/82
icrg.cols.monthly <- list(icrg.stability="A-GovStab",
                          icrg.socioeconomic="B-Socioeco",
                          icrg.investment="C-InvProf",
                          icrg.internal="D-IntConf",
                          icrg.external="E-ExtConf",
                          icrg.corruption="F-Corrupt",
                          icrg.military="G-Military",
                          icrg.religion="H-Religious",
                          icrg.law="I-Law",
                          icrg.ethnic="J-Ethnic",
                          icrg.accountability="K-DemoAcct",
                          icrg.bureau="L-Bureau")

# Loop through each sheet in the Excel file, convert the sheet to long, and
# append to all.dfs, which will later be combined into one super long dataframe
# and spread out to 12 individual variables
all.dfs.monthly <- list()
capture.output({
  for (i in 1:length(icrg.cols.monthly)) {
    new.name <- as.character(names(icrg.cols.monthly[i]))
    sheet <- as.character(icrg.cols.monthly[i])
    
    df.file <- file.path(PROJHOME, "Data", "data_raw", "External", 
                                   "ICRG", "ICRG_T3B.xls")
  
    df.dims <- read_excel(df.file, sheet=sheet, skip=5)
    df <- read_excel(df.file, sheet=sheet, skip=5, 
                     col_types=c("date", rep("numeric", ncol(df.dims) - 1))) %>%
      slice(3:n()) %>%
      select(Date = 1, everything(), -Country) %>%
      gather(Country, score, -Date) %>%
      mutate(var.name = new.name)
    
    all.dfs.monthly[[new.name]] <- df
  }
}, file="/dev/null")

icrg.monthly <- bind_rows(all.dfs.monthly) %>%
  mutate(score = as.numeric(score),
         year.num = year(Date),
         year.actual = ymd(paste0(year.num, "-01-01"))) %>%
  spread(var.name, score) %>%
  mutate(# Help out countrycode's regex
         Country = ifelse(Country == "Korea, DPR", "North Korea", Country),
         # Country variables
         cowcode = countrycode(Country, "country.name", "cown"),
         country.name = countrycode(Country, "country.name", "country.name"),
         iso = countrycode(country.name, "country.name", "iso3c")) %>%
  # Add unofficial COW codes for these countries
  mutate(cowcode = case_when(
    .$Country == "Hong Kong" ~ as.integer(715),
    .$Country == "New Caledonia" ~ as.integer(1012),
    .$Country == "Serbia" ~ as.integer(340),
    .$Country == "Serbia & Montenegro" ~ as.integer(345),
    TRUE ~ .$cowcode)) %>%
  mutate(country.name = ifelse(Country == "Serbia & Montenegro",
                               "Yugoslavia", country.name),
         iso = ifelse(Country == "Serbia & Montenegro", "YUG", iso)) %>%
  mutate(icrg.pol.risk = icrg.stability + icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.external + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + 
           icrg.accountability + icrg.bureau,
         icrg.pol.risk.internal = icrg.stability + icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + icrg.bureau,
         icrg.pol.risk.internal.nostab = icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + icrg.bureau,
         icrg.pol.risk.internal.scaled = icrg.rescale(icrg.pol.risk.internal),
         icrg.pol.risk.internal.nostab.scaled = icrg.rescale(icrg.pol.risk.internal.nostab),
         icrg.pol.grade = cut(icrg.pol.risk, 
                              c(0, 49.99, 59.99, 69.99, 79.99, Inf),
                              labels=c("Very High", "High", "Moderate", 
                                       "Low", "Very Low"),
                              ordered_result=TRUE, include.lowest=TRUE),
         icrg.pol.grade.internal = cut(icrg.pol.risk.internal.scaled, 
                                       c(0, 49.99, 59.99, 69.99, 79.99, Inf),
                                       labels=c("Very High", "High", "Moderate", 
                                                "Low", "Very Low"),
                                       ordered_result=TRUE, include.lowest=TRUE))


# Database of Political Institutions 2012
# http://go.worldbank.org/2EAGGLRZ40
pol.inst <- read_dta(file.path(PROJHOME, "Data", "data_raw", "External", 
                               "DPI", "DPI2012.dta")) %>%
  filter(!countryname %in% c("Turk Cyprus", "PRK")) %>%
  mutate(yrsoffc = fix.999(yrsoffc), oppfrac = fix.999(oppfrac),
         opp1seat = fix.999(opp1seat), totalseats = fix.999(totalseats),
         opp1vote = fix.999(opp1vote),
         finittrm = factor(ifelse(finittrm == -999, NA, finittrm), 
                           labels=c("No", "Yes"))) %>%
  mutate(countryname = case_when(
    .$countryname == "UAE" ~ "United Arab Emirates",
    .$countryname == "GDR" ~ "German Democratic Republic",
    .$countryname == "S. Africa" ~ "South Africa",
    .$countryname == "Dom. Rep." ~ "Dominican Republic",
    TRUE ~ .$countryname)) %>%
  mutate(cow = countrycode(countryname, "country.name", "cown"),
         year = as.numeric(year)) %>%
  select(year, cow, yrsoffc, finittrm,
         opp1vote, oppfrac, opp1seat, totalseats)


# National Elections Across Democracy and Autocracy (NELDA)
# http://hyde.research.yale.edu/nelda/
# 
# TODO: type of most recent election? + time since previous election?
# True if all elections that year were competitive
nelda <- read_tsv(file.path(PROJHOME, "Data", "data_raw", "External",
                            "NELDA", "NELDA.csv")) %>%
  mutate(competitive = ifelse(nelda3 == "yes" & nelda4 == "yes" & 
                                nelda5 == "yes", TRUE, FALSE)) %>%
  group_by(ccode, year) %>%
  summarise(all.comp = all(competitive), num.elections = n())

# Years since last competitive election
nelda.full <- expand.grid(ccode = unique(nelda$ccode),
                          year = min(nelda$year):max(nelda$year)) %>%
  arrange(ccode, year) %>%
  left_join(nelda, by=c("ccode", "year")) %>%
  group_by(ccode) %>%
  mutate(years.since.comp = calc.years.since.comp(get.increase(all.comp)),
         num.elections = ifelse(is.na(num.elections), 0, num.elections))


# CIRI Human Rights Data
# http://www.humanrightsdata.com/
ciri <- read.csv(file.path(PROJHOME, "Data", "data_raw", "External", "CIRI",
                           "CIRI Data 1981_2011 2014.04.14.csv")) %>%
  mutate(COW = case_when(
    .$CTRY == "Kosovo" ~ as.integer(347),
    .$CTRY == "Montenegro" ~ as.integer(341),
    .$CTRY == "Serbia" ~ as.integer(340),
    TRUE ~ .$COW)) %>%
  select(year = YEAR, cowcode = COW, assn = ASSN, physint = PHYSINT) %>%
  # Handle duplicate COWs like USSR and Yugoslavia where names change
  group_by(year, cowcode) %>%
  summarize(assn = max(assn, na.rm=TRUE),
            physint = max(physint, na.rm=TRUE)) %>%
  mutate(assn = ifelse(assn < 0, NA, assn),
         assn = factor(assn, labels=c("Severely restricted",
                                      "Limited", "Unrestricted"),
                       ordered=TRUE))


# World Bank World Development Indicators (WDI)
# http://data.worldbank.org/data-catalog/world-development-indicators
# Use WDI::WDI() to access the data
wdi.indicators <- c("NY.GDP.PCAP.KD",  # GDP per capita (constant 2005 US$)
                    "NY.GDP.MKTP.KD",  # GDP (constant 2005 US$)
                    "SP.POP.TOTL",     # Population, total
                    "DT.ODA.ALLD.CD")  # Net ODA and official aid received (current US$)
wdi.countries <- unique(gw.codes$iso2)
wdi.raw <- WDI(country="all", wdi.indicators, extra=TRUE, start=1990, end=2016)

wdi.clean <- wdi.raw %>%
  filter(iso2c %in% wdi.countries) %>%
  rename(gdpcap = NY.GDP.PCAP.KD, gdp = NY.GDP.MKTP.KD, 
         population = SP.POP.TOTL, oda = DT.ODA.ALLD.CD) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  # Ignore negative values of oda
  mutate(oda.log = sapply(oda, FUN=function(x) ifelse(x < 0, NA, log1p(x)))) %>%
  mutate(cow = countrycode(iso2c, "iso2c", "cown"),
         region = factor(region),  # Get rid of unused levels first
         region = factor(region, labels = 
                           gsub(" \\(all income levels\\)", "", levels(region)))) %>%
  mutate(cow = case_when(
    .$country == "Serbia" ~ as.integer(340),
    .$country == "Kosovo" ~ as.integer(347),
    TRUE ~ .$cow
  )) %>%
  select(-c(iso2c, iso3c, country, capital, longitude, latitude, income, lending))


# Count of INGO members
# http://dx.doi.org/10.1017/S0007123412000683
# Data from Amanda Murdie, "The Ties That Bind: A Network Analysis of Human
# Rights International Nongovernmental Organizations," *British Journal of
# Political Science* 44, no. 1 (January 2014): 1–27.
murdie <- read_dta(file.path(PROJHOME, "Data", "data_raw", 
                             "External", "Murdie 2014",
                             "11558_2013_9180_MOESM1_ESM.dta")) %>%
  mutate(cowcode = as.numeric(cowcode)) %>%
  mutate(cowcode = case_when(
    .$ctry == "Serbia" ~ as.integer(340),
    .$ctry == "Montenegro" ~ as.integer(341),
    .$ctry == "Kosovo" ~ as.integer(347),
    TRUE ~ as.integer(.$cowcode)
  )) %>%
  filter(cowcode != 1, cowcode <= 990) %>%
  select(year, cowcode, countngo) %>%
  group_by(cowcode, year) %>%
  slice(1) %>% ungroup()


# KOF Index of Globalization
# http://globalization.kof.ethz.ch/
capture.output({
  kof <- read_excel(file.path(PROJHOME, "Data", "data_raw", "External", "KOF",
                              "globalization_2015_long.xls"), 
                    sheet="data long", skip=1, na=".") %>%
    mutate(cowcode = countrycode(country, "country.name", "cown")) %>%
    mutate(cowcode = case_when(
      .$country == "Serbia" ~ as.integer(340),
      .$country == "West Bank and Gaza" ~ as.integer(669),
      TRUE ~ as.integer(.$cowcode)
    )) %>%
    select(cowcode, year, globalization = index) %>%
    filter(!is.na(cowcode))
}, file="/dev/null")


# Major Episodes of Political Violence, 1946-2014
# http://www.systemicpeace.org/inscrdata.html
# mepv <- read_excel(file.path(PROJHOME, "Data", "data_raw", "External",
#                              "MEPV", "MEPV2012n.xls"),
#                    sheet="MEPV2012n")

# Data not very useful. There's no movement at all pretty much anywhere, since
# it requires at least 500 deaths to register an event.
#
# ggplot(filter(mepv, COUNTRY == "Egypt"), aes(x=YEAR, y=ACTOTAL)) +
#   geom_line()
# ggplot(filter(mepv, COUNTRY == "Egypt"), aes(x=YEAR, y=nAC)) +
#   geom_line()


# Coups d'Etat, 1946-2014
# http://www.systemicpeace.org/inscrdata.html
coups <- read_excel(file.path(PROJHOME, "Data", "data_raw", "External",
                              "Coups", "CSPCoupsAnnual2014.xls"),
                    sheet="Sheet1") %>%
  filter(year > 1989) %>%
  mutate(coups.success = scoup1,
         coups.success.bin = coups.success > 0,
         coups.activity = scoup1 + atcoup2 + pcoup3 + apcoup4,
         coups.activity.bin = coups.activity > 0) %>%
  select(ccode, year, starts_with("coups"))

# Global instances of coups
# http://www.jonathanmpowell.com/coup-detat-dataset.html
coup.url <- "http://www.uky.edu/~clthyn2/coup_data/powell_thyne_ccode_year.txt"
coups.new <- read_tsv(coup.url) %>%
  filter(year > 1989) %>%
  mutate(cowcode = ccode) %>%  # ccode is the Gleditsch/Ward code
  mutate(coups.success.bin = coup1 == 2 | coup2 == 2 | coup3 == 2 | coup4 == 2,
         coups.activity.bin = coup1 > 0 | coup2 > 0 | coup3 > 0 | coup4 > 0) %>%
  select(cowcode, year, starts_with("coups")) %>%
  mutate(region = countrycode(cowcode, "cown", "continent"),
         subregion = countrycode(cowcode, "cown", "region")) %>%
  mutate(subregion = case_when(
           .$cowcode == 260 ~ "Western Europe",  # GFR (West)
           .$cowcode == 265 ~ "Eastern Europe",  # GDR (East)
           .$cowcode == 315 ~ "Eastern Europe",  # Czechoslovakia
           .$cowcode == 345 ~ "Eastern Europe",  # Yugoslavia
           .$cowcode == 678 ~ "Western Asia",    # Yemen (North)
           .$cowcode == 680 ~ "Western Asia",    # Yemen (South)
           .$cowcode == 713 ~ "Eastern Asia",    # Taiwan
           TRUE ~ .$subregion), 
         region = case_when(
           .$cowcode %in% c(260, 265, 315, 345) ~ "Europe",
           .$cowcode %in% c(678, 680, 713) ~ "Asia",
           TRUE ~ .$region
         ))

coups.global <- coups.new %>%
  group_by(year) %>%
  summarise(coups.success.global = sum(coups.success.bin, na.rm=TRUE),
            coups.activity.global = sum(coups.activity.bin, na.rm=TRUE))

coups.regional <- coups.new %>%
  group_by(year, region) %>%
  summarise(coups.success.regional = sum(coups.success.bin, na.rm=TRUE),
            coups.activity.regional = sum(coups.activity.bin, na.rm=TRUE))

coups.subregional <- coups.new %>%
  group_by(year, subregion) %>%
  summarise(coups.success.subregional = sum(coups.success.bin, na.rm=TRUE),
            coups.activity.subregional = sum(coups.activity.bin, na.rm=TRUE))

coups.final <- coups.new %>%
  left_join(coups.global, by="year") %>%
  left_join(coups.regional, by=c("year", "region")) %>%
  left_join(coups.subregional, by=c("year", "subregion")) %>%
  mutate(coups.success.regional = 
           coups.success.regional - coups.success.bin,
         coups.activity.regional = 
           coups.activity.regional - coups.activity.bin,
         coups.success.subregional = 
           coups.success.subregional - coups.success.bin,
         coups.activity.subregional = 
           coups.activity.subregional - coups.activity.bin)

# ICEWS protest and shaming data
# Preprocessed with ./Data/R/icews.R
icews <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                                "icews_panel.feather"))

# ICEWS events of interest (EOI) data
# Preprocessed with ./Data/R/icews.R
icews.eois <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                                     "icews_eois.feather"))

# Uppsala conflict data
# http://www.pcr.uu.se/research/ucdp/datasets/generate_your_own_datasets/dynamic_datasets/
# conflicts <- read_tsv(file.path(PROJHOME, "Data", "data_raw", "External", 
#                                 "Uppsala", "ywd_dataset.csv")) %>%
#   mutate(intensity = factor(intensity, 
#                             levels=c("No", "Minor", "Intermediate", "War"), 
#                             ordered=TRUE))


# Determine which countries are neighbors (share land borders)
# https://gist.github.com/andrewheiss/926b9d60a26e29f6bf32

# Variables for getting map shapefiles
map.url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
map.path <- file.path(PROJHOME, "Data", "data_raw", 
                      "External", "Natural Earth maps")
map.zip.name <- basename(map.url)
map.name <- tools::file_path_sans_ext(map.zip.name)

# Download Natural Earth shapefiles if needed
if (!file.exists(file.path(map.path, paste0(map.name, ".shp")))) {
  download.file(url=map.url, file.path(map.path, map.zip.name), "auto")
  unzip(file.path(map.path, map.zip.name), exdir=map.path)
  file.remove(file.path(map.path, map.zip.name))
}

# Load shapefiles
countries <- readOGR(map.path, map.name)

# Extract the ISO codes and map them to the numeric row names
country.names <- data_frame(id = row.names(countries@data),
                            country_iso3 = as.character(countries@data$adm0_a3_is),
                            neighbor_iso3 = country_iso3)

# Determine which countries are neighbors
# Adapted from http://stackoverflow.com/a/32318128/120898
neighbor.list <- poly2nb(countries)
neighbor.matrix <- nb2mat(neighbor.list, style="B", zero.policy=TRUE)
colnames(neighbor.matrix) <- rownames(neighbor.matrix)

# Clean up and transform the neighbor matrix
all.neighbors <- as.data.frame(neighbor.matrix) %>%
  mutate(country = row.names(.)) %>%  # Convert row names to actual column
  gather(neighbor, present, -country) %>%  # Convert to long
  filter(present == 1) %>%  # Only look at cells with a match
  # Add country names
  left_join(select(country.names, -neighbor_iso3), by=c("country" = "id")) %>%
  left_join(select(country.names, -country_iso3), by=c("neighbor" = "id")) %>%
  filter(country_iso3 != "-99", neighbor_iso3 != "-99") %>%  # Remove missing countries
  select(contains("iso3"))  # Just get the ISO columns

neighbor.cows <- all.neighbors %>%
  mutate(country_cow = countrycode(country_iso3, "iso3c", "cown"),
         neighbor_cow = countrycode(neighbor_iso3, "iso3c", "cown")) %>%
  # Add COW codes for Hong Kong, Serbia
  mutate(country_cow = ifelse(country_iso3 == "SRB", 340, country_cow),
         country_cow = ifelse(country_iso3 == "HKG", 715, country_cow),
         neighbor_cow = ifelse(neighbor_iso3 == "SRB", 340, neighbor_cow),
         neighbor_cow = ifelse(neighbor_iso3 == "HKG", 715, neighbor_cow)) %>%
  filter(complete.cases(.)) %>%
  select(contains("cow")) %>%
  unique()

summarize.neighbors <- function(chunk, df) {
  df.chunk <- df %>%
    filter(year.num == unique(chunk$year.num),
           cowcode %in% chunk$neighbor_cow) %>%
    select(icrg.stability, icrg.pol.risk.internal.scaled,
           starts_with("coups"))

  # If there is ICRG data for the given year, summarize it. Otherwise, just 
  # return a bunch of NAs. Without this if-else check, dplyr 0.4.3 chokes when 
  # summarizing an empty dataframe with min() and max() (but is inexplicably 
  # fine with using mean(), median(), and sd(), returning either NaN or NA; min
  # and max cause a 'segfault: memory not mapped' error)
  if (nrow(df.chunk) > 0) {
    df.chunk.summary <- df.chunk %>%
      summarise(neighbor.stability.mean = mean(icrg.stability, na.rm=TRUE),
                neighbor.stability.median = median(icrg.stability, na.rm=TRUE),
                neighbor.stability.sd = sd(icrg.stability, na.rm=TRUE),
                neighbor.stability.min = min(icrg.stability, na.rm=TRUE),
                neighbor.stability.max = max(icrg.stability, na.rm=TRUE),
                neighbor.pol.risk.mean = mean(icrg.pol.risk.internal.scaled, na.rm=TRUE),
                neighbor.pol.risk.median = median(icrg.pol.risk.internal.scaled, na.rm=TRUE),
                neighbor.pol.risk.sd = sd(icrg.pol.risk.internal.scaled, na.rm=TRUE),
                neighbor.pol.risk.min = min(icrg.pol.risk.internal.scaled, na.rm=TRUE),
                neighbor.pol.risk.max = max(icrg.pol.risk.internal.scaled, na.rm=TRUE),
                neighbor.coups = sum(coups.success.bin, na.rm=TRUE),
                neighbor.coups.bin = neighbor.coups > 0,
                neighbor.coup.activity = sum(coups.activity.bin, na.rm=TRUE),
                neighbor.coups.activity.bin = neighbor.coup.activity > 0)
  } else {
    df.chunk.summary <- df.chunk %>%
      summarise(neighbor.stability.mean = NA,
                neighbor.stability.median = NA,
                neighbor.stability.sd = NA,
                neighbor.stability.min = NA,
                neighbor.stability.max = NA,
                neighbor.pol.risk.mean = NA,
                neighbor.pol.risk.median = NA,
                neighbor.pol.risk.sd = NA,
                neighbor.pol.risk.min = NA,
                neighbor.pol.risk.max = NA,
                neighbor.coups = NA,
                neighbor.coups.bin = NA,
                neighbor.coup.activity = NA,
                neighbor.coups.activity.bin = NA)
  }
  
  df.final <- df.chunk.summary %>%
    mutate(neighbors.count = nrow(chunk),
           neighbors.cow = paste(chunk$neighbor_cow, collapse=", "),
           neighbors.clean = paste(countrycode(chunk$neighbor_cow, "cown", 
                                               "country.name"),
                                   collapse=", "),
           country.clean = countrycode(unique(chunk$country_cow), "cown", 
                                       "country.name"))  

  return(df.final)
}

all.country.years <- expand.grid.df(neighbor.cows, 
                                    data_frame(year.num = 1991:2016))

# Add coups to ICRG so coup attempts can also be neighborized
icrg.all.with.aggregates.coups <- icrg.all.with.aggregates %>%
  left_join(coups.new, by=c("cowcode", "year.num" = "year"))

neighbor.stability <- all.country.years %>%
  group_by(country_cow, year.num) %>%
  do(summarize.neighbors(., icrg.all.with.aggregates.coups)) %>%
  rename(cowcode = country_cow)


# Distances with CShapes (more accurate than the strict neighbor definition)
# Load and tidy all distance data generated with PROJHOME/Data/R/get_distances.R
distance.folder <- file.path(PROJHOME, "Data", "data_processed", "distances")

read_distances <- function(type, year) {
  stopifnot(year >= 1990, year <= 2015)
  stopifnot(type %in% c("capital", "cent", "min"))
  
  # Read in the RDS file for the type and year
  matrix.raw <- readRDS(file.path(distance.folder, 
                                  paste0(type, "_", year, "-01-01.rds")))
  
  # Convert distance matrix to long dataframe
  df.clean <- as.data.frame(matrix.raw) %>%
    mutate(cowcode = rownames(.)) %>%
    gather(cowcode.other, distance, -cowcode) %>%
    # Set inverted diagonal 0s to 0, real 0s to 1 
    mutate(distance.inv = ifelse(cowcode == cowcode.other & distance == 0, 0,
                                 ifelse(cowcode != cowcode.other & distance == 0, 1,
                                        1 / distance))) %>%
    mutate(type = type, year = year) %>%
    mutate_each(funs(as.numeric), starts_with("cowcode")) %>%
    # Mark if country is within 900 km
    mutate(is.rough.neighbor = distance < 900) %>%
    # Standardize inverted distances so that all rows within a country add to 1
    # so that it can be used in weighted.mean.
    # This is the same as calculating the row sum of the matrix
    group_by(cowcode) %>%
    mutate(distance.std = distance.inv / sum(distance.inv)) %>%
    ungroup() %>%
    left_join(gw.lookup, by=c("cowcode" = "gwcode"))
  
  return(df.clean)
}


# Load and combine all distance matrices
all.distances <- expand.grid(year = 1991:2015,
                             type = c("capital", "cent", "min"),
                             stringsAsFactors=FALSE) %>%
  rowwise() %>% do(read_distances(.$type, .$year)) %>% ungroup()

# Because it's not possible to do use separate summarise_each() calls in a
# dplyr chain, we have to make two separate summary dataframes and join them
# later.
#
# Define the continuous and binary vars to be summarized neighborly/distancely
vars.to.summarize.cont <- c("icrg.stability", "icrg.internal",
                            "icrg.pol.risk",
                            "icrg.pol.risk.internal",
                            "icrg.pol.risk.internal.nostab",
                            "icrg.pol.risk.internal.scaled",
                            "icrg.pol.risk.internal.nostab.scaled",
                            "protests.all", "protests.all.log",
                            "protests.violent", "protests.violent.log",
                            "protests.nonviolent", "protests.nonviolent.log",
                            "protests.all.pct.all", "protests.violent.pct.all",
                            "protests.nonviolent.pct.all",
                            "protests.all.std", "protests.violent.std",
                            "protests.nonviolent.std",
                            "any.crisis_pct", "domestic.political.crisis_pct",
                            "ethnic.religious.violence_pct", 
                            "international.conflict_pct",
                            "insurgency_pct", "rebellion_pct")
vars.to.summarize.bin <- c("coups.success.bin", "coups.activity.bin")

# Make a dataframe with just those variables
vars.to.summarize.df <- icrg.all.with.aggregates %>%
  left_join(select(coups.new, -c(region, subregion)), 
            by=c("cowcode", "year.num" = "year")) %>%
  left_join(icews, by=c("cowcode", "year.num" = "event.year")) %>%
  left_join(icews.eois, by=c("cowcode" = "ccode", "year.num" = "year")) %>%
  select(cowcode, year.num, 
         one_of(vars.to.summarize.cont, vars.to.summarize.bin)) %>%
  filter(year.num > 1990)

# Dataframe of distance-related variables, such as neighbor status and 
# distance weights
distances.to.summarize <- all.distances %>%
  left_join(vars.to.summarize.df, by=c("cowcode.other" = "cowcode",
                                       "year" = "year.num")) %>%
  filter(cowcode != cowcode.other)

# Get average of variables weighted by distance
vars.weighted <- distances.to.summarize %>%
  group_by(year, cowcode, type) %>%
  summarise_each(funs(wt = weighted.mean(., distance.std, na.rm=TRUE)),
                 one_of(vars.to.summarize.cont))

# Get summaries of continuous variables in countries that are rough neighbors
vars.neighbors.cont <- distances.to.summarize %>%
  filter(is.rough.neighbor == TRUE) %>%
  group_by(year, cowcode, type) %>%
  summarise_each(funs(n_nb = n(),
                      min_nb = min(., na.rm=TRUE),
                      max_nb = max(., na.rm=TRUE),
                      mean_nb = mean(., na.rm=TRUE),
                      sd_nb = sd(., na.rm=TRUE),
                      median_nb = min(., na.rm=TRUE)),
                 one_of(vars.to.summarize.cont)) %>%
  ungroup()

# Get counts of binary variables in countries that are rough neighbors
vars.neighbors.bin <- distances.to.summarize %>%
  filter(is.rough.neighbor == TRUE) %>%
  group_by(year, cowcode, type) %>%
  summarise_each(funs(sum_nb = sum(., na.rm=TRUE)),
                 one_of(vars.to.summarize.bin))

# Join all distance-related dataframes together
vars.distance <- distances.to.summarize %>%
  tidyr::expand(year, cowcode, type) %>%  # All possible combinations 
  left_join(vars.weighted, by=c("year", "cowcode", "type")) %>%
  left_join(vars.neighbors.cont, by=c("year", "cowcode", "type")) %>%
  left_join(vars.neighbors.bin, by=c("year", "cowcode", "type"))

# Minimum distance seems to capture proximity the best, since centroid and
# capital distance are biased against large countries (i.e. the US has no
# neighbors because everything is so far away from Kansas (centroid) and DC
# (capital))
vars.distance.min <- vars.distance %>%
  filter(type == "min") %>% select(-type)


# Unified Democracy Scores (UDS)
# http://www.unified-democracy-scores.org/
uds.url <- "http://www.unified-democracy-scores.org/files/20140312/z/uds_summary.csv.gz"
uds.tmp <- paste0(tempdir(), basename(uds.url))
download.file(uds.url, uds.tmp, method="internal")

uds <- read_csv(uds.tmp) %>%
  filter(year > 1989) %>%
  rename(uds_mean = mean, uds_sd = sd, uds_median = median,
         uds_pct025 = pct025, uds_pct975 = pct975) %>%
  mutate(uds_ord = cut(uds_mean, breaks=c(-Inf, 0, Inf),
                       labels=c("Autocracy", "Democracy"),
                       ordered_result=TRUE)) %>%
  select(-country)

# UDS data only goes to 2012, but V-Dem goes to 2015
# Extend the uds_* variables to include 2015 by creating a skeleton panel of
# all cowcodes and years + 2013:2015, joining `uds`, and then creating
# uds.ever.autocracy`
uds.extended <- expand.grid(cowcode = unique(uds$cowcode),
                            year = 1991:2015) %>%
  left_join(uds, by=c("cowcode", "year")) %>%
  group_by(cowcode) %>%
  mutate(uds.ever.autocracy = any(uds_ord == "Autocracy", na.rm=TRUE)) %>%
  ungroup()


# GWF Autocratic regimes
# http://sites.psu.edu/dictators/
# This is included in V-Dem, but only `gwf_regimetype` and `gwf_nonautocracy`
gwf <- read_stata(file.path(PROJHOME, "Data", "data_raw", 
                            "External", "GWF Autocratic Regimes",
                            "GWF_AllPoliticalRegimes.dta")) %>%
  mutate_each(funs(fix.NA), gwf_casename, gwf_country, gwf_regimetype, 
              gwf_next, gwf_prior, gwf_nonautocracy) %>%
  mutate(gwf.unified = ifelse(is.na(gwf_regimetype),
                              gwf_nonautocracy, gwf_regimetype))

regime.types.simple <- read_csv(file.path(PROJHOME, "Data", "data_base",
                                          "gwf_simplified.csv"))

gwf.simplified <- gwf %>%
  left_join(regime.types.simple, by="gwf.unified") %>%
  select(cowcode, year, gwf.unified, gwf.simple, gwf.binary) %>%
  mutate(gwf.simple = factor(gwf.simple,
                             levels=c("autocracy", "democracy", "other"),
                             labels=c("Autocracy", "Democracy", "Other")),
         gwf.binary = factor(gwf.binary,
                             levels=c("autocracy", "democracy"),
                             labels=c("Autocracy", "Democracy"))) %>%
  filter(year > 1990) 

# GWF data only goes to 2010, but V-Dem goes to 2015. 
# Extend the gwf.ever.autocracy variable to include 2015 by creating a skeleton
# panel of all cowcodes and years + 2011:2015, joining gwf.simplified, and then
# creating gwf.ever.autocracy
gwf.simplfied.extended <- expand.grid(cowcode = unique(gwf.simplified$cowcode), 
                                      year = 1991:2015) %>%
  left_join(gwf.simplified, by=c("cowcode", "year")) %>%
  group_by(cowcode) %>%
  mutate(gwf.ever.autocracy = any(gwf.binary == "Autocracy", na.rm=TRUE)) %>%
  ungroup()


# Dupuy, Ron, and Prakash foreign funding restrictions
dpr <- read_csv(file.path(PROJHOME, "Data", "data_raw", 
                          "External", "DupuyRonPrakash2014",
                          "dpr.csv")) %>%
  mutate(cowcode = countrycode(country_name, "country.name", "cown"),
         imposed = TRUE)

potential.dpr.panel <- dpr %>%
  tidyr::expand(cowcode,
                year = 1990:2015)

dpr.clean <- potential.dpr.panel %>%
  left_join(dpr, by=c("cowcode", "year"="year_onset")) %>%
  mutate(foreign.funding.imposed = ifelse(is.na(imposed), FALSE, imposed)) %>%
  group_by(cowcode) %>%
  mutate(foreign.funding.count = cumsum(foreign.funding.imposed)) %>%
  ungroup() %>%
  select(-country_name, -imposed)


# Christensen and Weinstein NGO laws
# Get and load DCJW data
dcjw.url <- "https://darinchristensen.github.io/Data/DCJW_NGO_Laws.xlsx"
dcjw.name <- basename(dcjw.url)
dcjw.path <- file.path(PROJHOME, "Data", "data_raw",
                       "External", "DCJW NGO Laws", dcjw.name)

if (!file.exists(dcjw.path)) {
  download.file(url=dcjw.url, dcjw.path, "auto")
}

# Tidy DCJW data
dcjw <- read_excel(dcjw.path)[,1:50] %>%
  select(-c(dplyr::contains("source"), dplyr::contains("burden"), 
            dplyr::contains("subset"), Coder, Date)) %>%
  gather(key, value, -Country) %>%
  separate(key, c("question", "var.name"), 4) %>%
  filter(!is.na(Country)) %>%
  mutate(var.name = ifelse(var.name == "", "value", gsub("_", "", var.name))) %>%
  spread(var.name, value) %>%
  mutate(year.actual = ymd(paste0(year, "-01-01"), quiet=TRUE),
         country.name = countrycode(Country, "country.name", "country.name"))

dcjw.clean <- dcjw %>%
  mutate(barrier = case_when(
    substr(.$question, 3, 3) == 2 ~ "dcjw.entry",
    substr(.$question, 3, 3) == 3 ~ "dcjw.funding",
    substr(.$question, 3, 3) == 4 ~ "dcjw.advocacy",
    TRUE ~ NA_character_
  )) %>%
  # Can't use case_when here because it gives "NAs are not allowed in 
  # subscripted assignments" error
  mutate(value = ifelse(question == "q_2c" & value == 0, 1,
                        ifelse(question == "q_2c" & value == 1, 0, value))) %>%
  # Get rid of non-barrier questions
  filter(!is.na(barrier)) %>%
  filter(!is.na(value)) %>%
  # Get rid of rows where year is missing and regulation was not imposed
  filter(!(is.na(year) & value == 0)) %>%
  # If year is missing but some regulation exists, assume it has always already
  # existed (since 1950, arbitrarily)
  mutate(year = ifelse(is.na(year), 1950, year))

potential.dcjw.panel <- dcjw.clean %>%
  tidyr::expand(country.name, barrier, 
                year = min(.$year, na.rm=TRUE):max(.$year, na.rm=TRUE))

dcjw.by.year <- dcjw.clean %>%
  # Sum of restrictions in each type of barrier in each country each year
  group_by(country.name, barrier, year) %>%
  summarise(index = sum(value)) %>%
  # Join with full possible panel
  right_join(potential.dcjw.panel, by=c("country.name", "barrier", "year")) %>%
  # Cumulative sum of restrictions in each type of barrier in each country
  mutate(index = ifelse(is.na(index), 0, index),
         index.cum = cumsum(index)) %>%
  ungroup() %>%
  # Lop off the ancient observations
  filter(year > 1990) %>%
  # Fix Serbia
  mutate(cowcode = countrycode(country.name, "country.name", "cown"),
         cowcode = ifelse(country.name == "Serbia", 340, cowcode)) %>%
  select(-c(index, country.name)) %>%
  # Make variables for each kind of barrier + total number of barriers
  spread(barrier, index.cum) %>%
  rowwise() %>%
  mutate(dcjw.total = sum(dcjw.entry, dcjw.funding, dcjw.advocacy)) %>%
  ungroup()


# Get map data for plotting
map.url <- paste0("http://www.naturalearthdata.com/", 
                  "http//www.naturalearthdata.com/download/110m/cultural/", 
                  "ne_110m_admin_0_countries.zip")
map.path <- file.path(PROJHOME, "Data", "data_raw", 
                      "External", "Natural Earth maps")
map.zip.name <- basename(map.url)
map.name <- tools::file_path_sans_ext(map.zip.name)

# Download Natural Earth shapefiles if needed
if (!file.exists(file.path(map.path, paste0(map.name, ".shp")))) {
  download.file(url=map.url, file.path(map.path, map.zip.name), "auto")
  unzip(file.path(map.path, map.zip.name), exdir=map.path)
  file.remove(file.path(map.path, map.zip.name))
}

countries.map <- readOGR(map.path, map.name)
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- ggplot2::fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99))) %>%  # Get rid of Antarctica and NAs
  mutate(id = ifelse(id == "GRL", "DNK", id))  # Greenland is part of Denmark


# ------------------
# Merge everything!
# ------------------
# Create panel based on all countries in V-Dem from 1990 to present
full.data <- tidyr::expand(vdem.cso, year, cowcode) %>%
  # Join *everything* to the empty panel
  left_join(vdem.cso, by=c("year", "cowcode")) %>%
  left_join(dcjw.by.year, by=c("year", "cowcode")) %>%
  left_join(dpr.clean, by=c("year", "cowcode")) %>%
  left_join(vars.distance.min, by=c("year", "cowcode")) %>%
  left_join(icrg.all.with.aggregates, by=c("cowcode", "year" = "year.num")) %>%
  left_join(pol.inst, by=c("cowcode" = "cow", "year")) %>%
  left_join(nelda.full, by=c("cowcode" = "ccode", "year")) %>%
  left_join(ciri, by=c("cowcode", "year")) %>%
  left_join(wdi.clean, by=c("cowcode" = "cow","year")) %>%
  left_join(murdie, by=c("cowcode", "year")) %>%
  left_join(kof, by=c("cowcode", "year")) %>%
  left_join(neighbor.stability, by=c("cowcode", "year" = "year.num")) %>%
  left_join(uds.extended, by=c("cowcode", "year")) %>%
  left_join(coups.final, by=c("cowcode", "year")) %>%
  left_join(icews, by=c("year" = "event.year", "cowcode")) %>%
  left_join(icews.eois, by=c("cowcode" = "ccode", "year")) %>%
  left_join(gwf.simplfied.extended, by=c("cowcode", "year")) %>%
  rename(year.num = year) %>%
  mutate(year.factor = factor(year.num)) %>%
  # Standardize all country names and ISO codes
  select(-dplyr::contains("country"), -iso) %>%
  mutate(country = countrycode(cowcode, "cown", "country.name"),
         iso3 = countrycode(cowcode, "cown", "iso3c")) %>%
  mutate(country = case_when(
    .$cowcode == 521 ~ "Somaliland",
    .$cowcode == 667 ~ "Palestine (West Bank)",
    .$cowcode == 668 ~ "Palestine (Gaza)",
    .$country == "Syrian Arab Republic" ~ "Syria",
    TRUE ~ .$country
  )) %>%
  mutate(iso3 = case_when(
    .$cowcode == 347 ~ "XKX",
    .$cowcode == 521 ~ "SML",
    .$cowcode == 667 ~ "PSE",
    .$cowcode == 668 ~ "GAZ",
    TRUE ~ .$iso3
  )) %>%
  select(year.num, year.factor, year.actual, cowcode, country, iso3, everything())

# Make sure the joining didn't add any extra rows
expect_equal(nrow(full.data), nrow(tidyr::expand(vdem.cso, year, cowcode)))

# Save all cleaned data files
write_feather(full.data,
              file.path(PROJHOME, "Data", 
                        "data_processed", "full_data.feather"))

write_feather(icrg.monthly, 
              file.path(PROJHOME, "Data", 
                        "data_processed", "icrg_monthly.feather"))

saveRDS(countries.ggmap,
        file=file.path(PROJHOME, "Data", 
                       "data_processed", "countries110_robinson_ggmap.rds"))

write_feather(dcjw,
              file.path(PROJHOME, "Data", 
                        "data_processed", "dcjw.feather"))

# For Judith's book
write_feather(icrg.all,
              file.path(PROJHOME, "Data", 
                        "data_processed", "icrg_all.feather"))

