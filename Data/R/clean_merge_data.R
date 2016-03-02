# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(lubridate)
library(countrycode)
library(WDI)

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


# ------------------
# Useful functions
# ------------------
# What it says on the proverbial tin: convert -999 to NA
fix.999 <- function(x) {
  ifelse(x == -999, NA, x)
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

# Varieties of Democracy
# https://v-dem.net/en/
vdem.raw <- read_csv(file.path(PROJHOME, "Data", "data_raw", "External", "V-Dem", 
                               "Country-Year, V-Dem + other, CVS/V-Dem-DS-CY+Others-v5.csv"))

# Missing COW codes
# vdem.raw %>% filter(is.na(COWcode)) %>% select(country_name) %>% unique

# Extract civil society-related variables
vdem.cso <- vdem.raw %>% select(country_name, country_id, country_text_id, 
                                year, COWcode, e_polity2, 
                                v2x_frassoc_thick, v2xcs_ccsi,
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
  group_by(COWcode) %>%
  mutate(v2csreprss_ord.lead = lead(v2csreprss_ord),
         v2csreprss.lead = lead(v2csreprss),
         v2cseeorgs.lead = lead(v2cseeorgs),
         cs_env_sum.lead = lead(cs_env_sum))


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
  mutate(cowcode = ifelse(Country == "Hong Kong", 715, cowcode),
         cowcode = ifelse(Country == "New Caledonia", 1012, cowcode)) %>%
  # Deal with Serbia 
  mutate(cowcode = ifelse(Country == "Serbia *", 345, cowcode),
         cowcode = ifelse(Country == "Serbia & Montenegro *", 345, cowcode)) %>%
  # Deal with duplicate COWs (Russia, Germany, and Serbia) where names change
  # by collapsing all rows and keeping the max value
  group_by(year.num, cowcode) %>%
  mutate_each(funs(max(., na.rm=TRUE)), starts_with("icrg")) %>%
  filter(!(Country %in% c("Serbia & Montenegro *", "USSR", "West Germany"))) %>%
  ungroup() %>%
  mutate(subregion = countrycode(iso, "iso3c", "region"),
         subregion = ifelse(iso == "TWN", "Eastern Asia", subregion)) %>%
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
         icrg.pol.risk.internal.scaled = icrg.rescale(icrg.pol.risk.internal),
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
         iso = countrycode(country.name, "country.name", "iso3c"),
         # Manually deal with these edge cases
         cowcode = ifelse(country.name == "Hong Kong", 715, cowcode),
         cowcode = ifelse(country.name == "New Caledonia", 1012, cowcode),
         cowcode = ifelse(country.name == "Serbia", 345, cowcode),
         cowcode = ifelse(country.name == "Serbia & Montenegro", 345, cowcode)) %>%
  mutate(icrg.pol.risk = icrg.stability + icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.external + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + 
           icrg.accountability + icrg.bureau,
         icrg.pol.risk.internal = icrg.stability + icrg.socioeconomic + 
           icrg.investment + icrg.internal + icrg.corruption + 
           icrg.military + icrg.religion + icrg.law + icrg.ethnic + icrg.bureau,
         icrg.pol.risk.internal.scaled = icrg.rescale(icrg.pol.risk.internal),
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
  mutate(countryname = ifelse(countryname == "UAE", 
                              "United Arab Emirates", countryname),
         countryname = ifelse(countryname == "GDR", 
                              "German Democratic Republic", countryname),
         countryname = ifelse(countryname == "S. Africa",
                              "South Africa", countryname),
         countryname = ifelse(countryname == "Dom. Rep.",
                              "Dominican Republic", countryname)) %>%
  mutate(cow = countrycode(countryname, "country.name", "cown"),
         year = as.numeric(year)) %>%
  select(year, cow, yrsoffc, finittrm, 
         opp1vote, oppfrac, opp1seat, totalseats)


# National Elections Across Democracy and Autocracy (NELDA)
# http://hyde.research.yale.edu/nelda/
# 
# TODO: type of most recent election? + time since previous election?
# True if all elections that year were competitive
nelda <- read_dta(file.path(PROJHOME, "Data", "data_raw", "External",
                            "NELDA", "id & q-wide.dta")) %>%
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
wdi.countries <- countrycode(na.exclude(unique(icrg.all$cowcode)), "cown", "iso2c")
wdi.raw <- WDI(country="all", wdi.indicators, extra=TRUE, start=1981, end=2016)

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
  select(year, cowcode, countngo)


# KOF Index of Globalization
# http://globalization.kof.ethz.ch/
capture.output({
  kof <- read_excel(file.path(PROJHOME, "Data", "data_raw", "External", "KOF",
                              "globalization_2015_long.xls"), 
                    sheet="data long", skip=1, na=".") %>%
    mutate(cowcode = countrycode(code, "iso3c", "cown")) %>%
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
  mutate(country_cow = ifelse(country_iso3 == "SRB", 345, country_cow),
         country_cow = ifelse(country_iso3 == "HKG", 715, country_cow),
         neighbor_cow = ifelse(neighbor_iso3 == "SRB", 345, neighbor_cow),
         neighbor_cow = ifelse(neighbor_iso3 == "HKG", 715, neighbor_cow)) %>%
  filter(complete.cases(.)) %>%
  select(contains("cow")) %>%
  unique()

summarize.neighbors <- function(chunk) {
  df.chunk <- icrg.all %>%
    filter(year.num == unique(chunk$year.num),
           cowcode %in% chunk$neighbor_cow) %>%
    select(icrg.stability, icrg.pol.risk.internal.scaled)

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
                neighbor.pol.risk.max = max(icrg.pol.risk.internal.scaled, na.rm=TRUE))
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
                neighbor.pol.risk.max = NA)
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

neighbor.stability <- all.country.years %>%
  group_by(country_cow, year.num) %>%
  do(summarize.neighbors(.)) %>%
  rename(cowcode = country_cow)


# ------------------
# Merge everything!
# ------------------
# TODO: Consolidate extra variables (e.g. there's "Country", "country.name", and "country_name")
# TODO: What happens when there are no neighbors?
full.data <- icrg.all %>% 
  left_join(vdem.cso, by=c("cowcode" = "COWcode", "year.num" = "year")) %>%
  left_join(pol.inst, by=c("cowcode" = "cow", "year.num" = "year")) %>%
  left_join(nelda.full, by=c("cowcode" = "ccode", "year.num" = "year")) %>%
  left_join(ciri, by=c("cowcode", "year.num" = "year")) %>%
  left_join(wdi.clean, by=c("cowcode" = "cow", "year.num" = "year")) %>%
  left_join(murdie, by=c("cowcode", "year.num" = "year")) %>%
  left_join(kof, by=c("cowcode", "year.num" = "year")) %>%
  left_join(neighbor.stability, by=c("cowcode", "year.num")) %>%
  filter(year.num > 1990)

# Save all cleaned data files
saveRDS(full.data, file.path(PROJHOME, "Data", 
                             "data_processed", "full_data.rds"))

saveRDS(icrg.monthly, file.path(PROJHOME, "Data",
                                "data_processed", "icrg_monthly.rds"))
