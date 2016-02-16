library(dplyr)
library(readr)
library(readxl)
library(countrycode)

# See Jay Ulfelder's version: https://github.com/ulfelder/dart-throwing-chimp/blob/master/f.countryyears.R

polity.url <- "http://www.systemicpeace.org/inscr/p4v2014.xls"
polity.tmp <- paste0(tempdir(), basename(polity.url))
download.file(polity.url, polity.tmp)

uds.url <- "http://www.unified-democracy-scores.org/files/20140312/z/uds_summary.csv.gz"
uds.tmp <- paste0(tempdir(), basename(uds.url))
download.file(uds.url, uds.tmp, method="internal")

polity <- read_excel(polity.tmp) %>%
  filter(year > 1989) %>%
  select(cowcode = ccode, year, democ, autoc, polity2, durable)

uds <- read_csv(uds.tmp) %>%
  filter(year > 1989)

scores <- polity %>% left_join(uds, by=c("year", "cowcode"))


asdf <- scores %>%
  group_by(cowcode) %>%
  summarize(min.polity = min(polity2, na.rm=TRUE),
            avg.uds = mean(mean, na.rm=TRUE)) %>%
  mutate(countryname = countrycode(cowcode, "cown", "country.name"))

regimes.polity <- asdf %>%
  filter(min.polity < 0)

regimes.uds <- asdf %>%
  filter(avg.uds < 0)

qwer <- regimes.polity %>%
  filter(!(countryname %in% regimes.uds$countryname))


set.seed(1234)
sim.scores <- scores %>%
  rename(uds_mean = mean, uds_sd = sd) %>%
  rowwise() %>%
  mutate(uds.sim = ifelse(is.na(uds_mean), NA_real_, 
                          mean(rnorm(1000, uds_mean, uds_sd))))
