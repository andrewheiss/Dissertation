library(dplyr)        # For magic dataframe manipulation
library(tidyr)        # For more magic dataframe manipulation
library(countrycode)  # Standardize countries
library(rvest)        # Scrape stuff from the web
library(pander)       # Markdown

# World Bank countries
wb.countries.raw <- read_html("https://web.archive.org/web/20160422022535/http://data.worldbank.org/country/") %>%
  html_nodes(xpath='//*[@id="block-views-countries-block_1"]/div/div/div/table') %>%
  html_table() %>% bind_rows() %>% as_data_frame()
wb.countries.raw

# Clean up list of countries and add standard codes
wb.countries.clean <- wb.countries.raw %>%
  # The table from their website uses four columns; gather those into one
  gather(key, `Country name`, everything()) %>%
  select(-key) %>%
  mutate(ISO3 = countrycode(`Country name`, "country.name", "iso3c"),
         `COW code` = countrycode(ISO3, "iso3c", "cown")) %>%
  filter(!is.na(ISO3)) %>%
  mutate(`Qualtrics ID` = 1:n())
wb.countries.clean

# Nice Markdown table
pandoc.table.return(wb.countries.clean, justify="lccc") %>%
  cat(., file=file.path(PROJHOME, "Data", "Survey", "output", "survey_countries.md"))

# Regex for question validation
gsub("\\.", "\\\\.", paste(wb.countries.clean$`Country name`, collapse="|")) %>%
  cat(., file=file.path(PROJHOME, "Data", "Survey", "output", "survey_regex.txt"))

# Plain text list of countries for Qualtrics
cat(paste0(wb.countries.clean$`Country name`, collapse="\n"),
    file=file.path(PROJHOME, "Data", "Survey", "output", "survey_countries.txt"))

# Save as RDS
saveRDS(wb.countries.clean,
        file.path(PROJHOME, "Data", "Survey", "output",
                  "survey_countries.rds"))
