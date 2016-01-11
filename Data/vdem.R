library(readr)
library(dplyr)
library(ggplot2)

vdem.raw <- read_csv("~/Downloads/Country-Year, V-Dem, CSV/V-Dem-DS-CY-v5.csv")

vdem.cso <- vdem.raw %>% select(country_name, country_id, country_text_id, 
                                year, COWcode,
                                starts_with("v2cseeorgs"), 
                                starts_with("v2csreprss"), 
                                starts_with("v2cscnsult"),
                                starts_with("v2csprtcpt"), 
                                starts_with("v2csgender"), 
                                starts_with("v2csantimv"))

plot.data <- vdem.cso %>%
  select(country_name, year, v2csreprss_ord, v2csreprss_osp) %>%
  filter(country_name %in% c("Egypt", "China"))

ggplot(plot.data, aes(x=year, y=v2csreprss_ord, colour=country_name)) + geom_line()
ggplot(plot.data, aes(x=year, y=v2csreprss_osp, colour=country_name)) + geom_line()
