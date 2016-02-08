# Load libraries
library(dplyr)
library(tidyr)

# Map libraries
# You must install geos (http://trac.osgeo.org/geos/) and 
# gdal (http://www.gdal.org/) first. 
# Easy to do on OS X: `brew install geos gdal`
# Then install these packages from source
# install.packages(c("rgeos", "rgdal"), type="source")
library(rgdal)
library(rgeos)
library(spdep)


# Variables for getting map shapefiles
map.url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
map.path <- file.path("Raw data", "maps")
map.zip.name <- basename(map.url)
map.name <- tools::file_path_sans_ext(map.zip.name)

# Download Natural Earth shapefiles
download.file(url=map.url, file.path(map.path, map.zip.name), "auto")
unzip(file.path(map.path, map.zip.name), exdir=map.path)
file.remove(file.path(map.path, map.zip.name))

# Load shapefiles
countries <- readOGR(map.path, map.name)

# Extract the ISO codes and map them to the numeric row names
country.names <- data_frame(id = row.names(countries@data),
                            country_iso3 = as.character(countries@data$adm0_a3_is),
                            neighbor_iso3 = country_iso3)

# Determine which countries are neighbors
# Adapted from http://stackoverflow.com/a/32318128/120898
#
# spdep::poly2nb/nb2mat method is faster and more accurate than rgeos::gTouches
#
# gTouches gives wrong results; doesn't see Russia-North Korea border; is suuuuuuuper slow
#   neighbor.matrix <- gTouches(countries, byid=TRUE)
#
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
head(all.neighbors)
#
#   country_iso3 neighbor_iso3
# 1          CHN           AFG
# 2          IRN           AFG
# 3          PAK           AFG
# 4          TJK           AFG
# 5          TKM           AFG
# 6          UZB           AFG

neighbor.summary <- all.neighbors %>%
  group_by(country_iso3) %>%
  summarise(num = n()) %>%
  arrange(desc(num))
neighbor.summary
#
#    country_iso3   num
#           (chr) (int)
# 1           CHN    16
# 2           RUS    14
# 3           SRB    12
# 4           BRA    10
# 5           FRA    10
# 6           COD     9
# 7           DEU     9
