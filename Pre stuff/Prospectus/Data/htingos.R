library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(Cairo)
library(grid)
library(countrycode)
library(rgeos)
library(maptools)

theme_blank_map <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank(),
          legend.text=element_text(size=rel(0.7), family="Source Sans Pro Light"),
          legend.title=element_text(size=rel(0.9), family="Source Sans Pro Semibold"))
  ret
}

load("~/Research/••Projects/Human trafficking/human-trafficking-survey/Data/responses.RData")

countries.small <- countries %>%
  mutate(home.country.qual = country_id, home.ISO3 = as.character(ISO3),
         work.country.qual = country_id, work.ISO3 = as.character(ISO3),
         home.country = countrycode(home.ISO3, "iso3c", "country.name"),
         work.country = countrycode(work.ISO3, "iso3c", "country.name")) %>%
  select(home.country.qual, home.ISO3, home.country, 
         work.country.qual, work.ISO3, work.country)


org.countries.small <- responses.countries %>%
  select(survey.id, work.country.qual = Q3.2, sit.on.board = Q3.27, 
         board.required = Q3.28, restricted = Q3.29, how.restricted = Q3.30) %>%
  left_join(select(countries.small, work.country.qual, work.ISO3, work.country), 
            by="work.country.qual") %>%
  select(-work.country.qual)

new.cats <- list("Not restricted"=levels(org.countries.small$restricted)[1:3],
                 "Restricted"=levels(org.countries.small$restricted)[4:5], 
                 "Don't know"=c("Don't know")) %>%
  plyr::ldply(function(x) data.frame(restricted = x),
              .id = "restricted.bin") %>%
  mutate(restricted.bin = factor(restricted.bin, 
                                 levels=c("Not restricted", "Restricted", "Don't know"),
                                 ordered=TRUE))

org.countries.small %<>%
  left_join(new.cats, by="restricted")



work.countries <- org.countries.small %>%
  group_by(work.ISO3) %>%
  summarize(reports.bin = ifelse(any(restricted.bin == "Restricted"), TRUE, FALSE)) %>%
  mutate(reports.bin = factor(reports.bin, 
                              labels=c("Few or no restrictions reported    ", 
                                       "Restrictions reported")))

# Load map information
world.map <- readShapeSpatial("~/Research/•Sandbox/urgent_actions/map_data/ne_50m_admin_0_countries.shp")
world.ggmap <- ggplot2::fortify(world.map, region = "iso_a3")

# Make a data frame of all potential countries
all.countries <- data_frame(id = as.character(world.map$iso_a3)) %>%
  filter(id != "-99")


restrictions.full <- all.countries %>% 
  left_join(work.countries, by=c("id" = "work.ISO3")) %>%
  mutate(country = countrycode(id, "iso3c", "country.name")) %>%
  filter(id != "ATA")# Get rid of Antarctica

restrictions.map <- ggplot(restrictions.full, aes(map_id=id)) +
  geom_map(aes(fill=reports.bin), map=world.ggmap, colour="black", size=0.1) +
  expand_limits(x=world.ggmap$long, y=world.ggmap$lat) + 
  coord_map(xlim=c(-180,180), ylim=c(-60, 90)) + 
  scale_fill_manual(values=c("#4daf4a", "#e41a1c"), na.value="grey", name="") + 
  theme_blank_map() + theme(legend.position = "bottom", 
                            legend.key.size=unit(0.55, "line"))
restrictions.map
ggsave(restrictions.map, filename="~/Desktop/restrictions_map.pdf", 
       width=6.5, height=4, units="in", device=cairo_pdf)

# Of the X organizations that responded, Y reported that they faced government restrictions in the countries they worked in. 

# nrow(responses.org) human trafficking NGOs answered questions about their work in length(unique(org.countries.small$work.ISO3)) different countries.
# 

org.countries.small %>%
  filter(restricted.bin == "Restricted") %>%
  group_by(work.ISO3) %>%
  summarize(reports = n()) %>%
  nrow()


orgs.small <- responses.org %>%
  select(survey.id, org.name = Q1.2, home.country.qual = Q1.4, work.only.us) %>%
  left_join(select(countries.small, home.country.qual, home.ISO3, home.country), 
            by="home.country.qual") %>%
  select(-home.country.qual)


restrictions.df <- org.countries.small %>%
  left_join(orgs.small, by="survey.id")

table(restrictions.df$restricted)
table(restrictions.df$restricted.bin)

respondents.restricted <- restrictions.df %>%
  group_by(survey.id) %>%
  summarize(reports.bin = ifelse(any(restricted.bin == "Restricted"), TRUE, FALSE)) %>%
  select(reports.bin) %>% table()

restrictions.df %>% filter(restricted.bin == "Restricted") %>%
  select(work.country) %>% table()


# Polity IV
# http://www.systemicpeace.org/polity/polity4.htm
p4 <- read.csv("p4v2012.csv") %>%
  mutate(ISO3 = countrycode(ccode, "cown", "iso3c")) %>%
  filter(year == "2011") %>%
  select(work.ISO3 = ISO3, work.polity = polity2) %>%
  mutate(work.regime = cut(work.polity, breaks=c(-10, -6, 5, 10), 
                           include.lowest=TRUE, ordered_result=TRUE,
                           labels=c("Autocracy", "Anocracy", "Democracy")))


df <- org.countries.small %>%
  left_join(orgs.small, by="survey.id") %>%
  left_join(p4, by="work.ISO3")


plot.data <- df %>%
  select(sit.on.board, work.regime) %>% na.omit()
ggplot(plot.data, aes(x=sit.on.board, fill=work.regime)) + geom_bar(position="dodge")

board.req <- df %>% filter(sit.on.board == "Yes") %>% 
  select(org.name, work.country, board.required, work.regime) %>%
  arrange(board.required, work.regime)

plot.data <- df %>%
  select(board.required, work.regime) %>% na.omit()
ggplot(plot.data, aes(x=board.required, fill=work.regime)) + geom_bar(position="dodge")


plot.data <- df %>%
  select(survey.id, restricted.bin, work.regime) %>% na.omit()
ggplot(plot.data, aes(x=restricted.bin, fill=work.regime)) + 
  geom_bar(position="dodge") + 
  coord_flip()
