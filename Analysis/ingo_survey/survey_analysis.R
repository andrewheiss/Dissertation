#' ---
#' title: "Survey analysis"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: ../html/fixes.css
#'     toc: yes
#'     highlight: pygments
#'     theme: cosmo
#'     includes:
#'       after_body: ../html/jump.html
#'     keep_md: yes
#' bibliography: /Users/andrew/Dropbox/Readings/Papers.bib
#' csl: /Users/andrew/.pandoc/csl/american-political-science-association.csl
#' ...

#' # Load clean data and set everything up
#+ message=FALSE
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggstance)
library(stringr)
library(pander)
library(magrittr)
library(DT)
library(scales)
library(countrycode)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

# Load cleaned, country-based survey data (*with* the Q4\* loop)
survey.clean.all <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                      "survey_clean_all.rds"))

# Load cleaned, organization-based data (without the Q4 loop)
survey.orgs.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                       "survey_orgs_clean.rds"))

# Load cleaned, country-based data (only the Q4 loop)
survey.countries.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                            "survey_countries_clean.rds"))

# Load Robinson map projection
countries.ggmap <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                                     "countries110_robinson_ggmap.rds"))

# All possible countries (to fix the South Sudan issue)
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))


#' # General questions
#'
#' ## How many NGOs responded?
nrow(survey.orgs.clean) 


#' ## How many respondents answered questions for more than one country?
survey.countries.clean %>% filter(loop.number > 1) %>% nrow %T>%
  {print(percent(. / nrow(survey.orgs.clean)))}


#' ## Who in the organization responded to the survey?
df.plot.respondents <- survey.orgs.clean %>%
  group_by(Q2.3) %>%
  summarise(num = n()) %>%
  arrange(num) %>% 
  mutate(question = factor(Q2.3, levels=Q2.3, ordered=TRUE))

plot.respondents <- ggplot(df.plot.respondents, aes(x=num, y=question)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0)) +
  labs(x="Number of respondents", y=NULL, 
       title="Who filled out the survey?",
       subtitle="Q2.3: What is your position in your organization?") +
  theme_ath()

plot.respondents

#' Lots of others. Who are the others?
# Table
position.in.org <- survey.orgs.clean %>%
  filter(!is.na(Q2.3_TEXT)) %>%
  mutate(position.in.org = str_to_title(Q2.3_TEXT)) %>% 
  group_by(position.in.org) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

datatable(position.in.org)

# Figure
df.plot.respondents.other <- position.in.org %>%
  filter(num >= 5) %>%
  mutate(question = factor(position.in.org, 
                           levels=rev(position.in.org), ordered=TRUE))

plot.respondents.other <- ggplot(df.plot.respondents.other,
                                 aes(x=num, y=question)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0)) +
  labs(x="Number of respondents", y=NULL,
       title="What are the positions of those who answered “Other”?",
       subtitle="Q2.3: What is your position in your organization?: Free responses to “Other”") +
  theme_ath()

plot.respondents.other


#' ## How are these NGOs distributed by HQ?
df.hq.countries <- survey.orgs.clean %>%
  group_by(Q2.2_iso3) %>%
  summarise(num.ngos = n()) %>%
  ungroup() %>%
  # Combine with list of all possible mappable countries
  right_join(possible.countries, by=c("Q2.2_iso3"="id")) %>%
  mutate(num.ceiling = ifelse(num.ngos >= 50, 50, num.ngos),
         prop = num.ngos / sum(num.ngos, na.rm=TRUE),
         country.name = countrycode(Q2.2_iso3, "iso3c", "country.name"),
         presence = num.ngos >= 1) %>%
  arrange(desc(num.ngos))

datatable(df.hq.countries)


#' ### Number of unique HQ countries
sum(df.hq.countries$presence, na.rm=TRUE)


#' ### Regional distribution of HQ countries
df.hq.regions <- df.hq.countries %>%
  filter(!is.na(num.ngos)) %>%
  mutate(region = countrycode(Q2.2_iso3, "iso3c", "continent"),
         region = ifelse(Q2.2_iso3 == "TWN", "Asia", region)) %>%
  group_by(region) %>%
  summarise(num = sum(num.ngos, na.rm=TRUE)) %>% ungroup() %>%
  mutate(prop = num / sum(num)) %>%
  arrange(desc(num)) %>%
  mutate(region = factor(region, levels=rev(region), ordered=TRUE))
df.hq.regions

plot.hq.regions <- ggplot(df.hq.regions, aes(x=num, y=region)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0)) +
  labs(x="Number of respondents", y=NULL, 
       title="Region of headquarters",
       subtitle="Q2.2: Where is your organization's headquarters? (summarized by region)") +
  theme_ath()
plot.hq.regions


#' ### Countries with at least one response
plot.hq.map.presence <- ggplot(df.hq.countries, aes(fill=presence, map_id=Q2.2_iso3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_manual(values=c("grey50", "#FFFFFF"), na.value="#FFFFFF", guide=FALSE) +
  labs(title="Countries with at least one response",
       subtitle="Q2.2: Where is your organization's headquarters?") +
  theme_ath_map()
plot.hq.map.presence


#' ### Responses per country (50 NGO ceiling)
plot.hq.map.scale <- ggplot(df.hq.countries, aes(fill=num.ceiling, map_id=Q2.2_iso3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(low="grey95", high="grey20", breaks=seq(0, 50, 10), 
                      labels=c(paste(seq(0, 40, 10), "  "), "50+"),
                      na.value="#FFFFFF", name="NGOs based in country",
                      guide=guide_colourbar(ticks=FALSE, barwidth=6)) + 
  labs(title="Number of responding NGOs around the world",
       subtitle="Q2.2: Where is your organization's headquarters? (50 NGO ceiling)") +
  theme_ath_map() +
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))
plot.hq.map.scale


#' ## Where do these NGOs work?
df.work.countries.all <- survey.orgs.clean %>%
  unnest(Q2.5_iso3) %>%
  group_by(Q2.5_iso3) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  # Combine with list of all possible mappable countries
  right_join(possible.countries, by=c("Q2.5_iso3"="id")) %>%
  mutate(prop = num / sum(num, na.rm=TRUE),
         country.name = countrycode(Q2.5_iso3, "iso3c", "country.name"),
         presence = num >= 1) %>%
  arrange(desc(num))

datatable(df.work.countries.all)

#' ### Regional distribution of countries where NGOs report working
df.work.regions.all <- survey.orgs.clean %>%
  unnest(Q2.5_iso3) %>%
  mutate(region = countrycode(Q2.5_iso3, "iso3c", "continent"),
         region = ifelse(Q2.5_iso3 == "TWN", "Asia", region),
         region = ifelse(Q2.5_iso3 == "XKX", "Europe", region)) %>%
  group_by(region) %>%
  summarise(num = n()) %>% ungroup() %>%
  mutate(prop = num / sum(num)) %>%
  arrange(desc(num))
df.work.regions.all

#' Not plotting because the organization-county-region unit is a little wonky.
#' 


#' ### Number of unique countries where work is reported
sum(df.work.countries.all$presence, na.rm=TRUE)

#' That's pretty much every country! There are 172 recognized countries in the
#' map file, so mapping them out is kind of pointless.
#' 


#' ### Responses per country
plot.work.all.map.scale <- ggplot(df.work.countries.all,
                                  aes(fill=num, map_id=Q2.5_iso3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(low="grey95", high="grey20", #breaks=seq(0, 200, 50), 
                      # labels=c(paste(seq(0, 150, 50), "  "), "50+"),
                      na.value="#FFFFFF", name="NGOs reporting work in country",
                      guide=guide_colourbar(ticks=FALSE, barwidth=6)) + 
  labs(title="Countries where NGOs work",
       subtitle="Q2.5: Besides 'home_country', where does your organization work?") +
  theme_ath_map() +
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))
plot.work.all.map.scale

# TODO: How many work in/answered questions for/are based in authoritarian countries?

# df.work.countries.answered


#' 
#' Count all the countries they selected + the countries they answered for
#' 
#' # What do these NGOs do?
#' 
#' What kind of issues do the NGOs work on? Which issues do they work on the most?
#' 
#' What kinds of activities do these NGOs engage in?
#' 
#' How big are these NGOs? Staff? Volunteers?
#' 
#' Do NGOs collaborate?
#' 
#' Where does their funding come from?
#' 
#' # Deeper principles (mission, vision, values)
#' 
#' Q3.9 - Q3.13
#' 

#' # Testing hypotheses
#' 
#' My claim:
#' 
#' > If an INGO does not make one of these three adjustments—shifting its ideal
#' point, increasing its operational flexibility, or becoming essential for the
#' regime—it runs a high risk of expulsion.
#' 
#' What influences feelings of restriction? Main hypotheses:
#' 
#' - Normative principles and ideals
#'     - Issue area
#' - Instrumental flexibility
#'     - Size
#'     - Type of funding
#'     - Collaboration
#' - Alignment with regime preferences
#' 
