#' ---
#' title: "Analysis for survey chapter"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: ../html/fixes.css
#'     code_folding: hide
#'     toc: yes
#'     toc_float: true
#'     toc_depth: 4
#'     highlight: pygments
#'     theme: cosmo
#'     self_contained: no
#'     includes:
#'       after_body: ../html/add_home_link.html
#' bibliography: /Users/andrew/Dropbox/Readings/Papers.bib
#' csl: /Users/andrew/.pandoc/csl/american-political-science-association.csl
#' ...

#+ message=FALSE
# Load clean data
knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

library(plyr)  # first because of productplots
library(tidyverse)  # yay hadley
library(magrittr)  # For %T>%
library(forcats)  # yay clean factors
library(stringr)  # yay clean string manipulation
library(productplots)
library(pander)
library(DT)  # yay fancy HTML tables
library(tm)  # yay text analysis
library(countrycode)
library(riverplot)
library(pryr)
library(feather)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))
source(file.path(PROJHOME, "Analysis", "lib", "cat_analysis.R"))

# Reproducibility
my.seed <- 1234
set.seed(my.seed)

# Bayesian stuff
CHAINS <- 4
ITER <-2000
WARMUP <- 1000
options(mc.cores = 1)  # No need to parallelize with simple models

# Treat ordered factors as treatments in models
options(contrasts=rep("contr.treatment", 2))

# Load cleaned, country-based survey data (*with* the Q4\* loop)
survey.clean.all <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                      "survey_clean_all.rds"))

# Load cleaned, organization-based data (without the Q4 loop)
survey.orgs.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                       "survey_orgs_clean.rds"))

# Load cleaned, country-based data (only the Q4 loop)
survey.countries.clean <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                            "survey_countries_clean.rds"))

# External data
full.data <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                                    "full_data.feather"))

# Load Robinson map projection
countries.ggmap <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                                     "countries110_robinson_ggmap.rds"))

# All possible countries (to fix the South Sudan issue)
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))

# Survey responses
great.none.dk <- c("A great deal", "A lot", "A moderate amount",
                   "A little", "None at all", "Don't know", "Not applicable")
great.none <- great.none.dk[1:5]

#' ## General notes
#' 
#' Adjustments in programming ~ alignment/relationship with government + instrumental flexibility + principled concerns
#' 
#' Also, feeling of restriction ~ alignment
#' 
#' Which variables adjust more or first? Core or flexibility? Are NGOs protecting their core with flexibility?
#' 
#' Size, volunteers, money, etc. = show how NGOs differ across groups or types of issues
#' 

#' ## Survey details
#'
#' ### How many NGOs responded?
nrow(survey.orgs.clean) 


#' ### How many respondents answered questions for more than one country?
survey.countries.clean %>% 
  filter(loop.number > 1) %>% 
  nrow() %T>%
  {print(scales::percent(. / nrow(survey.orgs.clean)))}


#' ### Who in the organization responded to the survey?
df.plot.respondents <- survey.orgs.clean %>%
  group_by(Q2.3) %>%
  summarise(num = n()) %>%
  arrange(num) %>% 
  mutate(question = ordered(fct_inorder(Q2.3)))

plot.respondents <- ggplot(df.plot.respondents, aes(x=num, y=question)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0),
                     sec.axis = sec_axis(~ . / sum(df.plot.respondents$num),
                                         labels=scales::percent)) +
  labs(x="Respondents", y=NULL) +
  theme_ath()

plot.respondents +
  labs(title="Who filled out the survey?",
       subtitle="Q2.3: What is your position in your organization?")

#' Lots of others. Who are the others?
# Table
position.in.org <- survey.orgs.clean %>%
  filter(!is.na(Q2.3_TEXT)) %>%
  mutate(position.in.org = str_to_title(Q2.3_TEXT)) %>% 
  group_by(position.in.org) %>%
  summarise(num = n()) %>%
  mutate(prop = num / sum(num)) %>%
  arrange(desc(num))

datatable(position.in.org)


#' ## Where are NGOs based?
#' 
#' ### How are these NGOs distributed by HQ?
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


#' #### Number of unique HQ countries
sum(df.hq.countries$presence, na.rm=TRUE)


#' #### Regional distribution of HQ countries
df.hq.regions <- df.hq.countries %>%
  filter(!is.na(num.ngos)) %>%
  mutate(region = countrycode(Q2.2_iso3, "iso3c", "continent"),
         region = ifelse(Q2.2_iso3 == "TWN", "Asia", region)) %>%
  group_by(region) %>%
  summarise(num = sum(num.ngos, na.rm=TRUE)) %>% ungroup() %>%
  mutate(prop = num / sum(num)) %>%
  arrange(desc(num)) %>%
  mutate(region = ordered(fct_rev(fct_inorder(region))))
df.hq.regions

plot.hq.regions <- ggplot(df.hq.regions, aes(x=num, y=region)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0),
                     sec.axis = sec_axis(~ . / sum(df.hq.regions$num),
                                         labels=scales::percent)) +
  labs(x="Respondents", y=NULL) +
  theme_ath()

plot.hq.regions +
  labs(title="Region of headquarters",
       subtitle="Q2.2: Where is your organization's headquarters? (summarized by region)")


#' #### Countries with at least one response
plot.hq.map.presence <- ggplot(df.hq.countries, aes(fill=presence, map_id=Q2.2_iso3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_manual(values=c("grey50", "#FFFFFF"), na.value="#FFFFFF", guide=FALSE) +
  theme_ath_map()

plot.hq.map.presence + 
  labs(title="Countries with at least one response",
       subtitle="Q2.2: Where is your organization's headquarters?")


#' #### Responses per country (50 NGO ceiling)
plot.hq.map.scale <- ggplot(df.hq.countries, aes(fill=num.ceiling, map_id=Q2.2_iso3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(low="grey95", high="grey20", breaks=seq(0, 50, 10), 
                      labels=c(paste(seq(0, 40, 10), "  "), "50+"),
                      na.value="#FFFFFF", name="NGOs based in country",
                      guide=guide_colourbar(ticks=FALSE, barwidth=6)) +
  theme_ath_map() +
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))

plot.hq.map.scale + 
  labs(title="Number of responding NGOs around the world",
       subtitle="Q2.2: Where is your organization's headquarters? (50 NGO ceiling)")


#' ## Where do NGOs work?
#' 
#' ### Countries marked in the survey
continents.marked <- survey.orgs.clean %>%
  select(home = Q2.2_iso3, target = Q2.5_iso3) %>%
  unnest(target) %>%
  mutate_at(vars(home, target), funs(continent = countrycode(., "iso3c", "continent"))) %>%
  mutate_at(vars(home_continent, target_continent),
            funs(recode(., Oceania = "Asia &\nOceania", Asia = "Asia &\nOceania"))) %>%
  group_by(home_continent, target_continent) %>%
  summarize(times.marked = n()) %>%
  filter(!is.na(home_continent), !is.na(target_continent)) %>%
  group_by(home_continent) %>%
  mutate(perc.home = times.marked / sum(times.marked)) %>%
  group_by(target_continent) %>%
  mutate(perc.target = times.marked / sum(times.marked)) %>%
  ungroup()

#' #### Flows
continents.marked %>%
  datatable(options=list(pageLength=nrow(.))) %>%
  formatPercentage(c("perc.home", "perc.target"), 2)

edges.fancy <- continents.marked %>%
  select(N1 = home_continent, N2 = target_continent, Value = times.marked) %>%
  mutate(N2 = paste0(N2, " ")) %>%
  arrange(Value) %>%
  as.data.frame()

nodes.fancy <- edges.fancy %>%
  select(-Value) %>%
  gather(X, ID) %>%
  mutate(x = ifelse(X == "N1", 1, 2)) %>%
  distinct(ID, X, .keep_all=TRUE) %>%
  left_join(cont.colors, by="ID") %>%
  select(ID, x, col) %>%
  as.data.frame()

continents.river <- makeRiver(nodes.fancy, edges.fancy)

# Save plot to an object using a null PDF device
# http://stackoverflow.com/a/14742001/120898
# pdf(NULL)
# dev.control(displaylist="enable")
# plot(continents.river, srt=0, lty=0)
# text(1, 0, "HQ region\n", adj=c(0.5, -0.5), font=2)
# text(2, 0, "Host region\n", adj=c(0.5, -0.5), font=2)
# countries.marked.plot <- recordPlot()
# invisible(dev.off())
#
# THIS IS AMAZING. This total beats recordPlot().
# Use pryr's active binding function (%<a-%) to essentially save a chunk of R
# code to an object that can then get re-run later, like a macro.
countries.marked.plot %<a-% {
  plot(continents.river, srt=0, lty=0)
  text(1, 0, "HQ", adj=c(0.5, -0.5), font=2)
  text(2, 0, "Host", adj=c(0.5, -0.5), font=2)
  text(1.5, 0, "Countries selected\n", adj=c(0.5, -0.5), font=2)
}

countries.marked.plot

#' #### Overall summary
df.target.regions <- continents.marked %>%
  group_by(target_continent) %>%
  summarise(times.marked = sum(times.marked)) %>%
  mutate(perc.target = times.marked / sum(times.marked)) %>%
  arrange(times.marked) %>%
  mutate(target_continent = fct_inorder(target_continent))

df.target.regions %>% datatable() %>% 
  formatPercentage("perc.target", 2)

plot.target.regions <- ggplot(df.target.regions, aes(x=times.marked, y=target_continent)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0),
                     sec.axis = sec_axis(~ . / sum(df.target.regions$times.marked),
                                         labels=scales::percent)) +
  labs(x="Times marked", y=NULL) +
  theme_ath()

plot.target.regions +
  labs(title="Region of work country",
       subtitle="Q2.5: Where does your organization work? (summarized by region)")


#' ### Countries answered
continents.answered <- survey.clean.all %>%
  select(home = Q2.2_iso3, target = Q4.1_iso3) %>%
  mutate_at(vars(home, target), funs(continent = countrycode(., "iso3c", "continent"))) %>%
  mutate_at(vars(home_continent, target_continent),
            funs(recode(., Oceania = "Asia &\nOceania", Asia = "Asia &\nOceania"))) %>%
  group_by(home_continent, target_continent) %>%
  summarize(times.marked = n()) %>%
  filter(!is.na(home_continent), !is.na(target_continent)) %>%
  group_by(home_continent) %>%
  mutate(perc.home = times.marked / sum(times.marked)) %>%
  group_by(target_continent) %>%
  mutate(perc.target = times.marked / sum(times.marked)) %>%
  ungroup()

#' #### Flows
continents.answered %>%
  datatable(options=list(pageLength=nrow(.))) %>%
  formatPercentage(c("perc.home", "perc.target"), 2)

edges.fancy.answered <- continents.answered %>%
  select(N1 = home_continent, N2 = target_continent, Value = times.marked) %>%
  mutate(N2 = paste0(N2, " ")) %>%
  arrange(Value) %>%
  as.data.frame()

nodes.fancy.answered <- edges.fancy %>%
  select(-Value) %>%
  gather(X, ID) %>%
  mutate(x = ifelse(X == "N1", 1, 2)) %>%
  distinct(ID, X, .keep_all=TRUE) %>%
  left_join(cont.colors, by="ID") %>%
  select(ID, x, col) %>%
  as.data.frame()

continents.river.answered <- makeRiver(nodes.fancy.answered,
                                       edges.fancy.answered)

countries.answered.plot %<a-% {
  plot(continents.river.answered, srt=0, lty=0)
  text(1, 0, "HQ", adj=c(0.5, -0.5), font=2)
  text(2, 0, "Target", adj=c(0.5, -0.5), font=2)
  text(1.5, 0, "Countries answered\n", adj=c(0.5, -0.5), font=2)
}

countries.answered.plot

#' #### Overall summary
df.target.regions.answered <- continents.answered %>%
  group_by(target_continent) %>%
  summarise(times.marked = sum(times.marked)) %>%
  mutate(perc.target = times.marked / sum(times.marked)) %>%
  arrange(times.marked) %>%
  mutate(target_continent = fct_inorder(target_continent))

df.target.regions.answered %>% datatable() %>% 
  formatPercentage("perc.target", 2)

plot.target.regions.answered <- ggplot(df.target.regions.answered, 
                                       aes(x=times.marked, y=target_continent)) +
  geom_barh(stat="identity") +
  scale_x_continuous(expand=c(0, 0),
                     sec.axis = sec_axis(~ . / sum(df.target.regions.answered$times.marked),
                                         labels=scales::percent)) +
  labs(x="Times marked", y=NULL) +
  theme_ath()

plot.target.regions.answered +
  labs(title="Region of work country",
       subtitle="Q4.1: Which country would you like to discuss? (summarized by region)")

#' ### Both at once
#+ fig.width=8, fig.height=3.5
countries.marked.answered.both %<a-% {
  split.screen(c(1, 2))
  screen(1)
  countries.marked.plot
  screen(2)
  countries.answered.plot
  close.screen(all=TRUE) 
}

countries.marked.answered.both

# Save to file
filename <- "3-countries-marked-answered"

cairo_pdf(file.path(PROJHOME, "Output", "figures", paste0(filename, ".pdf")),
          width=8, height=3.5, family="Source Sans Pro")
countries.marked.answered.both
invisible(dev.off())

png(file.path(PROJHOME, "Output", "figures", paste0(filename, ".png")), 
    width=8, height=3.5, family="Source Sans Pro", 
    bg="white", units="in", res=300, type="cairo")
countries.marked.answered.both
invisible(dev.off())


#' ## Did NGOs choose easier countries to answer about?
#' 
#' NGOs chose one of the selected countries for the second half of the survey. Did they choose countries that were easier or harder to work in? Did the restrictivness of the country's civil society regulatory environment influence their answer?
#' 
csre.only <- full.data %>%
  select(iso3, cs_env_sum) %>%
  group_by(iso3) %>%
  mutate(csre.fixed = zoo::na.locf(cs_env_sum, na.rm=FALSE)) %>%  # Forward-fill CSRE
  summarise(csre.mean = mean(csre.fixed, na.rm=TRUE),
            csre.last = last(csre.fixed),
            csre.min = min(csre.fixed, na.rm=TRUE),
            csre.max = max(csre.fixed, na.rm=TRUE))

csre.selected <- survey.clean.all %>%
  select(ResponseID, home_iso3 = Q2.2_iso3, target_iso3 = Q2.5_iso3, answered_iso3 = Q4.1_iso3) %>%
  unnest(target_iso3) %>%
  left_join(csre.only, by=c("target_iso3" = "iso3")) %>%
  left_join(csre.only, by=c("answered_iso3" = "iso3"), suffix=c("_target", "_answered"))
  
csre.selected.answered <- csre.selected %>%
  group_by(ResponseID, answered_iso3) %>%
  summarise(avg.csre.all.targets = mean(csre.mean_target, na.rm=TRUE),
            avg.csre.answered = mean(csre.mean_answered, na.rm=TRUE)) %>%
  filter(!is.na(avg.csre.answered)) %>%
  mutate(restriction = case_when(
    avg.csre.answered < avg.csre.all.targets ~ "More restrictive than average",
    avg.csre.answered > avg.csre.all.targets ~ "Less restrictive than average",
    avg.csre.answered == avg.csre.all.targets ~ "Average restrictiveness"
  ))

ggplot(csre.selected.answered, 
       aes(y=avg.csre.all.targets, x=avg.csre.answered)) + 
  geom_abline(slope=1, size=0.25) +
  geom_vline(xintercept=0, size=0.25) +
  geom_point(aes(color=restriction), size=0.75) + 
  labs(x="Average CSRE in country answered",
       y="Average CSRE in all countries selected") +
  scale_color_manual(values=ath.palette("palette1"), name=NULL) +
  coord_cartesian(xlim=c(-6, 6.5), ylim=c(-6, 6.6)) +
  theme_ath()

#' To some extent, yes. 45% of respondents answered questions about countries that were less restrictive than the average restrictiveness of their total portfolio of countries.
#' 
#+ results="asis"
csre.selected.answered %>%
  group_by(restriction) %>%
  summarize(total = n()) %>%
  mutate(prop = total / sum(total)) %>%
  pandoc.table()

#' The scatterplot shows a kind of equal distribution of easier vs. harder (45%
#' vs. 35%). The distribution shows this a little differently, with the country
#' answered skewed left.
#' 
csre.selected.answered.dist <- csre.selected.answered %>%
  gather(key, value, avg.csre.all.targets, avg.csre.answered) %>%
  mutate(key = fct_recode(key, 
                          `All countries selected   ` = "avg.csre.all.targets",
                          `Country answered` = "avg.csre.answered"))

ggplot(csre.selected.answered.dist, aes(x=value, fill=key)) + 
  geom_density(alpha=0.5, color=NA) + 
  scale_fill_manual(values=ath.palette("palette1")[4:5], name=NULL) +
  labs(x=NULL, y=NULL) +
  theme_ath() + 
  theme(panel.grid.major.y=element_blank(),
        axis.text.y=element_blank())



#' ## What do these NGOs do?
#' 
#' ### Which issues do these NGOs work on?
df.issues <- survey.orgs.clean %>%
  unnest(Q3.1_value) %>%
  group_by(Q3.1_value) %>%
  summarise(num = n()) %>%
  arrange(desc(num)) %>%
  filter(!is.na(Q3.1_value)) %>%
  mutate(issue = factor(Q3.1_value, levels=rev(Q3.1_value), ordered=TRUE))

df.issues.denom <- sum(!is.na(survey.orgs.clean$Q3.1_value))

plot.issues <- ggplot(df.issues, aes(x=num, y=issue)) + 
  geom_barh(stat="identity") + 
  scale_x_continuous(expand=c(0, 0),
                     sec.axis = sec_axis(~ . / df.issues.denom,
                                         labels=scales::percent)) +
  labs(x="Times selected", y=NULL) +
  theme_ath()

plot.issues + 
  labs(title="Which issues do NGOs work on?",
       subtitle="Q3.1: Which issues does your organization focus on? (multiple answers allowed)")


#' ### Which issues do NGOs work on the most?
df.issues.most <- survey.orgs.clean %>%
  group_by(Q3.2) %>%
  summarise(num = n()) %>%
  arrange(desc(num)) %>%
  filter(!is.na(Q3.2)) %>%
  mutate(issue = factor(Q3.2, levels=rev(Q3.2), ordered=TRUE))

plot.issues.most <- ggplot(df.issues.most, aes(x=num, y=issue)) + 
  geom_barh(stat="identity") + 
  scale_x_continuous(expand=c(0, 0),
                     sec.axis = sec_axis(~ . / sum(df.issues.most$num),
                                         labels=scales::percent)) +
  labs(x="Respondents", y=NULL) +
  theme_ath()

plot.issues.most +
  labs(title="Which issues do NGOs work on?",
       subtitle="Q3.2: Which issues does your organization focus on the most?")

#' There are lots of others, both for all issues and most important issue. What are they?
df.other.issues <- survey.orgs.clean %>%
  filter(!is.na(Q3.1_other_TEXT)) %>%
  mutate(other.issue = str_to_title(Q3.1_other_TEXT)) %>% 
  group_by(other.issue) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

datatable(df.other.issues)

#' There are 300+ other issues here. That's a lot. Natural language processing
#' can help make sense of the mess.
# Build clean corpus for NLP
issue.corpus.df <- survey.orgs.clean %>%
  filter(!is.na(Q3.1_other_TEXT)) %>%
  mutate(other.issue = str_to_lower(Q3.1_other_TEXT)) %>%
  select(other.issue)

issue.corpus <- issue.corpus.df %>%
  DataframeSource() %>% Corpus() %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stemDocument, language="english") %>%
  tm_map(stripWhitespace)

tdm.issues <- TermDocumentMatrix(issue.corpus)

# This works, but doesn't provide frequencies, so dplyr it is
#   findFreqTerms(tdm.issues, 5)
term.frequency <- as.data.frame(as.matrix(tdm.issues)) %>%
  mutate(term = rownames(.)) %>%
  gather(document, freq, -term) %>%
  filter(freq > 0) %>%
  group_by(term) %>%
  summarise(num = n()) %>%
  ungroup()

frequent.issues <- term.frequency %>%
  filter(num >= 5) %>%
  arrange(desc(num))
frequent.issues

#' Lots of NGOs deal with rights, health, and developent. Do they qualify those
#' broader issues?
findAssocs(tdm.issues, "right", 0.1)
findAssocs(tdm.issues, "health", 0.1)
findAssocs(tdm.issues, "develop", 0.1)

#' Imputing latent themes from these other topics algorithmically is tricky though. K-means, PAM, latent semantic analysis (LSA), and latent dirichlet allocation (LDA) all choke, since the corpus isn't that big and the "documents" are super short (often just one word long). I attempt each of these in `/Analysis/ingo_survey/sandbox.R`, but none of them work well, even for coarse sorting. 
#' 
#' So, hand-coding it is.
#' 
#' Here are the results post-hand-coding:
#' 
df.issues.clean <- survey.orgs.clean %>%
  unnest(Q3.1.clean_value) %>%
  group_by(Q3.1.clean_value) %>%
  summarise(num = n()) %>%
  arrange(desc(num)) %>%
  filter(!is.na(Q3.1.clean_value)) %>%
  mutate(issue = factor(Q3.1.clean_value, levels=rev(Q3.1.clean_value), ordered=TRUE))

plot.issues.clean <- ggplot(df.issues.clean, aes(x=num, y=issue)) + 
  geom_barh(stat="identity") + 
  scale_x_continuous(expand=c(0, 0)) +
  labs(x="Times selected", y=NULL,
       title="Which issues do NGOs work on?",
       subtitle="Q3.1: Which issues does your organization focus on? (multiple answers allowed)") +
  theme_ath()

plot.issues.clean


df.issues.most.clean <- survey.orgs.clean %>%
  group_by(Q3.2.clean, potential.contentiousness) %>%
  summarise(num = n()) %>%
  filter(!is.na(Q3.2.clean)) %>%
  ungroup() %>%
  arrange(desc(num)) %>%
  mutate(issue = factor(Q3.2.clean, levels=rev(unique(Q3.2.clean)), ordered=TRUE))

plot.issues.most.clean <- ggplot(df.issues.most.clean,
                                 aes(x=num, y=issue,
                                     fill=potential.contentiousness)) + 
  geom_barh(stat="identity") + 
  scale_x_continuous(expand=c(0, 0)) +
  scale_fill_manual(values=c("grey80", "grey40"),
                    name="Potential contentiousness") +
  labs(x="Times selected", y=NULL,
       title="Which issues do NGOs work on?",
       subtitle="Q3.2: Which issues does your organization focus on the most?") +
  theme_ath()

plot.issues.most.clean


#' ### What kinds of activities do these NGOs engage in?
labels.activities <- data_frame(levels=c("aid", "education", "mobilize", 
                                         "advocacy", "monitor"),
                                labels=c("Providing direct aid and services",
                                         "Engaging in research and public education",
                                         "Mobilizing people",
                                         "Engaging in advocacy",
                                         "Monitoring and assessing the effects of policies"))

df.activities <- survey.orgs.clean %>%
  select(dplyr::contains("Q3.3"), -dplyr::contains("TEXT")) %>%
  gather(question, response) %>%
  mutate(question = str_replace(question, "Q3\\.3_", ""),
         question = factor(question, levels=labels.activities$levels,
                           labels=labels.activities$labels, ordered=TRUE)) %>%
  filter(!(response %in% c("Don't know", "Not applicable"))) %>%
  group_by(question, response) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(response = factor(response, levels=levels(survey.orgs.clean$Q3.3_aid), ordered=TRUE))

plot.activities <- ggplot(df.activities, aes(y=num, x=response)) +
  geom_bar(stat="identity") +
  labs(y="Number of responses", x=NULL,
       title="What kinds of activities are NGOs engaged in?",
       subtitle="Q3.3: Please indicate how often your organization engages in each of these types of activities") +
  facet_wrap(~ question, ncol=1) + 
  theme_ath()

plot.activities

#' ### Staffing details
#' 
#' #### Full time employees
df.employees <- survey.orgs.clean %>%
  select(Q3.4.num) %>%
  filter(!is.na(Q3.4.num))

plot.employees <- ggplot(df.employees, aes(x=Q3.4.num)) +
  geom_histogram(binwidth=0.5) +
  labs(x="Number of employees (natural log)", y="Frequency",
       title="How many employees do NGOs use?",
       subtitle="Q3.4: Approximately how many full-time employees does your organization have?") +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:5)), labels=scales::comma) +
  theme_ath()

plot.employees


#' #### Volunteers
df.volunteers <- survey.orgs.clean %>%
  select(Q3.5.num) %>%
  filter(!is.na(Q3.5.num))

plot.volunteers <- ggplot(df.volunteers, aes(x=Q3.5.num)) +
  geom_histogram(binwidth=0.5) +
  labs(x="Number of volunteers (natural log)", y="Frequency",
       title="How many volunteers do NGOs use?",
       subtitle="Q3.5: Approximately how many volunteers does your organization have?") +
  scale_x_continuous(trans="log1p", breaks=c(0, 10^(0:6), 5000000), labels=scales::comma) +
  theme_ath()

plot.volunteers


#' ### Collaboration
df.collaboration <- survey.orgs.clean %>%
  unnest(Q3.6_value) %>%
  group_by(Q3.6_value) %>%
  summarise(num = n()) %>%
  arrange(desc(num)) %>%
  filter(!is.na(Q3.6_value)) %>%
  mutate(partner = factor(Q3.6_value, levels=rev(Q3.6_value), ordered=TRUE))

# Add line break to label
levels(df.collaboration$partner)[levels(df.collaboration$partner) == "We do not collaborate with other organizations or institutions"] <-
  "We do not collaborate with other\norganizations or institutions"

plot.collaboration <- ggplot(df.collaboration, aes(x=num, y=partner)) + 
  geom_barh(stat="identity") + 
  scale_x_continuous(expand=c(0, 0)) +
  labs(x="Times selected", y=NULL,
       title="Do NGOs collaborate with other organizations?",
       subtitle="Q3.6: Does your organization collaborate with any of these organizations\nor institutions? (multiple answers allowed)") +
  theme_ath()

plot.collaboration

#' What are the other organizations?
df.collaboration.other <- survey.orgs.clean %>%
  filter(!is.na(Q3.6_other_TEXT)) %>%
  mutate(collaboration.other = str_to_title(Q3.6_other_TEXT)) %>% 
  group_by(collaboration.other) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

datatable(df.collaboration.other)

#' Seems to mostly be universities, research centers, foundations, and
#' religious groups.
#' 

#' #### Specific organizations and institutions
#' 
#' Q3.7: Please list a few of the organizations or institutions you partner
#' with most often:
df.collaboration.partners <- survey.orgs.clean %>%
  filter(!is.na(Q3.7)) %>% select(Q3.7) %>% arrange(Q3.7)

datatable(df.collaboration.partners)


#' ### Funding
labels.funding <- data_frame(levels=c("individual", "corporate", "foundation", 
                                      "home_govt", "host_govt", "other"),
                             labels=c("Individual donations",
                                      "Corporate donations",
                                      "Foundation donations",
                                      "Grants from home country",
                                      "Grants from host country",
                                      "Other"))

df.funding <- survey.orgs.clean %>%
  select(dplyr::contains("Q3.8"), -dplyr::contains("TEXT")) %>%
  gather(question, response) %>%
  mutate(question = str_replace(question, "Q3\\.8_", ""),
         question = factor(question, levels=labels.funding$levels,
                           labels=labels.funding$labels, ordered=TRUE)) %>%
  filter(!(response %in% c("Don't know", "Not applicable"))) %>%
  group_by(question, response) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(response = factor(response, 
                           levels=levels(survey.orgs.clean$Q3.8_individual), 
                           ordered=TRUE))

plot.funding <- ggplot(df.funding, aes(y=num, x=response)) +
  geom_bar(stat="identity") +
  labs(y="Number of responses", x=NULL,
       title="Where does NGO funding come from?",
       subtitle="Q3.8: How much of your organization’s funding comes from each of these sources?") +
  facet_wrap(~ question, ncol=1) + 
  theme_ath()

plot.funding

#' What other sources of funding do NGOs use?
df.funding.other <- survey.orgs.clean %>%
  filter(!is.na(Q3.8_other_TEXT)) %>%
  arrange(Q3.8_other_TEXT) %>% select(Q3.8_other_TEXT)

datatable(df.funding.other)

#' The EU, churches, membership fees, etc.
#' 


#' ## Responses to reactions
#' 
#######

reactions <- tribble(
  ~reaction,              ~reaction.clean,
  "Q4.21_funding",        "Changed sources of funding",
  "Q4.21_issues",         "Changed issues worked on",
  "Q4.21_comm_govt",      "Changed how communicate\nwith the government",
  "Q4.21_comm_donors",    "Changed how communicate\nwith donors",
  "Q4.21_locations",      "Changed locations work in",
  "Q4.21_country_office", "Changed location of country office",
  "Q4.21_local_staff",    "Used more local staff/volunteers",
  "Q4.21_foreign_staff",  "Used more foreign staff/volunteers"
)

reaction.counts <- survey.clean.all %>%
  select(ResponseID, loop.number, starts_with("Q4.21"), -dplyr::contains("_TEXT")) %>%
  gather(reaction, value, -c(ResponseID, loop.number)) %>%
  filter(value == "Yes") %>%
  count(reaction) %>%
  arrange(n) %>%
  left_join(reactions, by="reaction") %>%
  mutate(reaction.clean = ordered(fct_inorder(reaction.clean)))

# TODO: Percent labels along top
plot.reaction.counts <- ggplot(reaction.counts, aes(y=reaction.clean, x=n)) +
  geom_barh(stat="identity") +
  labs(x=NULL, y=NULL) + 
  theme_ath() + theme(panel.grid.major.y=element_blank())
plot.reaction.counts

fig.save.cairo(plot.reaction.counts, filename="3-reaction-counts",
               width=4.5, height=2.5)


#' How do different types of INGOs react differently in different political contexts?
reaction.counts <- survey.clean.all %>%
  select(ResponseID, loop.number, starts_with("Q4.21"), -dplyr::contains("_TEXT"),
         potential.contentiousness, target.regime.type) %>%
  gather(reaction, value, -c(ResponseID, loop.number,
                             potential.contentiousness, target.regime.type)) %>%
  filter(value == "Yes") %>%
  count(reaction, potential.contentiousness, target.regime.type) %>%
  group_by(potential.contentiousness, target.regime.type) %>%
  mutate(total = sum(n), prop = n / total) %>%
  left_join(reactions, by="reaction") %>%
  arrange(potential.contentiousness, target.regime.type, prop) %>%
  mutate(reaction.clean = ordered(fct_inorder(reaction.clean)))

plot.reactions.regime.issue <- ggplot(reaction.counts,
                                      aes(y=reaction.clean, x=prop)) +
  geom_barh(stat="identity") +
  labs(x=NULL, y=NULL) + 
  scale_x_continuous(labels=scales::percent) +
  theme_ath() + theme(panel.grid.major.y=element_blank()) +
  facet_wrap(~ target.regime.type + potential.contentiousness)
plot.reactions.regime.issue

#' The most common reaction to restrictions is to turn to more local staff and volunteers, likely because it provides insulation against accuations of being tools of foreign governments (and it gives organizations a better connection to trends on the ground).
#' 
#' However, though this is the most frequent reaction for highly contentious NGOs working in autocracies, it is less common compared to their low contention counterparts. FREE RESPONSE ABOUT THAT HERE. Or in-person interview with human rights organization and how they purposely don't use local staff
#' 
#' INGOs that work in autocracies change the sources of their funding more often, which makes sense given the increase in restrictions on foreign funding—that's one of the more popular types of NGO restriction. In general, contentiousness of the issue doesn't do much for INGOs working in autocracies, since the distribution of reactions is pretty much the same, with two exceptions: more high contention INGOs in autocracies change the locations they work in and change the locations of their country offices. 
#' 
#' Interestingly, INGOS that work in democracies do differ slighly by contentiousness. High contention INGOs change the issues they work on more frequently compared to their less contentious counterparts, change the locations they work in less often, and change their sources of funding more often
#' 
#' Number of changes made. Organizations were offered 8 types of reactions - on average, how many did they choose? More in autcracies (21% ; 1.7 reactions) than in democracies (13.8%; 1.1 reactions), with a 9x% chance that the difference is greater than zero. Issue contentiousness does not matter, though. Both high and low contention NGOs selected 1.3 reactions

num.changes <- survey.clean.all %>%
  select(ResponseID, loop.number, starts_with("Q4.21"), -dplyr::contains("_TEXT")) %>%
  gather(reaction, value, -c(ResponseID, loop.number)) %>%
  filter(value == "Yes") %>%
  group_by(ResponseID, loop.number) %>%
  summarize(num.changes = length(value))

survey.clean.num.changes <- survey.clean.all %>%
  left_join(num.changes, by=c("ResponseID", "loop.number"))

survey.clean.num.changes %>%
  group_by(target.regime.type) %>%
  summarise(avg.pct.changed = mean(Q4.21_percent.changed, na.rm=TRUE)) %>%
  mutate(avg.converted = avg.pct.changed * 8)

survey.clean.num.changes %>%
  group_by(potential.contentiousness) %>%
  summarise(avg.pct.changed = mean(Q4.21_percent.changed, na.rm=TRUE)) %>%
  mutate(avg.converted = avg.pct.changed * 8)

ggplot(survey.clean.num.changes, aes(x=num.changes)) + 
  geom_density() + 
  facet_wrap(~ target.regime.type)


#' INGOs make more changes in response to feeling restricted, which makes
#' sense, since they have to respond more to harsher restrictions
#' 
asdf <- survey.clean.num.changes %>%
  filter(!is.na(Q4.17), Q4.17 != "Don’t know") %>%
  group_by(Q4.17) %>%
  summarise(
    num = n(),
    avg.num.changed = mean(num.changes, na.rm=TRUE),
    avg.pct.changed = mean(Q4.21_percent.changed, na.rm=TRUE)
    ) %>%
  mutate(avg.converted = avg.pct.changed * 8)
ggplot(asdf, aes(x=avg.converted, y=Q4.17)) + geom_barh(stat="identity")

#' Relationship with the government matters. INGOs that have a poor
#' relationship with the government tend to make more changes. This possibly
#' reflects differences in application of the law? Like, governments force more
#' reactions and changes from organizations they don't like?
#' 
asdf <- survey.clean.num.changes %>%
  filter(!is.na(Q4.11), Q4.11 != "Don't know", Q4.11 != "Prefer not to answer") %>%
  group_by(Q4.11, target.regime.type) %>%
  summarise(
    num = n(),
    avg.num.changed = mean(num.changes, na.rm=TRUE),
    avg.pct.changed = mean(Q4.21_percent.changed, na.rm=TRUE)
  ) %>%
  mutate(avg.converted = avg.pct.changed * 8)
ggplot(asdf, aes(x=avg.converted, y=Q4.11)) + geom_barh(stat="identity") + facet_wrap(~ target.regime.type)



asdf <- survey.clean.num.changes %>%
  filter(!is.na(Q4.17), !is.na(Q4.11)) %>%
  filter(Q4.17 != "Don’t know", Q4.11 != "Don't know", Q4.11 != "Prefer not to answer") %>%
  group_by(Q4.17, Q4.11) %>%
  summarise(
    num = n(),
    avg.num.changed = mean(num.changes, na.rm=TRUE),
    avg.pct.changed = mean(Q4.21_percent.changed, na.rm=TRUE)
  ) %>%
  mutate(avg.converted = avg.pct.changed * 8)

ggplot(asdf, aes(x=avg.converted, y=Q4.11)) + geom_barh(stat="identity") + facet_wrap(~ Q4.17)

# Changes ~ relationship with government
survey.clean.num.changes %>%
  group_by(Q4.11) %>%
  summarise(avg.pct.changed = mean(Q4.21_percent.changed, na.rm=TRUE))
  
asdf <- survey.clean.all %>%
  filter(Q2.5_count < 100)

ggplot(asdf, aes(x=as.numeric(Q2.5_count), y=Q4.21_percent.changed)) + 
  geom_point() + geom_smooth()

num.changes <- survey.clean.all %>%
  select(ResponseID, loop.number, starts_with("Q4.21"), -dplyr::contains("_TEXT")) %>%
  gather(reaction, value, -c(ResponseID, loop.number)) %>%
  filter(value == "Yes") %>%
  group_by(ResponseID, loop.number) %>%
  summarize(num.changes = length(value))

survey.clean.num.changes <- survey.clean.all %>%
  left_join(num.changes, by=c("ResponseID", "loop.number"))


survey.clean.num.changes %>%
  group_by(target.regime.type) %>%
  summarise(asdf = mean(num.changes, na.rm=TRUE))

survey.clean.num.changes %>%
  group_by(potential.contentiousness) %>%
  summarise(asdf = mean(num.changes, na.rm=TRUE))

# General overview
# How restricted they feel
# Reactions
# DV = reactions to restrictions
# What explains those reactions

open.ended <- survey.clean.all %>%
  select(ResponseID, loop.number, contains("TEXT"), 
         Q3.9, Q3.10, Q3.11, Q3.12, Q3.13, 
         Q4.10, Q4.12, Q4.18, Q4.20, Q5.1, 
         -Q2.3_TEXT, -Q3.1_other_TEXT)
  
# How many open-ended questions were there?
open.ended %>% ncol() - 2

# How many open-ended questions did respondents answer?
open.ended.respondnents <- open.ended %>%
  gather(key, value, -ResponseID, -loop.number) %>%
  filter(!is.na(value)) %>%
  group_by(ResponseID, loop.number) %>%
  summarise(n = n())

ggplot(open.ended.respondnents, aes(x=n)) +
  geom_histogram(binwidth=1) + 
  labs(x="Number of open-ended questions answered", y="Count") + 
  theme_ath()

summary(open.ended.respondnents$n)

# How many people skipped all open-ended questions?
skipped <- sum(!(survey.orgs.clean$ResponseID %in% open.ended.respondnents$ResponseID))
skipped
skipped / nrow(survey.countries.clean)
