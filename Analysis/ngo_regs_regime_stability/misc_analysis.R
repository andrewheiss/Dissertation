#' ---
#' title: "Non-regression analysis"
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
#' ---

#' # Load libraries and data
library(magrittr)
library(dplyr)
library(ggplot2)
library(Cairo)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

full.data <- readRDS(file.path(PROJHOME, "Data", "data_processed",
                               "full_data.rds"))


#' # General data summary
#' 
#' Number of countries:
length(unique(full.data$Country))

#' Range of years:
min(full.data$year.num)
max(full.data$year.num)

#' # Understanding and visualizing civil society environment
#' 
#' The V-Dem Bayesian measurement model is cool and all, but it's really hard
#' to interpret by itself. Showing a few example countries can help. 
cs.plot.all <- full.data %>%
  filter(year.num > 2000) %>%
  group_by(Country) %>%
  summarise(env.mean = mean(cs_env_sum, na.rm=TRUE)) %>%
  filter(!is.na(env.mean)) %>%
  arrange(env.mean) %>%
  ungroup()

cs.plot <- bind_rows(cs.plot.all %>% slice(1:5),
                     data_frame(Country = "—", env.mean = 0),
                     cs.plot.all %>% slice((nrow(.) - 4):nrow(.))) %>%
  mutate(Country = factor(Country, levels=unique(Country), ordered=TRUE))

#' The usual suspects get their scores
cs.plot

#' Average volatility/range by regime type:
cs.plot.all %>% 
  left_join(select(full.data, Country, polity_ord), by="Country") %>%
  group_by(polity_ord) %>% summarise(index.change.avg = mean(env.mean))

cs.plot.all %>% 
  left_join(select(full.data, Country, polity_ord2), by="Country") %>%
  group_by(polity_ord2) %>% summarise(index.change.avg = mean(env.mean))

plot.csre.top.bottom <- ggplot(cs.plot, aes(x=Country, y=env.mean)) +
  geom_point(size=2) + 
  geom_segment(aes(yend=0, xend=Country), size=1) + 
  labs(x=NULL, y="Mean civil society regulatory environment index (CSRE)") +
  coord_flip() +
  theme_ath()
plot.csre.top.bottom

fig.save.cairo(plot.csre.top.bottom, filename="1-csre-top-bottom", 
               width=5, height=2.5)


#' # Understanding and visualizing ICRG
#' 
#' Conceptualizing the political risk measure is a little tricky. Showing a few
#' example countries can help. Figure out which countries have change the
#' least/most since 2000:
risk.stats <- full.data %>%
  filter(year.num > 2000) %>%
  group_by(Country) %>%
  summarise(index.change = max(icrg.pol.risk.internal.scaled) - 
              min(icrg.pol.risk.internal.scaled)) %>%
  filter(!is.na(index.change)) %>%
  arrange(index.change) %T>%
  {print(head(., 5))} %>% {print(tail(., 5))}

#' Norway and Congo are the most consistent; Syria saw the biggest change.
#'
#' Average volatility/range by regime type:
risk.stats %>% 
  left_join(select(full.data, Country, polity_ord), by="Country") %>%
  group_by(polity_ord) %>% summarise(index.change.avg = mean(index.change))

risk.stats %>% 
  left_join(select(full.data, Country, polity_ord2), by="Country") %>%
  group_by(polity_ord2) %>% summarise(index.change.avg = mean(index.change))


#' Visualize changes:
example.countries <- c(652, 385, 484)
example.stability <- full.data %>%
  filter(year.num > 2000, cowcode %in% example.countries) %>%
  mutate(country.plot = factor(Country,
                               levels=c("Norway", "Syria", "Congo"), 
                               ordered=TRUE))

# example.stability$icrg.pol.grade.internal
# c(0, 49.99, 59.99, 69.99, 79.99, Inf)
plot.icrg.examples <- ggplot(example.stability, 
                             aes(x=year.actual, 
                                 y=icrg.pol.risk.internal.scaled,
                                 colour=country.plot)) + 
  geom_line(size=1.5) + 
  labs(x=NULL, y="Internal political risk (ICRG)") + 
  scale_colour_manual(values=c("#014358", "#FD7401", "#BEDB3A"), name=NULL) +
  theme_ath()
plot.icrg.examples

fig.save.cairo(plot.icrg.examples, filename="1-icrg-examples", 
               width=5, height=3)
