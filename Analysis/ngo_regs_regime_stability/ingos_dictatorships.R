library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(scales)
library(grid)
library(Cairo)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

institutions <- read_dta(file.path(PROJHOME, "Data", "data_raw", 
                                   "External", "twq15byyeardta.dta"))

autocrats <- read_dta(file.path(PROJHOME, "Data", "data_raw", 
                                "External", "GWFtscs.dta"))

murdie <- read_dta(file.path(PROJHOME, "Data", "data_raw", 
                             "External", "Murdie 2014",
                             "11558_2013_9180_MOESM1_ESM.dta")) %>%
  select(year, cowcode, ingos = ingos_1num) %>%
  filter(cowcode %in% autocrats$cowcode) %>%
  mutate(ingos_present = ifelse(ingos > 50, TRUE, FALSE)) %>%
  group_by(year) %>%
  summarize(ingos = sum(ingos_present, na.rm=TRUE) / n()) %>%
  mutate(ingos = ifelse(ingos == 0, NA, ingos))

inst.labels <- c("More than 50 INGOs with members", "At least one party",
                 "Multiple parties and legislature", "Election in last six years")
inst.order <- c("At least one party", "Multiple parties and legislature",
                "Election in last six years", "More than 50 INGOs with members")

plot.data <- institutions %>%
  left_join(murdie, by="year") %>%
  filter(year < 2009 & year > 1950) %>%
  select(year, ingos, percentage_w_party, 
         percentage_partisan_leg, percentage_election_last6) %>%
  gather(variable, value, -year) %>%
  na.omit() %>%
  mutate(variable = factor(variable, labels=inst.labels),
         variable = factor(variable, levels=inst.order, ordered=TRUE))
  
p <- ggplot(plot.data, aes(x=year, y=value, colour=variable)) + 
  geom_line(size=1) + 
  labs(x=NULL, y="Percentage of dictatorships") + 
  scale_y_continuous(labels=percent) + 
  scale_colour_manual(name="", values=c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00")) + 
  guides(colour = guide_legend(nrow=2, byrow=TRUE)) + 
  theme_ath() + theme(legend.key = element_blank())

fig.save.cairo(p, filename="1-ingos-dictatorships", 
               width=5, height=2.5)
