library(dplyr)
library(tidyr)
library(foreign)
library(ggplot2)
library(scales)
library(grid)
library(Cairo)

theme_clean <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          legend.position="bottom", 
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"))
  
  ret
}

institutions <- read.dta("twq15byyeardta.dta")
autocrats <- read.dta("~/Research/•Sandbox/Kendall-TaylorFrantz2014/GWFtscs.dta")

murdie <- read.dta("~/Research/•Sandbox/Kendall-TaylorFrantz2014/11558_2013_9180_MOESM1_ESM.dta") %>%
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
  geom_hline(yintercept=seq(0.6, 0.9, by=0.1), size=0.25, colour="grey90") + 
  geom_line(size=1) + 
  labs(x=NULL, y="Percentage of dictatorships") + 
  scale_y_continuous(labels=percent) + 
  scale_colour_manual(name="", values=c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00")) + 
  guides(colour = guide_legend(nrow=2, byrow=TRUE)) + 
  theme_clean() + theme(legend.key = element_blank())
p

ggsave(p, filename="ingos_dictatorships.pdf", 
       width=6.5, height=3.75, units="in", device=cairo_pdf)
