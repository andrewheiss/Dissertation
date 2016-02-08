library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(Cairo)


# Load data
icrg.monthly <- readRDS(file.path(PROJHOME, "Data",
                                  "data_processed", "icrg_monthly.rds"))

icrg.russia <- icrg.monthly %>%
  filter(cowcode == 365, !is.na(score))

laws <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                           "russia_laws.csv")) %>%
  mutate(Date = ymd(Date)) %>%
  arrange(Date)

wars <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                           "russia_wars.csv")) %>%
  mutate(Date_start = ymd(Date_start), Date_end = ymd(Date_end)) %>%
  arrange(Date_start) %>%
  mutate(war = factor(war, levels=war, ordered=TRUE))

presidents <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                                 "russia_presidents.csv")) %>%
  mutate(Date_start = ymd(Date_start), Date_end = ymd(Date_end)) %>%
  arrange(Date_start) %>%
  mutate(president = factor(president, levels=unique(president), ordered=TRUE))


# Plot timelines
p.icrg.wars <- ggplot() + 
  geom_rect(data=wars, aes(x=NULL, y=NULL, xmin=Date_start, xmax=Date_end, 
                           ymin=0, ymax=12, fill=war), alpha=0.3) + 
  geom_text(data=laws, aes(x=Date, y=0, label=law), 
            angle=90, hjust="left", vjust=-0.5, size=3,
            family="Source Sans Pro Semibold") + 
  geom_segment(data=presidents, aes(x=Date_start, xend=Date_end, 
                                    y=12.5, yend=12.5, colour=president), size=2) + 
  geom_vline(data=laws, aes(xintercept=as.numeric(Date)), colour="grey50") + 
  geom_line(data=icrg.russia, aes(x=Date, y=score), colour="#6A4A3C", size=1) + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31")),
                  ylim=c(0, 12.5)) +
  scale_y_continuous(breaks=seq(0, 12, 2)) + 
  scale_colour_manual(values=c("#EB6742", "#CC333F", "#00A0B0"), name=NULL) + 
  scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", 
                             "#984EA3", "#FF7F00", "#FFFF33"), name=NULL) + 
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2)) +
  labs(x=NULL, y="ICRG government stability") +
  theme_light(base_family="Source Sans Pro Light") + 
  theme(legend.position="bottom", 
        plot.title=element_text(family="Source Sans Pro Semibold"),
        panel.grid.minor=element_blank(),
        legend.key.size=unit(0.65, "lines"),
        legend.key=element_blank(), legend.margin=unit(0.25, "lines"))
p.icrg.wars

ggsave(p.icrg.wars, filename=file.path(PROJHOME, "Output", "figures",
                                       "russia_timeline_icrg_wars.pdf"),
       width=8, height=6, units="in", device=cairo_pdf)
ggsave(p.icrg.wars, filename=file.path(PROJHOME, "Output", "figures",
                                       "russia_timeline_icrg_wars.png"), 
       width=8, height=6, units="in", type="cairo", dpi=300)
