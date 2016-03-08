library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(scales)
library(Cairo)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

my.seed <- 1234
set.seed(my.seed)


# Load data
icrg.monthly <- readRDS(file.path(PROJHOME, "Data",
                                  "data_processed", "icrg_monthly.rds"))
full.data <- readRDS(file.path(PROJHOME, "Data",
                               "data_processed", "full_data.rds"))

icrg.russia <- icrg.monthly %>%
  filter(cowcode == 365, !is.na(icrg.pol.risk.internal.scaled))

russia.data <- full.data %>%
  filter(cowcode == 365)

laws <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                           "russia_laws.csv")) %>%
  filter(include_plot == 1) %>%
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


#' # Plot timelines
plot.csre <- ggplot(russia.data, aes(x=year.actual, y=cs_env_sum)) + 
  geom_line(size=1) + 
  labs(x=NULL, y="CSRE") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  theme_ath()

plot.icrg.internal <- ggplot(icrg.russia, 
                             aes(x=Date, y=icrg.pol.risk.internal.scaled)) +
  geom_line(size=1) + 
  labs(x=NULL, y="Internal political risk") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  theme_ath()

plot.icrg.external <- ggplot(russia.data, aes(x=year.actual, 
                                              y=icrg.pol.risk.subregional.loo)) +
  geom_line(size=1) + 
  labs(x=NULL, y="Mean subregional political risk") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  theme_ath()

plot.shaming <- ggplot(russia.data, aes(x=year.actual, y=icews.pct.shame)) +
  geom_line(size=1) + 
  labs(x=NULL, y="Mean subregional political risk") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  scale_y_continuous(labels=percent) +
  theme_ath()

plot.misc <- ggplot() +
  geom_segment(data=presidents, aes(x=Date_start, xend=Date_end, 
                                    y=0.5, yend=0.5, colour=president), size=5) +
  geom_rect(data=wars, aes(x=NULL, y=NULL, xmin=Date_start, xmax=Date_end, 
                           ymin=0, ymax=0.3, fill=war)) + 
  scale_colour_manual(values=c("#EB6742", "#CC333F", "#00A0B0"), name=NULL) + 
  scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", 
                             "#984EA3", "#FF7F00", "#FFFF33"), name=NULL) + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2)) +
  labs(x=NULL, y="Presidents and wars") +
  theme_ath() +
  theme(legend.position="bottom", 
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        legend.key.size=unit(0.65, "lines"),
        legend.key=element_blank(), legend.margin=unit(0.25, "lines"),
        axis.text.y=element_blank())

plot.laws <- ggplot(laws, aes(x=Date, y=0)) + 
  geom_point(size=3) +
  geom_text_repel(data=laws, aes(x=Date, y=0, label=law), 
            size=3, point.padding=unit(0.5, "lines"),
            box.padding=unit(0.75, "lines"),
            family="Source Sans Pro Semibold") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  labs(x=NULL, y=NULL) +
  theme_ath() + 
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank())

plot.timeline <- arrangeGrob(rbind(ggplotGrob(plot.laws),
                                   ggplotGrob(plot.csre),
                                   ggplotGrob(plot.icrg.internal),
                                   ggplotGrob(plot.icrg.external),
                                   ggplotGrob(plot.shaming),
                                   ggplotGrob(plot.misc)))
grid::grid.draw(plot.timeline)

fig.save.cairo(plot.timeline, filename="1-russia-timeline", 
               width=6, height=7)
