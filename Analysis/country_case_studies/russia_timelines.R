library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(gtable)
library(gridExtra)
library(scales)
library(Cairo)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))


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
  geom_vline(data=laws, aes(xintercept=as.numeric(Date)),
             size=0.5, colour="grey50", linetype="dotted") +
  labs(x=NULL, y="CSRE") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  theme_ath()

plot.icrg.internal <- ggplot(icrg.russia, 
                             aes(x=Date, y=icrg.pol.risk.internal.scaled)) +
  geom_line(size=1) + 
  geom_vline(data=laws, aes(xintercept=as.numeric(Date)),
             size=0.5, colour="grey50", linetype="dotted") +
  labs(x=NULL, y="Internal risk") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  theme_ath()

plot.icrg.external <- ggplot(russia.data, aes(x=year.actual, 
                                              y=icrg.pol.risk.subregional.loo)) +
  geom_line(size=1) + 
  geom_vline(data=laws, aes(xintercept=as.numeric(Date)),
             size=0.5, colour="grey50", linetype="dotted") +
  labs(x=NULL, y="Subregional risk") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  theme_ath()

plot.shaming <- ggplot(russia.data, aes(x=year.actual, y=icews.pct.shame)) +
  geom_line(size=1) + 
  geom_vline(data=laws, aes(xintercept=as.numeric(Date)),
             size=0.5, colour="grey50", linetype="dotted") +
  labs(x=NULL, y="% shaming") + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  scale_y_continuous(labels=percent) +
  theme_ath()

plot.misc <- ggplot() +
  geom_segment(data=presidents, aes(x=Date_start, xend=Date_end, 
                                    y=0.5, yend=0.5, colour=president), size=4) +
  geom_rect(data=wars, aes(x=NULL, y=NULL, xmin=Date_start, xmax=Date_end, 
                           ymin=0, ymax=0.2, fill=war)) + 
  geom_vline(data=laws, aes(xintercept=as.numeric(Date)),
             size=0.5, colour="grey50", linetype="dotted") +
  scale_colour_manual(values=c("#EB6742", "#CC333F", "#00A0B0"), name=NULL) + 
  scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", 
                             "#984EA3", "#FF7F00", "#FFFF33"), name=NULL) + 
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31"))) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2)) +
  labs(x=NULL, y=NULL) +
  theme_ath() +
  theme(legend.position="bottom", 
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        legend.key.size=unit(0.65, "lines"),
        legend.key=element_blank(), legend.margin=unit(0.25, "lines"),
        axis.text=element_blank())

plot.laws <- ggplot(laws, aes(x=Date, y=0)) + 
  geom_segment(aes(x=Date, xend=Date, y=0, yend=plot_y), 
               size=0.5, colour="grey30") +
  geom_label(data=laws,
            aes(x=Date, y=plot_y, label=law, hjust=plot_h),
            family="Source Sans Pro Semibold", size=2.5,
            fill="grey30", colour="white", label.padding=unit(0.3, "lines")) +
  geom_point(size=3, colour="grey30") +
  coord_cartesian(xlim=ymd(c("1995-01-01", "2015-12-31")),
                  ylim=c(-1, 3)) +
  labs(x=NULL, y=NULL) +
  theme_ath() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_blank())
plot.laws

# Combine all the plots
plot.timeline <- rbind(ggplotGrob(plot.laws),
                       ggplotGrob(plot.csre),
                       ggplotGrob(plot.icrg.internal),
                       ggplotGrob(plot.icrg.external),
                       ggplotGrob(plot.shaming),
                       ggplotGrob(plot.misc))

# Adjust panel sizes
# via http://stackoverflow.com/a/24333504/120898
panels <- plot.timeline$layout$t[grep("panel", plot.timeline$layout$name)]
plot.timeline$heights[panels] <- lapply(c(0.9, 1, 1, 1, 1, 0.15), unit, "null")

grid::grid.draw(plot.timeline)

fig.save.cairo(plot.timeline, filename="1-russia-timeline", 
               width=6, height=7.5)
