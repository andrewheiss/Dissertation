library(dplyr)
library(tidyr)
library(readr)
library(countrycode)
library(feather)
library(lubridate)
library(ggplot2)
library(gtable)
library(gridExtra)
library(scales)
library(Cairo)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))

# Countries to plot
cases <- c("EGY", "CHN", "RUS")


# Borrowed from wesanderson
# https://github.com/karthik/wesanderson/blob/master/R/colors.R
timeline.palettes <- list(
  wars = c("#00A1B0", "#6B4A3D", "#CC3340", 
           "#EB6642", "#EDC952", "#B00DC9"),
  leaders = c("#EB2E4A", "#78C4D4", "#333845",
              "#D9EDE3", "#F5F791", "#ABABAB")
)

timeline.palette <- function(name, n) {
  pal <- timeline.palettes[[name]]
  
  if (is.null(pal)) {
    stop("Palette not found.")
  }
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  out <- pal[1:n]
  
  structure(out, class="palette", name=name)
}


# -----------
# Load data
# -----------
icrg.monthly <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                                       "icrg_monthly.feather"))
full.data <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                                    "full_data.feather"))

dcjw <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                               "dcjw.feather"))

# Archigos database of political leaders
# http://privatewww.essex.ac.uk/~ksg/archigos.html
# leaders <- read_tsv("http://privatewww.essex.ac.uk/~ksg/data/1March_Archigos_4.1.txt")
# leaders.cases.orig <- leaders %>%
#   filter(ccode %in% countrycode(cases, "iso3c", "cown")) %>%
#   filter(enddate > ymd("1991-01-01")) %>%
#   mutate(middate = startdate - floor((startdate - enddate) / 2),
#          time.in.office = enddate - startdate,
#          leader = iconv(leader, from="Windows-1252", to="UTF-8"))  # Convert to UTF-8

# Export to CSV for hand refining (and reinsertion of Medvedev since Archigos
# says Putin never relinquished power, which is trueish)
# write_csv(leaders.cases.orig, file.path(PROJHOME, "Data", "data_base",
#                                         "leaders_archigos.csv"))

leaders.cases <- read_csv(file.path(PROJHOME, "Data", "data_base",
                                    "cases_presidents.csv")) %>%
  mutate(Date_start = ymd(Date_start), Date_end = ymd(Date_end)) %>%
  arrange(ISO3, Date_start)

laws.cases <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                                 "cases_ngo_laws.csv")) %>%
  filter(include_plot == 1) %>%
  mutate(Date = ymd(Date),
         law = gsub("XXX", "\n", law)) %>%
  arrange(ISO3, Date)

wars.cases <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                                 "cases_wars.csv")) %>%
  mutate(Date_start = ymd(Date_start), Date_end = ymd(Date_end)) %>%
  arrange(ISO3, Date_start)

plot.timeline <- function(ISO, start.date = "1995-01-01", end.date = "2016-12-31") {
  # Filtered data
  df.country <- full.data %>%
    filter(iso == ISO)
  
  df.icrg <- icrg.monthly %>%
    filter(iso == ISO, !is.na(icrg.internal))
  
  df.leaders <- leaders.cases %>%
    filter(ISO3 == ISO) %>%
    mutate(president = factor(president, levels=unique(president), ordered=TRUE))
  
  df.laws <- laws.cases %>%
    filter(ISO3 == ISO)
  
  df.wars <- wars.cases %>%
    filter(ISO3 == ISO) %>%
    mutate(war = factor(war, levels=war, ordered=TRUE))
  
  # Individual plots
  plot.csre <- ggplot(df.country, aes(x=year.actual, y=cs_env_sum)) + 
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    labs(x=NULL, y="CSRE") + 
    scale_y_continuous(breaks=c(-2.5, 0, 2.5)) +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(-4, 4)) +
    theme_ath()
  
  plot.icrg.internal <- ggplot(df.icrg, 
                               aes(x=ymd(Date), y=icrg.internal)) +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    labs(x=NULL, y="Internal risk") + 
    scale_y_continuous(breaks=c(0, 3, 6, 9, 12)) +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(0, 12)) +
    theme_ath()
  
  plot.icrg.external <- ggplot(df.country, aes(x=ymd(year.actual), 
                                                y=icrg.pol.risk_wt)) +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    labs(x=NULL, y="Weighted neighbor political risk") +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(45, 90)) +
    theme_ath()
  
  plot.shaming <- ggplot(df.country, aes(x=year.actual, y=shaming.states.std)) +
    geom_hline(yintercept=3, colour="grey50") +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    labs(x=NULL, y="Relative shaming by states") + 
    scale_y_continuous(labels=c("1\n(less)", 2, "3\n(normal)", 4, "5\n(more)")) +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(1, 5)) +
    theme_ath()
  
  plot.misc <- ggplot() +
    geom_segment(data=df.leaders, aes(x=Date_start, xend=Date_end, 
                                      y=0.5, yend=0.5, colour=president), size=4) +
    geom_rect(data=df.wars, aes(x=NULL, y=NULL, xmin=Date_start, xmax=Date_end, 
                             ymin=0, ymax=0.2, fill=war)) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    scale_colour_manual(values=timeline.palette("leaders"), name=NULL) +
    scale_fill_manual(values=timeline.palette("wars"), name=NULL) + 
    coord_cartesian(xlim=ymd(c(start.date, end.date))) +
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
  
  plot.laws <- ggplot(df.laws, aes(x=Date, y=0)) + 
    geom_segment(aes(x=ymd(Date), xend=ymd(Date), y=0, yend=plot_y), 
                 size=0.5, colour="grey30") +
    geom_label(data=df.laws,
               aes(x=Date, y=plot_y, label=law, hjust=plot_h),
               family="Source Sans Pro Semibold", size=2.5,
               fill="grey30", colour="white", label.padding=unit(0.3, "lines")) +
    geom_point(size=3, colour="grey30") +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(-1, 3)) +
    labs(x=NULL, y=NULL) +
    theme_ath() + 
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text=element_blank())
  
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
  plot.timeline$heights[panels] <- unit(c(0.9, 1, 1, 1, 1, 0.15), "null")
  
  # All done!
  return(plot.timeline)
}

plot.country <- function(country) {
  p.timeline <- plot.timeline(country)
  
  # grid::grid.draw(p.timeline)
  fig.save.cairo(p.timeline, filename=sprintf("timeline-%s", tolower(country)),
                 width=6, height=7.5)
  
  return(TRUE)
}

suppressWarnings(sapply(cases, FUN=plot.country))
