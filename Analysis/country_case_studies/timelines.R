#' ---
#' title: "Case study timelines"
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
#' ---

#+ load_data_libraries, message=FALSE
knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For output

# Load libraries
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

# General settings
my.seed <- 1234
set.seed(my.seed)

# Countries to plot
cases <- c("EGY", "JOR", "CHN", "MMR", "RUS", "KAZ")


# -----------
# Load data
# -----------
icrg.monthly <- read_feather(file.path(PROJHOME, "Data", "data_processed",
                                       "icrg_monthly.feather")) %>%
  filter(Country != "USSR")

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
                                    "cases_presidents.csv"),
                          col_types=cols(
                            ISO3 = col_character(),
                            Date_start = col_date(format = ""),
                            Date_end = col_date(format = ""),
                            president = col_character()
                          )) %>%
  mutate(Date_start = ymd(Date_start), Date_end = ymd(Date_end)) %>%
  arrange(ISO3, Date_start)

laws.cases <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                                 "cases_ngo_laws.csv"),
                       col_types=cols(
                         Date = col_date(format = ""),
                         ISO3 = col_character(),
                         law = col_character(),
                         note = col_character(),
                         include_plot = col_integer(),
                         plot_y = col_double(),
                         plot_h = col_character()
                       )) %>%
  filter(include_plot == 1) %>%
  mutate(Date = ymd(Date),
         law = gsub("XXX", "\n", law)) %>%
  arrange(ISO3, Date)

wars.cases <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                                 "cases_wars.csv"),
                       col_types=cols(
                         ISO3 = col_character(),
                         Date_start = col_date(format = ""),
                         Date_end = col_date(format = ""),
                         war = col_character(),
                         note = col_character()
                       )) %>%
  mutate(Date_start = ymd(Date_start), Date_end = ymd(Date_end)) %>%
  arrange(ISO3, Date_start)

plot.timeline <- function(ISO, start.date = "1995-01-01", end.date = "2016-12-31") {
  # Filtered data
  df.country <- full.data %>%
    filter(iso3 == ISO)
  
  df.icrg <- icrg.monthly %>%
    filter(iso == ISO)
  
  df.protests <- df.country %>%
    select(year.actual, protests.violent.std_wt, protests.nonviolent.std_wt) %>%
    gather(protest.type, value, -year.actual) %>%
    filter(!is.nan(value)) %>%
    mutate(protest.type = factor(protest.type, 
                                 levels=c("protests.violent.std_wt", 
                                          "protests.nonviolent.std_wt"),
                                 labels=c("Violent", "Nonviolent")))
  
  df.coups <- df.country %>%
    select(year.actual, coups.activity.bin_sum_nb) %>%
    filter(!is.na(year.actual)) %>%
    mutate(coup.activity = case_when(
      .$coups.activity.bin_sum_nb == 0 ~ NA_real_,
      .$coups.activity.bin_sum_nb == 1 ~ 45,
      .$coups.activity.bin_sum_nb == 2 ~ 45
    )) %>%
    mutate(point.size = case_when(
      .$coups.activity.bin_sum_nb == 1 ~ 1,
      .$coups.activity.bin_sum_nb == 2 ~ 2.25
    ))
  
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
    annotate("label", x=ymd("1995-01-01"), y=Inf, 
             hjust=0, vjust="top", size=2.5, alpha=0.8,
             colour="black", fill="grey70",
             family="Source Sans Pro Semibold",
             label="Civil Society Regulatory Environment") +
    labs(x=NULL, y=NULL) + 
    scale_y_continuous(breaks=c(-2.5, 0, 2.5)) +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(-4, 4)) +
    theme_ath()
  
  plot.icrg.govt.stab <- ggplot(df.icrg, 
                                aes(x=ymd(Date), y=icrg.stability)) +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    annotate("label", x=ymd("1995-01-01"), y=Inf, 
             hjust=0, vjust="top", size=2.5, alpha=0.8,
             colour="black", fill="grey70",
             family="Source Sans Pro Semibold",
             label="Government stability") +
    labs(x=NULL, y=NULL) + 
    scale_y_continuous(breaks=c(0, 3, 6, 9, 12)) +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(0, 12)) +
    theme_ath()

  plot.icrg.internal.stab <- ggplot(df.icrg, 
                                    aes(x=ymd(Date),
                                        y=icrg.pol.risk.internal.nostab.scaled)) +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    annotate("label", x=ymd("1995-01-01"), y=Inf, 
             hjust=0, vjust="top", size=2.5, alpha=0.8,
             colour="black", fill="grey70",
             family="Source Sans Pro Semibold",
             label="Internal political stability") +
    labs(x=NULL, y=NULL) + 
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(20, 70)) +
    theme_ath()
  
  plot.icrg.external <- ggplot(df.country, aes(x=ymd(year.actual), 
                                                y=icrg.pol.risk_wt)) +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    geom_point(data=df.coups, aes(y=coup.activity, size=point.size)) +
    annotate("label", x=ymd("1995-01-01"), y=Inf, 
             hjust=0, vjust="top", size=2.5, alpha=0.8,
             colour="black", fill="grey70",
             family="Source Sans Pro Semibold",
             label="Political risk in neigbhors ( • = coup activity)") +
    labs(x=NULL, y=NULL) +
    scale_size_identity(guide="none") +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(40, 90)) +
    theme_ath()

  plot.protests <- ggplot(df.protests, aes(x=year.actual, y=value, linetype=protest.type)) +
    geom_hline(yintercept=3, colour="grey50") +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    annotate("label", x=ymd("1995-01-01"), y=Inf, 
             hjust=0, vjust="top", size=2.5, alpha=0.8,
             colour="black", fill="grey70",
             family="Source Sans Pro Semibold",
             label="Protest activity in neighbors (dotted = nonviolent)") +
    labs(x=NULL, y=NULL) + 
    scale_linetype_manual(values=c("solid", "21"), guide=FALSE) +
    scale_y_continuous(labels=c("Less", "Normal", "More"),
                       breaks=c(1, 3, 5)) +
    coord_cartesian(xlim=ymd(c(start.date, end.date)),
                    ylim=c(1, 5)) +
    theme_ath()
  
  plot.shaming <- ggplot(df.country, aes(x=year.actual, y=shaming.states.std)) +
    geom_hline(yintercept=3, colour="grey50") +
    geom_line(size=1) + 
    geom_vline(data=df.laws, aes(xintercept=as.numeric(Date)),
               size=0.5, colour="grey50", linetype="dotted") +
    annotate("label", x=ymd("1995-01-01"), y=Inf, 
             hjust=0, vjust="top", size=2.5, alpha=0.8,
             colour="black", fill="grey70",
             family="Source Sans Pro Semibold",
             label="Relative shaming by states") +
    labs(x=NULL, y=NULL) + 
    scale_y_continuous(labels=c("Less", "Normal", "More"),
                       breaks=c(1, 3, 5)) +
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
    scale_colour_manual(values=ath.palette("leaders"), name=NULL) +
    scale_fill_manual(values=ath.palette("wars"), name=NULL) + 
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
                         ggplotGrob(plot.icrg.govt.stab),
                         ggplotGrob(plot.icrg.internal.stab),
                         ggplotGrob(plot.icrg.external),
                         ggplotGrob(plot.protests),
                         ggplotGrob(plot.shaming),
                         ggplotGrob(plot.misc))
  
  # Adjust panel sizes
  # via http://stackoverflow.com/a/24333504/120898
  panels <- plot.timeline$layout$t[grep("panel", plot.timeline$layout$name)]
  plot.timeline$heights[panels] <- unit(c(0.9, 1, 1, 1, 1, 1, 1, 0.15), "null")
  
  # All done!
  return(plot.timeline)
}

# plot.country <- function(country) {
#   p.timeline <- plot.timeline(country)
#   
#   fig.save.cairo(p.timeline, filename=sprintf("timeline-%s", tolower(country)),
#                  width=6, height=7.5)
#   
#   return(TRUE)
# }
# 
# suppressWarnings(sapply(cases, FUN=plot.country))

#' ## Egypt
#+ warning=FALSE, fig.width=6, fig.height=7.5
country <- "EGY"
grid::grid.draw(plot.timeline(country))
fig.save.cairo(plot.timeline(country), 
               filename=sprintf("2-timeline-%s", tolower(country)),
               width=6, height=7.5)

#' ## Jordan
#+ warning=FALSE, fig.width=6, fig.height=7.5
country <- "JOR"
grid::grid.draw(plot.timeline(country))
fig.save.cairo(plot.timeline(country), 
               filename=sprintf("2-timeline-%s", tolower(country)),
               width=6, height=7.5)

#' ## China
#+ warning=FALSE, fig.width=6, fig.height=7.5
country <- "CHN"
grid::grid.draw(plot.timeline(country))
fig.save.cairo(plot.timeline(country), 
               filename=sprintf("2-timeline-%s", tolower(country)),
               width=6, height=7.5)

#' ## Myanmar
#+ warning=FALSE, fig.width=6, fig.height=7.5
country <- "MMR"
grid::grid.draw(plot.timeline(country))
fig.save.cairo(plot.timeline(country), 
               filename=sprintf("2-timeline-%s", tolower(country)),
               width=6, height=7.5)

#' ## Russia
#+ warning=FALSE, fig.width=6, fig.height=7.5
country <- "RUS"
grid::grid.draw(plot.timeline(country))
fig.save.cairo(plot.timeline(country), 
               filename=sprintf("2-timeline-%s", tolower(country)),
               width=6, height=7.5)

#' ## Kazakhstan
#+ warning=FALSE, fig.width=6, fig.height=7.5
country <- "KAZ"
grid::grid.draw(plot.timeline(country))
fig.save.cairo(plot.timeline(country), 
               filename=sprintf("2-timeline-%s", tolower(country)),
               width=6, height=7.5)
