library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

coef.names <- data_frame(term = c("icrg.pol.risk.internal.scaled", 
                                  "yrsoffc", 
                                  "years.since.comp",
                                  "opp1vote", 
                                  "physint",
                                  "gdpcap.log",
                                  "population.log",
                                  "oda.log",
                                  "countngo",
                                  "globalization"),
                         clean.name = c("Internal political risk (ICRG)",
                                        "Years executive in office", 
                                        "Years since competitive election",
                                        "Opposition vote share",
                                        "Physical integrity rights", 
                                        "GDP per capita (log)", 
                                        "Population (log)", 
                                        "Foreign aid (log)", 
                                        "Number of INGO members", 
                                        "Globalization"))

theme_ath <- function(base_size=9, base_family="Source Sans Pro Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), 
          panel.margin = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size=0.25, colour="grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size=rel(0.8)),
          legend.key.size=unit(.7, "line"),
          legend.key = element_blank(),
          legend.margin = unit(0.1, "lines"),
          strip.text = element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background = element_rect(fill="#ffffff", colour=NA))
  ret
}

fig.save.cairo <- function(fig, filepath=file.path(PROJHOME, "Output", "figures"), 
                           filename, width, height, units="in", ...) {
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".pdf")),
         width=width, height=height, units=units, device=cairo_pdf, ...)
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".png")),
         width=width, height=height, units=units, type="cairo", dpi=300, ...)
}

fig.coef <- function(model) {
  # Convert model to a tidy dataframe for plotting
  plot.data <- model %>%
    map_df(tidy, .id="model.name") %>%
    filter(term != "(Intercept)",
           !str_detect(term, "as\\.factor")) %>%
    mutate(ymin = estimate + (qnorm(0.025) * std.error),
           ymax = estimate + (qnorm(0.975) * std.error)) %>%
    left_join(coef.names, by="term") %>%
    mutate(clean.name = factor(clean.name, levels=rev(unique(clean.name)),
                               ordered=TRUE),
           sub.model = factor(model.name, levels=c("Democracy", "Autocracy"),
                              labels=c("Democracies", "Autocracies    "),
                              ordered=TRUE))
  
  coef.plot <- ggplot(plot.data, aes(x=clean.name, y=estimate, colour=sub.model)) + 
    geom_hline(yintercept=0, colour="#8C2318", alpha=0.6, size=1) + 
    geom_pointrange(aes(ymin=ymin, ymax=ymax), size=.5, 
                    position=position_dodge(width=.5)) + 
    scale_colour_manual(values=c("#BEDB3A", "#441152"), name="",
                        guide=guide_legend(reverse=TRUE)) +
    labs(x=NULL, y="Coefficient") + 
    coord_flip() + 
    theme_ath()
  coef.plot
}
