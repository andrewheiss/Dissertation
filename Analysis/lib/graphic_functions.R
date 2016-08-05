library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(stringr)
library(readr)
library(ggplot2)
library(ggstance)

coef.names <- read_csv(file.path(PROJHOME, "Analysis", 
                                 "ngo_regs_regime_stability", 
                                 "coef_names.csv"))

col.auth <- "#441152"
col.dem <- "#BEDB3A"

# Borrowed from wesanderson
# https://github.com/karthik/wesanderson/blob/master/R/colors.R
# Plain colors from http://clrs.cc/
ath.palettes <- list(
  wars = c("#00A1B0", "#6B4A3D", "#CC3340", 
           "#EB6642", "#EDC952", "#B00DC9"),
  leaders = c("#EB2E4A", "#78C4D4", "#333845",
              "#D9EDE3", "#F5F791", "#ABABAB"),
  palette1 = c("#FF4136",  # red
               "#0074D9",  # blue
               "#2ECC40",  # green
               "#B10DC9",  # purple
               "#FF851B",  # orange
               "#FFDC00"),  # yellow
  contention = c("#3D9970",  # olive
                 "#FF851B")  # orange
)

ath.palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- ath.palettes[[name]]
  if (is.null(pal)) {
    stop("Palette not found.")
  }
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}


theme_ath <- function(base_size=9, base_family="Source Sans Pro Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(size=rel(1.1), vjust=1.2, family="Source Sans Pro Semibold"),
          plot.subtitle=element_text(size=rel(0.8), family="Source Sans Pro Light"),
          plot.caption=element_text(margin=margin(t=10), size=rel(0.6),
                                    family="Source Sans Pro Light"),
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

# For maps
theme_ath_map <- function(base_size=9, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(size=rel(1.1), vjust=1.2, family="Source Sans Pro Semibold"),
          plot.subtitle=element_text(size=rel(0.8), family="Source Sans Pro Light"),
          plot.caption=element_text(margin=margin(t=10), size=rel(0.6),
                                    family="Source Sans Pro Light"),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank(),
          legend.text=element_text(size=rel(0.7), family="Source Sans Pro Light"),
          legend.title=element_text(size=rel(0.7), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"))
  ret
}

fig.save.cairo <- function(fig, filepath=file.path(PROJHOME, "Output", "figures"), 
                           filename, width, height, units="in", ...) {
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".pdf")),
         width=width, height=height, units=units, device=cairo_pdf, ...)
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".png")),
         width=width, height=height, units=units, type="cairo", dpi=300, ...)
}

fig.coef <- function(models, title=NULL, xlab=NULL, legend=TRUE, 
                     space.below=FALSE, var.order=NULL, vars.included="all") {
  
  # Determine which variables to plot
  if (all(vars.included == "all")) {
    vars.search <- ".*"
  } else {
    vars.search <- paste0(vars.included, collapse="|")
  }
  
  # Convert model to a tidy dataframe for plotting
  plot.data <- models %>%
    map_df(tidy, .id="model.name") %>%
    filter(term != "(Intercept)",
           !str_detect(term, "\\.factor")) %>%
    filter(str_detect(term, vars.search)) %>%
    mutate(xmin = estimate + (qnorm(0.025) * std.error),
           xmax = estimate + (qnorm(0.975) * std.error)) %>%
    left_join(coef.names, by="term") %>%
    mutate(model.name = factor(model.name, levels=rev(unique(model.name)),
                               ordered=TRUE))
  
  if (!is.null(var.order)) {
    plot.data <- plot.data %>%
      mutate(term = factor(term, levels=var.order, ordered=TRUE)) %>%
      arrange(term) %>%
      mutate(clean.name = factor(clean.name, levels=rev(unique(clean.name)),
                                 ordered=TRUE))
  } else {
    plot.data <- plot.data %>%
      mutate(clean.name = factor(clean.name, levels=rev(unique(clean.name)),
                                 ordered=TRUE))
  }
  
  coef.plot <- ggplot(plot.data, aes(y=clean.name, x=estimate, colour=model.name)) + 
    geom_vline(xintercept=0, colour="#8C2318", alpha=0.6, size=1) + 
    geom_pointrangeh(aes(xmin=xmin, xmax=xmax), size=.5, 
                     position=position_dodge(width=.7)) + 
    labs(y=NULL, x=xlab, title=title) + 
    theme_ath()
  
  if (legend) {
    coef.plot <- coef.plot + 
      scale_colour_manual(values=c("#004259", "#FC7300", "#BFDB3B"), name="",
                          guide=guide_legend(reverse=TRUE))
  } else {
    coef.plot <- coef.plot + 
      scale_colour_manual(values=c("#004259", "#FC7300", "#BFDB3B"), guide=FALSE)
  }
  
  if (space.below) {
    new.margins <- theme_ath()$plot.margin
    new.margins[3] <- 25
    
    coef.plot <- coef.plot + theme(plot.margin = new.margins)
  }
  coef.plot
}
