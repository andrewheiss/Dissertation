library(broom)
library(stringr)
library(ggstance)
library(ggrepel)
library(gridExtra)
library(viridis)
library(pander)
library(Cairo)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('missing', '')
panderOptions('big.mark', ',')
panderOptions('digits', 2)
panderOptions('round', 2)
panderOptions('table.alignment.default', 'left')

coef.names <- read_csv(file.path(PROJHOME, "Analysis", 
                                 "ngo_regs_regime_stability", 
                                 "coef_names.csv")) %>%
  mutate(term.clean = factor(term.clean, levels=unique(term.clean), ordered=TRUE),
         category = factor(category, levels=unique(category), ordered=TRUE),
         term.clean.rev = factor(term.clean, levels=rev(levels(term.clean))),
         category.rev = factor(category, levels=rev(levels(category))),
         term.short = factor(term.short, levels=unique(term.short), ordered=TRUE))

col.auth <- "#441152"
col.dem <- "#BEDB3A"

# Colors for continents Sankey plot
cont.colors <- tribble(
  ~N1, ~col,
  "Africa", "#FF4036",  # Red
  "Americas", "#2ECC40",  # Green
  "Asia &\nOceania", "#0073D9",  # Blue
  "Europe", "#FFDB00"  # Yellow
) %>%
  mutate(N2 = paste0(N1, " ")) %>%
  gather(key, ID, -col) %>% select(-key)

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
  contention1 = c("#3D9970",  # olive
                 "#FF851B"),  # orange
  contention = c("#53787B",  # turquoise
                 "#DC5B44"),  # orange
  regime1 = c("#FFDC00",  # yellow
             "#85144b"),  # magenta
  regime2 = c("#0074D9",  # blue
             "#FF4136"),  # red
  regime = c("#203864",  # gold
             "#7F6000"),  # word blue 
  org.size = c("#0074D9",  # blue
               "#FF4136"),  # red
  staffing = c("#B10DC9",  # purple
               "#FFDC00"),  # yellow
  single.color = c("#53787B")  # turquoise
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

# Blank plot for spacing things in arrangeGrob()
grob.blank <- grid::rectGrob(gp=grid::gpar(col="white"))

# Blank plot for spacing things in *bind.gtable()
plot.blank <- ggplot() + geom_blank(aes(1, 1)) + theme_void()


theme_ath <- function(base_size=9, base_family="Source Sans Pro Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  update_geom_defaults("label", list(family="Source Sans Pro Light"))
  update_geom_defaults("label_repel", list(family="Source Sans Pro Light"))
  update_geom_defaults("text", list(family="Source Sans Pro Light"))
  update_geom_defaults("text_repel", list(family="Source Sans Pro Light"))

  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(size=rel(1.1), vjust=1.2, family="Source Sans Pro Semibold"),
          plot.subtitle=element_text(size=rel(0.8), family="Source Sans Pro Light"),
          plot.caption=element_text(margin=margin(t=10), size=rel(0.6),
                                    family="Source Sans Pro Light"),
          panel.border = element_blank(), 
          panel.spacing = unit(1, "lines"),
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
          legend.spacing = unit(0.1, "lines"),
          strip.text = element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background = element_rect(fill="#ffffff", colour=NA))
  ret
}

theme_ath_density <- function(...) {
  ret <- theme_ath(...) +
    theme(panel.grid.major=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank())
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

# facet_grid has a space="free*" parameter to close up unused levels in facet panels, but it doesn't have a way to specify the number of columns.
# 
# facet_wrap lets you specify the number of columns, but doesn't have the space parameter
# 
# Dilemma!
# 
# This function modifies the panel sizes in a plot that uses facet_wrap and
# rescales them proportional to the number of variables in each panel. It's a
# combination of suggestions at these SO posts:
#
# - http://stackoverflow.com/a/32583612/120898
# - http://stackoverflow.com/a/20639481/120898
# - http://stackoverflow.com/a/28099801/120898
# - http://stackoverflow.com/q/31572239/120898
#
correct_panel_size <- function(p) {
  # Figure out how many y breaks are in each panel
  # Use ggplot_build object for plot parameters
  p.object <- ggplot_build(p) 
  nvars.in.tiles <- sapply(lapply(p.object$layout$panel_ranges, "[[", "y.major"), length)

    # Manipulate underlying ggplot grob
  p.grob <- ggplotGrob(p)
  
  # Identify the panel elements in the grob
  p.panels <- grep("panel", p.grob$layout$name)
  p.panel_index <- unique(p.grob$layout$t[p.panels])
  
  # Replace the default panel heights (currently 1null) with relative sizes
  # based on number of variables in each tile (1null, 3null, etc.). ggplot/grid
  # will scale the tile height accordingly
  p.grob$heights[p.panel_index] <- grid::unit(nvars.in.tiles, "null")
  
  # All done!
  return(p.grob)
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

# ggplot layer for the convex hull of a set of points
# http://docs.ggplot2.org/dev/vignettes/extending-ggplot2.html#the-simplest-stat
StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Add zero-based positive/negative color bars in DT::datatable
# Adapted from http://stackoverflow.com/a/33524422/120898
styleColorBarCentered <- function(column, color.negative, color.positive) {
  max_val <- max(abs(column))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val, color.negative, max_val, color.negative, 
             color.positive, color.positive, max_val, max_val))
} 
