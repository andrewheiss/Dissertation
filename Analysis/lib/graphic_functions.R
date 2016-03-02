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
