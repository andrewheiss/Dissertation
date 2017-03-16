library(dplyr)
library(ggplot2)
library(gridExtra)
library(Cairo)

# Load data
full.data <- readRDS(file.path(PROJHOME, "Data","data_processed",
                               "full_data.rds"))

# Plot specific cases
plot.data <- full.data %>%
  select(country_name, year.num, v2csreprss, v2csreprss_ord, 
         v2cseeorgs, v2cseeorgs_ord, cs_env_sum) %>%
  filter(country_name %in% c("Egypt", "China", "Russia"))

fig.points <- ggplot(plot.data, aes(x=year.num, y=cs_env_sum, 
                                    colour=country_name)) + 
  geom_line(size=1.5) + 
  coord_cartesian(xlim=c(1995, 2015)) + 
  labs(x=NULL, y="Civil society legal environment index",
       title="Civil society legal environment",
       subtitle="Index of measurement model point estimates") +
  scale_color_manual(values=c("#EA2E49", "#441152", "#29968B"), name=NULL) + 
  theme_light(7, base_family="Source Sans Pro") + 
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        legend.key=element_blank(), legend.margin=unit(0.25, "lines"),
        panel.grid.minor.y=element_blank())

fig.ord <- ggplot(plot.data, aes(x=year.num, y=v2csreprss_ord,
                                 colour=country_name)) + 
  geom_line(aes(group=1), size=1.5) +
  coord_cartesian(xlim=c(1995, 2015)) + 
  labs(x=NULL, y=NULL,
       title="Civil society repression",
       subtitle="Ordinal measure") +
  scale_color_manual(values=c("#EA2E49", "#441152", "#29968B"), 
                     name=NULL, guide=FALSE) + 
  theme_light(7, base_family="Source Sans Pro") + 
  theme(panel.grid.minor.y=element_blank()) +
  facet_wrap(~ country_name)

fig.both <- arrangeGrob(fig.points, fig.ord, nrow=2)
grid::grid.draw(fig.both)

ggsave(fig.both, filename=file.path(PROJHOME, "Output", "figures",
                                    "cases_cs_both.pdf"), 
       width=5, height=5, units="in", device=cairo_pdf)
ggsave(fig.both, filename=file.path(PROJHOME, "Output", "figures",
                                    "cases_cs_both.png"),
       width=5, height=5, units="in", type="cairo", dpi=300)
