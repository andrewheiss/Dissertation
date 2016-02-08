library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(Cairo)

ngos <- read_csv(file.path(PROJHOME, "Data", "data_base", 
                           "russian_ngo_reregistration.csv")) %>%
  filter(!is.na(`Before 1999 re-registration deadline`)) %>%
  select(1:3) %>%
  gather(period, ngos, -`NGO type`, factor_key=TRUE) %>%
  mutate(`NGO type` = gsub("national public", "national\npublic", `NGO type`),
         `NGO type` = gsub(" in", "\nin", `NGO type`)) %>%
  mutate(label.before = ifelse(period == "Before 1999 re-registration deadline", 
                               paste(format(ngos, big.mark=","), 
                                     `NGO type`, sep="\n"), NA),
         label.after = ifelse(period != "Before 1999 re-registration deadline", 
                              format(ngos, big.mark=","), NA),
         period = factor(period, labels=gsub("1999 ", "1999\n", levels(period))))

p <- ggplot(ngos, aes(x=period, y=ngos, group=`NGO type`)) + 
  geom_line(size=0.75) + 
  geom_text(aes(label=label.before), hjust="right", nudge_x=-0.05, nudge_y=-150,
            family="Source Sans Pro Light", size=3, lineheight=0.9) + 
  geom_text(aes(label=label.after), hjust="left", nudge_x=0.05,
            family="Source Sans Pro Light", size=3) + 
  labs(x=NULL, y=NULL) + 
  coord_cartesian(xlim=c(0.75, 1.75)) + 
  theme_light(10, base_family="Source Sans Pro Semibold") + 
  theme(panel.background=element_blank(), panel.grid=element_blank(),
        axis.ticks=element_blank(), axis.text.y=element_blank(),
        panel.border=element_blank())

ggsave(p, filename=file.path(PROJHOME, "Output", "figures",
                             "russia_reregistration_before_after.pdf"), 
       width=4, height=2.5, units="in", device=cairo_pdf)
ggsave(p, filename=file.path(PROJHOME, "Output", "figures",
                             "russia_reregistration_before_after.png"), 
       width=4, height=2.5, units="in", type="cairo", dpi=300)
