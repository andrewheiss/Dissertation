library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(grid)

theme_gantt <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) %+replace%
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size=0.5, colour="grey80"),
          axis.ticks=element_blank(),
          legend.position="bottom", 
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"),
          legend.key = element_blank())
  
  ret
}
  
tasks <- read_csv("schedule.csv") %>%
  gather(date.type, task.date, -c(Project, Task)) %>%
  arrange(date.type, task.date) %>%
  mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=TRUE))

tufte.grid <- data_frame(breaks=seq(floor_date(min(tasks$task.date), unit="month"),
                                    ceiling_date(max(tasks$task.date), unit="month"),
                                    by="1 month"))
# geom_hline(data=tufte.grid, aes(yintercept=as.numeric(breaks)), colour="grey90")

all.tasks <- tasks %>% filter(date.type=="Start") %>% select(Task)
x.breaks <- seq(length(all.tasks$Task) + 0.5 - 3, 0, by=-3)

timeline <- ggplot(tasks, aes(x=Task, y=task.date, colour=Project)) + 
  geom_line(size=6) + 
  geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") + 
  labs(x=NULL, y=NULL) + coord_flip() + 
  scale_y_date(breaks="2 months", labels=date_format("%b ‘%y")) + 
  theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1))
timeline
ggsave(timeline, filename="../Figures/timeline.pdf", 
       width=6.5, height=6.5, units="in", device=cairo_pdf)
