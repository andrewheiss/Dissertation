library(tidyverse)
library(lubridate)

words <- read_csv(file.path(PROJHOME, "Writing", "words", "words.csv")) %>%
  filter(!is.na(date)) %>%
  mutate(datetime = ymd_hms(date, tz="America/New_York")) %>%
  mutate(date = ymd(format(datetime, "%F")))

words.day <- words %>%
  group_by(date) %>%
  summarise(total = max(words))

annotations <- tribble(
  ~date, ~y, ~align, ~note,
  "2014-03-11", 5000, 0, "Started dissertation git repository",
  "2017-07-07", 5000, 1.1, "Deadline"
) %>%
  mutate(date = ymd(date))

plot.words <- ggplot(words.day, aes(x = date, y = total)) + 
  geom_vline(xintercept = as.numeric(ymd("2017-07-07")),
             linetype = 2) +
  geom_line(size = 1, color = "#FF4136") +
  geom_text(data = annotations, 
            aes(x = date, y = y, hjust = align, label = note),
            family = "Source Sans Pro") +
  annotate("text", x = ymd("2017-04-01"), y = 60000, angle = 75,
           label = "AAAAAAHHHH!!1!", family = "Source Sans Pro Semibold") +
  geom_point(data = annotations,
             aes(x = date, y = 0), shape = 4, stroke = 2) +
  labs(x = NULL, y = "Cumulative word count",
       title = "Dissertation word count vs. time",
       subtitle = "zomg my life is a PhD Comics comic",
       caption = "https://github.com/andrewheiss/Dissertation/tree/master/Writing\nhttp://phdcomics.com/comics/archive.php?comicid=1915") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(limits = c(ymd("2014-01-01"), NA)) +
  theme_bw(base_family = "Source Sans Pro") + 
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = rel(1.6), family = "Source Sans Pro Semibold"),
        plot.subtitle = element_text(size = rel(1.1)),
        plot.caption = element_text(size = rel(0.7), margin = margin(t = 15)))

plot.words

ggsave("~/Desktop/dissertation_word_count.png", plot.words,
       width = 6, height = 4)
