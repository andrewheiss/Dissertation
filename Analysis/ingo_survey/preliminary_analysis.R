library(dplyr)
library(ggplot2)
library(ggstance)

source(file.path(PROJHOME, "Analysis", "lib", "graphic_functions.R"))


survey.orgs <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                 "survey_orgs.rds"))
survey.countries <- readRDS(file.path(PROJHOME, "Data", "data_processed", 
                                      "survey_countries.rds"))

survey.all <- survey.countries %>% 
  right_join(survey.orgs, by=c("ResponseID", "survey")) %>%
  filter(!is.na(loop.number))

prop.table(xtabs(~ Q3.2 + Q4.17, data=survey.all), 1)  # rows add to 1
prop.table(xtabs(~ Q3.2 + Q4.17, data=survey.all), 2)  # columns add to 1

plot.data <- survey.all %>%
  select(Q4.17) %>% na.omit %>%
  mutate(Q4.17 = factor(Q4.17, levels=rev(levels(Q4.17))))

plot.data %>% group_by(Q4.17) %>% summarize(num = n())

restrictions.plot <- ggplot(plot.data, aes(y=Q4.17)) + 
  geom_barh() + 
  scale_x_continuous(expand=c(0, 0), limits=c(0, 170)) +
  labs(x=NULL, y=NULL, title="INGOs and government restrictions", 
       subtitle=paste("Q4.17: Overall, how is your organization’s work",
                      "affected by government regulations\nin the country",
                      "you work in?"),
       caption="Source: Global survey of INGOs and government regulations, 2016") +
  theme_ath()

fig.save.cairo(restrictions.plot, filename="misc-ingos-restrictions", 
               width=5, height=2.5)
 
