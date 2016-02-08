library(MASS)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(countrycode)
library(WDI)
library(lubridate)
library(ggplot2)
library(viridis)
library(gridExtra)
library(Cairo)


# Models
model.simple <- lm(v2csreprss ~ icrg.stability + yrsoffc + years.since.comp + 
                     opp1vote + e_polity2, data=full.data)
summary(model.simple)

model.simple1 <- lm(v2xcs_ccsi ~ icrg.stability + yrsoffc + years.since.comp + 
                     opp1vote + e_polity2, data=full.data)
summary(model.simple1)

model.simple2 <- lm(v2cseeorgs ~ icrg.stability + yrsoffc + years.since.comp + 
                      opp1vote + e_polity2, data=full.data)
summary(model.simple2)

model.simple3 <- lm(cs_env_sum ~ icrg.stability + yrsoffc + years.since.comp + 
                      opp1vote + e_polity2, data=full.data)
summary(model.simple3)


model.full <- lm(v2csreprss ~ icrg.stability + yrsoffc + years.since.comp + 
                   opp1vote + e_polity2 + physint + gdpcap.log + population.log + 
                   oda.log + countngo + globalization, data=full.data)
summary(model.full)


model.simple.ord <- polr(v2csreprss_ord ~ icrg.stability + yrsoffc + years.since.comp + 
                           opp1vote + e_polity2, data=full.data)
summary(model.simple.ord)

library(rstanarm)
options(mc.cores = parallel::detectCores())
model.simple.b <- stan_lm(v2csreprss ~ icrg.stability + yrsoffc + 
                            years.since.comp + opp1vote + e_polity2, 
                          data=full.data, prior=NULL, 
                          seed=1234, adapt_delta=0.999)
print(model.simple.b, digits=2)

model.full.b <- stan_lm(v2csreprss ~ icrg.stability + yrsoffc + years.since.comp + 
                          opp1vote + e_polity2 + physint + gdpcap.log + population.log + 
                          oda.log + countngo + globalization, 
                        data=full.data, prior=NULL, 
                        seed=1234, adapt_delta=0.999)
print(model.full.b, digits=2)


plot.data.simple <- plot(model.simple.b, 
                         ci_level=0.8, outer_level=0.95)$data %>%
  mutate(model = "Simple    ", params = as.character(params))

plot.data.full <- plot(model.full.b, regex_pars=".", 
                       ci_level=0.8, outer_level=0.95)$data %>%
  mutate(model = "Full", params = as.character(params))

params.ignore <- c("sigma", "log-fit_ratio", "R2", "mean_PPD", "log-posterior")

param.levels <- plot.data.full %>% 
  filter(params != "(Intercept)", !(params %in% params.ignore)) %>%
  mutate(params = as.character(params)) %>%
  arrange(desc(y)) %>% select(params) %>% c %>% unlist

param.labels <- c("Government stability (ICRG)", 
                  "Years executive in office",
                  "Years since last competitive election",
                  "Opposition vote share",
                  "Polity IV",
                  "Physical integrity rights (CIRI)",
                  "GDP per capita (log)",
                  "Population (log)",
                  "Development aid (log)",
                  "INGO members/volunteers (log)",
                  "Globalization (KOF)")

plot.data <- bind_rows(plot.data.simple, plot.data.full) %>%
  filter(params != "(Intercept)", !(params %in% params.ignore)) %>%
  mutate(params = factor(params, levels=rev(param.levels), 
                         labels=rev(param.labels), ordered=TRUE))

fig.model <- ggplot(plot.data, aes(x=params, y=mean, colour=model)) + 
  geom_hline(yintercept=0, colour="#DE4D67", alpha=0.6, size=1) + 
  # geom_segment(aes(y=l, yend=h, x=params, xend=params), size=1.5,
  #              position=position_dodge(width=1)) +
  geom_pointrange(aes(ymin=ll, ymax=hh), size=0.75, fatten=1,
                  position=position_dodge(width=1)) + 
  labs(x=NULL, y="Coefficient (mean)") + 
  scale_colour_manual(values=viridis(2, begin=0.3, end=0.7), name=NULL,
                      guide=guide_legend(reverse=TRUE)) +
  coord_flip() + 
  theme_light(9, base_family="Source Sans Pro") + 
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        legend.key=element_blank(), legend.margin=unit(0.25, "lines"),
        panel.grid.minor.y=element_blank())

ggsave(fig.model, 
       filename="~/Research/••Dissertation/Administration/Progress memos/2016-01-27/fig_model.pdf",
       width=5, height=3, units="in", device=cairo_pdf)
ggsave(fig.model,
       filename="~/Research/••Dissertation/Administration/Progress memos/2016-01-27/fig_model.png", 
       width=5, height=3, units="in", type="cairo", dpi=300)

# Higher number for v2csreprss is less repression; under 0 is bad


# model.ord.b <- stan_polr(v2csreprss_ord ~ icrg.stability + yrsoffc + opp1vote + 
#                            e_polity2, data=full.data,
#                           prior=NULL, seed=1234)
# model.ord.b
# plot(model.ord.b)


# ggplot(full.data, aes(x=icrg.stability, y=v2csreprss)) + 
#   geom_point(aes(colour=v2csreprss_ord)) + stat_smooth()
# 
# ggplot(full.data, aes(x=v2csreprss_ord, y=icrg.stability)) + 
#   geom_violin() + 
#   geom_point(stat="summary", fun.y="mean", size=5)
