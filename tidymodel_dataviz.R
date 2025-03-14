

rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE, show_col_types = FALSE)

library(tidyverse)
library(ggplot2)
library(rstatix)
# library(flextable)
library(lubridate)
library(performance)

setwd("~/Documents/GitHub/OA-research/rproject/")

np_file <- '/pH_aLLdatasets_by_hour_stats.rds' 

new_points <- read_rds(paste0(getwd(), np_file)) %>% 
  ungroup() %>%
  rename('pH' = a) %>%
  select(date, hour, name, pH, sd, se)

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4", `8`= "#4575b4")


fileName <- paste0(getwd(), '/predicted_vars.rds')

plot_data <- read_rds(fileName)

fileName <- paste0(getwd(), '/fit_data_vars.rds')

fit_data <- read_rds(fileName)

# Estimate these metrics:
# Root Mean Squared Error
# lapply(fit_data, performance_rmse)
# performance_rmse(fit_data$Aragonite) 
# performance_mse(fit_data$Aragonite)
performance(fit_data$Aragonite)
# model_performance(fit_data$Aragonite)


do.call(rbind, lapply(fit_data, performance))

# check_model(fit_data$Aragonite)

# Summary stats by day

plot_data %>%
  # mutate(date = day(date))
  filter(vars %in% 'Aragonite') %>%
  group_by(name, vars) %>%
  shapiro_test(.pred) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE)) # %>% # NOT NORMAL


plot_data %>%
  group_by(name, vars) %>%
  mutate(is.outlier = is_outlier(.pred)) %>%
  filter(is.outlier == FALSE) %>%
  select(-is.outlier) -> plot_data

# Filter by day ----


plot_data %>% ungroup() %>% distinct(date) %>% pull(date) %>% as.character() -> recode_date

# struc_group <- c(rep('A) Embryo', 2), rep('B) Larvae', 4), rep('C) Post-larvae', 26))

struc_group <- c(rep('A)', 2), rep('B)', 4), rep('C)', 26))


level_key <- structure(struc_group, names = recode_date)

plot_data %>% mutate(g = recode_factor(as.factor(date), !!!level_key)) -> plot_data


which_vars <- c('CO3', 'Aragonite', 'pCO2_matm')
plot_data %>% pull(vars) %>% unique()

plot_data %>%
  filter(vars %in% which_vars) %>%
  group_by(name, vars, g) %>%
  rstatix::get_summary_stats(.pred) %>%
  select(name, vars, g, n, mean, median, se, iqr, mad) %>%
  view()

plot_data %>%
  filter(vars %in% which_vars) %>%
  group_by(name, vars, g) %>%
  rstatix::get_summary_stats(.pred) %>%
  select(name, vars, g, n, mean, median, se, iqr, mad) %>%
  view()

#

new_points %>%
  mutate(g = recode_factor(as.factor(date), !!!level_key)) -> new_points

new_points %>%
  group_by(name, g) %>%
  rstatix::get_summary_stats(pH) %>%
  select(name, g, n, mean, median, se, iqr, mad) %>%
  view()


new_points %>%
  # filter(g %in% 'Larvae') %>%
  ggplot(aes(x = name, y = pH, color = name)) +
  facet_grid(~ g) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  theme(legend.position = 'top', panel.border = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 45,
      hjust = 1, vjust = 1, size = 10)) +
  labs(y = expression('pH')) +
  scale_color_manual("", values = pHpalette)

new_points %>%
  filter(!name %in% '8') %>%
  ggplot(aes(pH, color = name)) +
  facet_wrap(~ g, scales = 'free_y') +
  stat_ecdf(geom = "step", pad = FALSE, size = 1) +
  labs(y = expression(~f*italic("(x)")), 
    caption = "Empirical Cumulative Distribution") +
  # geom_density(stat = "density") +
  scale_color_manual("", values = pHpalette) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(legend.position = 'top', panel.border = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 45,
      hjust = 1, vjust = 1, size = 10)) +
  scale_color_manual("", values = pHpalette)

plot_data %>%
  filter(vars %in% 'Aragonite') %>%
  # filter(!name %in% '8') %>%
  ggplot(aes(.pred, color = name)) +
  facet_grid(~ g, scales = 'free_y') +
  geom_vline(xintercept = 1, color = 'grey', linetype = 'dashed') +
  stat_ecdf(geom = "step", pad = FALSE, size = 1) +
  labs(y = expression(~f*italic("(x)")), x = expression(Omega["ara"]),
    caption = "Empirical Cumulative Distribution") +
  scale_color_manual("", values = rev(pHpalette)) +
  theme_classic(base_family = "GillSans", base_size = 16) +
  theme(legend.position = 'top', 
    strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank()) -> psave

# design 1
psave + 
  geom_rug(data = subset(plot_data, vars %in% 'Aragonite' & 
      name %in% '7.6'), sides = 'b') +
  geom_rug(data = subset(plot_data, vars %in% 'Aragonite' & 
      name %in% '7.8'), sides = 'b', outside = T,position = 'identity') +
  coord_cartesian(clip = "off") 

# Design 2

psave + 
  geom_rug(data = subset(plot_data, vars %in% 'Aragonite' & 
      name %in% '7.6'), sides = 't',) +
  geom_rug(data = subset(plot_data, vars %in% 'Aragonite' & 
      name %in% '7.8'), sides = 'b') -> psave


ggsavepath <- paste0(getwd(), '/Figures')

ggsave(psave, filename = 'ara_ecdf.png', path = ggsavepath, 
  width = 5.2, height = 3, dpi = 300)

library(ggdensity)

# https://github.com/jamesotto852/ggdensity

# 

new_points %>%
  mutate(ymin = pH-se, ymax = pH+se) %>%
  filter(g %in% 'Embryo') %>%
  ggplot(aes(y = pH, x = id, color = name)) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_hline(yintercept = 8, color = 'grey', linetype = 'dashed') +
  # geom_point() +
  geom_path(aes(group = name), linejoin = "mitre", size = 1) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3, size = 0.5) +
  theme(legend.position = 'top', panel.border = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 45,
          hjust = 1, vjust = 1, size = 10)) +
      labs(y = expression('pH')) +
      scale_color_manual("", values = pHpalette)


# 
# plot_data %>%
#   mutate(ymin = pH-se, ymax = pH+se) %>%
#   ggplot(aes(y = pH, x = date, color = name)) + 
#   theme_bw(base_family = "GillSans", base_size = 14) +
#   geom_hline(yintercept = 8, color = 'grey', linetype = 'dashed') +
#   geom_path(aes(group = name), linejoin = "mitre", size =1) +
#   theme_classic(base_family = "GillSans", base_size = 16) +
#   theme(legend.position = 'top', panel.border = element_blank(),
#     # axis.ticks.x = element_blank(),
#     axis.line.x = element_blank(),
#     axis.text.x = element_text(angle = 45, 
#       hjust = 1, vjust = 1, size = 10)) +
#   labs(y = expression('pH')) +
#   scale_color_manual("", values = pHpalette) -> psave
# 
# psave + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, alpha = 0.1) -> psave
# 
# 


