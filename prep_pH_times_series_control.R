# Ricardo Gomez-Reyes April 2022
# Acidification experiment

# Prepare a data subset of pH measures in the control treatment

# Because the sensor stability, the 'Canal-2' is only considered from the asentamiento_canal_1_2_agua_marina dataset

# From February 28 to March 05 pH measures were registered by the sensor every 5 seconds

rm(list = ls())

options(stringsAsFactors = FALSE)

library(tidyverse)

source(paste0(getwd(), "/stats.R"))

path <- '~/Documents/DOCTORADO/pH_measures/2022_/asentamiento'

pattern_f <- 'asentamiento_canal_1_2_agua_marina.dat$'

# list.files(path, pattern = pattern_f,  full.names = TRUE)

df <- read.files(path, pattern_f = pattern_f)

df %>% distinct(date)

cols <- names(df %>% select_if(is.numeric))


df %>% 
  mutate(id = 1:nrow(.)) %>%
  pivot_longer(cols = all_of(cols), values_to = 'Obs') %>%
  drop_na(Obs) %>%
  filter(name %in% c('Canal-2')) %>%
  mutate(name = 'Control') %>%
  mutate(dataset = 'Larvae_to_Settlement') -> df_longer


# saveRDS(df_longer, file = paste0(path, '/pH_control_longer_set.rds'))
write_rds(df_longer, file = paste0(getwd(), '/pH_control_longer_set.rds'))

# End here or
# Previs

library(ggh4x)

df_longer %>%
  sample_n(1000) %>%
  ungroup() %>%
  ggplot(aes(id, Obs, color = name, group = name)) +
  geom_point(alpha = 0.5, size = 0.1) +
  scale_y_continuous(breaks = seq(7,8.15, by = 0.1)) +
  # scale_x_continuous(breaks = seq(0, x_breaks, by = 12)) +
  labs(y = 'pH', x = 'Hour') +
  theme_bw(base_size = 10, base_family = "GillSans") +
  theme(
    legend.position = 'none',
    legend.text = element_text(size = 7),
    strip.background = element_blank(),
    panel.grid = element_line(size = rel(0.5)),
    panel.grid.minor = element_line(size = rel(0)),
    panel.border = element_blank()) +
  guides(colour = guide_legend("")) -> psave

max(df_longer$Obs) -> M
stringr::str_length(M)

M

M-M/3

psave + 
  scale_y_continuous(guide = "axis_minor",
    minor_breaks = seq(8,M, by = 0.005)) +
  theme(axis.ticks.length.y = unit(0.5, "cm"),
    ggh4x.axis.ticks.length.minor = rel(0.5),
    ggh4x.axis.ticks.length.mini = rel(0.2))
