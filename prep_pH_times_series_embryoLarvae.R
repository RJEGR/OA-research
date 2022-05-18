# Ricardo Gomez-Reyes April 2022
# Acidification experiment

# Prepare a data subset of pH measures from the embrio to Larvae experiment step

# Sensors were settle at the top and fish tank during the experiment
# Treatments:
# Experimental I (pH 7.6): c('Canal-1', 'Canal-3')
# Experimental II (pH 7.8): c('Canal-2', 'Canal-4')
# Canal 3 y 4 were collected from the fish tank
# # From February 04 to 09 pH measures were registered by the sensors every 5 seconds

# 1) Embrio to Larva dataset ----

rm(list = ls())
if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)

library(tidyverse)

source(paste0(getwd(), "/stats.R"))

path <- '~/Documents/DOCTORADO/pH_measures/2022_/embryo_larvae_dataset/'

pattern_f <- '.dat$'

# list.files(path, pattern = pattern_f,  full.names = TRUE)

df <- read.files(path, pattern_f)

df %>% distinct(date)

cols <- names(df %>% select_if(is.numeric))

# Recode canal to experimental names
recode_w <- c("Canal-1", "Canal-2", 'Experimental-I', 'Experimental-II')
recode_w <- structure(recode_w, names = cols)

df %>% 
  mutate(id = 1:nrow(.)) %>%
  pivot_longer(cols = all_of(cols), values_to = 'Obs') %>%
  drop_na(Obs) %>%
  mutate(name = recode(name, !!!recode_w)) %>%
  filter(name %in% c('Experimental-I', 'Experimental-II')) %>%
  mutate(dataset = 'Embrio_to_Larvae') -> df_longer

write_rds(df_longer, file = paste0(getwd(), '/pH_embryoLarvae_longer_set.rds'))
# write_rds(df_longer, file = paste0(path, '/pH_embryoLarvae_longer_set.rds'))

# End here or

# filter date and pH based on DIC/TA results

library(lubridate)

df %>%
  mutate(date = paste0(date,"T",hour)) %>%
  mutate(date = lubridate::mdy_hms(date)) %>%
  mutate(hour = hour(date)) %>%
  filter(between(hour, 11, 12)) %>%
  pivot_longer(cols = all_of(cols), values_to = 'Obs') %>%
  drop_na(Obs) %>%
  mutate(name = ifelse(name %in% c('Canal-1', 'Canal-3'),
    'Experimental I', 'Experimental II')) %>%
  mutate(date = date(date)) %>%
  group_by(name, date) %>%
  summarise(a = mean(Obs), sd = sd(Obs), n = n()) %>% 
  arrange(date)
  
  
# Previs
# -----


x_breaks <- nrow(ph_df)*5 / 3600 # breaks by hours


ocount(df, 3) 

l <- list()

for(i in seq(1, 5, by = 0.5)) { l[[i]] <- ocount(dff1, i) }


subtitle <- expression('Outlier detection by the z Standard Deviations from the Mean')
caption <- expression('z = (x - mean(x)) / sd(x)')


do.call(rbind,l) %>%
  ungroup() %>%
  mutate(total = N+Y, pct = 100*(N/total)) %>%
  # filter(name %in% c('Canal-3', 'Canal-4')) %>%
  ggplot(aes(x = g, y = pct, color = name)) +
  geom_line(aes(group = name), orientation = "x", linetype = 'dashed') +
  geom_point() +
  labs(x ='Z score', y = '% data not removed', caption = caption) +
  # scale_color_manual(values = getPalette) +
  theme_bw(base_size = 10, base_family = "GillSans") +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 7),
    strip.background = element_blank(),
    panel.grid = element_line(size = rel(0.5)),
    panel.grid.minor = element_line(size = rel(0)),
    panel.border = element_blank()) +
  guides(colour = guide_legend("")) -> psave

psave

# dff1 %>%
#   pivot_longer(cols = namesL, values_to = 'Obs') %>%
#   drop_na(Obs) %>%
#   group_by(name, date) %>%
#   summarise(qqfun(Obs)) %>%
#   mutate(z = zfun(y)) %>%
#   mutate(outlier = ifelse(abs(z) >= 3, 'Y', 'N')) -> df_summ


# do.call(rbind,l) %>% view()

df_longer %>%
  drop_na(Obs) %>%
  group_by(date, name) %>%
  mutate(value = Obs) %>%
  summarise(
    a = mean(value), sd = sd(value), IC = IC(value),
    upper = a+IC, lower = a-IC,
    zupper = a+(3*sd), zlower = a-(3*sd), n = n()) -> out_stats

out_stats %>%
  # filter(name %in% 'Canal-2') %>%
  ggplot(aes(y = a, x = date, color = name)) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) +
  # facet_grid(.~Buffer, scales = 'free_y') +
  geom_point() +
  geom_line(aes(group = name), orientation = "x", 
    linetype = 'dashed') +
  # geom_bar(stat="identity", fill = 'white', color = 'grey67') + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) 
# scale_color_manual('', values = getPalette)

# bind (ie. rbind data from larvae and poslarvae) 
