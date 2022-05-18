# Ricardo Gomez-Reyes April 2022
# Acidification experiment

# Prepare a data subset of pH measures from the embrio to Larvae experiment step

# Sensors were settle at the top and fish tank during the experiment
# Treatments:
# Experimental I (pH 7.6): c('Canal-3')
# Experimental II (pH 7.8): c('Canal-4')
# # From February 10 to March 07 the pH measures were registered by the sensors every 5 seconds
# 3) Larva to settlement dataset ----

rm(list = ls())

options(stringsAsFactors = FALSE)

library(tidyverse)

source(paste0(getwd(), "/stats.R"))

path <- '~/Documents/DOCTORADO/pH_measures/2022_/asentamiento/'

pattern_f <- '.dat$'

list.files(path, pattern = pattern_f,  full.names = TRUE)

df <- read.files(path, pattern_f)

df %>% arrange(-desc(date)) %>% distinct(date)

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
  # mutate(name = ifelse(name %in% 'Canal-3','Experimental I', 'Experimental II')) %>%
  mutate(dataset = 'Larvae_to_Settlement') -> df_longer

# df_longer %>% group_by(name) %>% summarise(quantile(Obs))

write_rds(df_longer, file = paste0(getwd(), '/pH_larvaeSettlement_longer_set.rds'))
# write_rds(df_longer, file = paste0(path, '/pH_larvaeSettlement_longer_set.rds'))

# End here or
# Previs
# -----

# filter date and pH based on DIC/TA results

library(lubridate)

df_longer %>%
  filter(grepl('03/03/2022', date)) %>%
  mutate(date = paste0(date,"T",hour)) %>%
  mutate(date = lubridate::mdy_hms(date)) %>%
  mutate(hour = hour(date)) %>% 
  mutate(date = date(date)) %>%
  filter(between(hour, 15, 15)) %>%
  group_by(name, date) %>%
  summarise(a = mean(Obs), sd = sd(Obs), n = n()) %>% 
  arrange(date) %>% view()

