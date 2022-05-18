# Evaluacion de los datos respirometricos.
# 1) preparar datos de entrada para la formula
# 2) Inspeccionar toma de datos
# 3) 

# CLEAR OBJECT LIST AND IMAGE CANVAS   
rm(list = ls());
if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)


library(tidyverse)
library(ggplot2)

path <- '~/Documents/DOCTORADO/Respitometría/'
pattern <- 'Oxygen.xlsx$'
files <- list.files(path = path, pattern = pattern, full.names = T)

df <- readxl::read_excel(files[1], col_names = T, skip = 12) %>% 
  rename('date' = `Date/Time`, 'min' = `Time/Min.`)

# if work w/ dates, prepare lubridate data ----


df %>% 
  rename('date' = `Date/Time`, 'min' = `Time/Min.`) %>%
  separate(col = date, into = c('date', 'time'), sep = ' ') %>%
  mutate(date = paste0(date,"T",time)) %>%
  mutate(date = lubridate::mdy_hms(date)) %>%
  mutate(time = lubridate::hour(date))

# -----

cols <- names(df)

cols <- cols[grepl('^[A-Z][0-9]$', cols)]

df %>% select(date, min, all_of(cols)) %>%
  pivot_longer(cols = all_of(cols)) -> df_longer

df_longer %>% ggplot(aes(value)) + geom_histogram()
df_longer %>% ggplot(aes(y = value, x = min)) + 
  geom_point() + scale_y_reverse()

