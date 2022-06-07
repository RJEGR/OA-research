# Evaluacion de los datos respirometricos.
# 1) Preparar datos de entrada para la formula
# 2) Inspeccionar toma de datos
# 2.1) Evaluar por archivo la formula ( O2[B] - O2[A] ) / ( T[B] - T[A]) = delta 
# delta - adjustment (ie. delta blank) / Indv
# 3) unir datos respirometria y conteos absolutos  

# Oxygen [cO2 [µmol/L]]

# test later: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13162

# CLEAR OBJECT LIST AND IMAGE CANVAS   
rm(list = ls());
if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)


library(tidyverse)
library(ggplot2)
library(lubridate)

mtd_file <- 'MTD_RESPIROMETRY.csv'

path <- '~/Documents/DOCTORADO/Respitometría/'

pattern <- 'Oxygen.xlsx$'

files <- list.files(path = path, pattern = pattern, full.names = T)

# features <- list.files(path = path, pattern = 'conteos_abs_respirometrias', full.names = T)
# 
# features <- read_csv(features) %>% select(ID, Lane, N, Blank, blank_Lane)

mtd_df <- read_csv(list.files(path = path, pattern = mtd_file, full.names = T)) %>% select(ID, N, Design)

namespH <- c("7.6","7.8","8.0")
recodeL <- c('Experimental-I', 'Experimental-II', 'Control')

level_key <- structure(recodeL, names = namespH)

  
read_sdr <- function(file) {
  
  hpf <- sapply(strsplit(basename(file), "_"), `[`, 2)
  pH <- sapply(strsplit(basename(file), "_"), `[`, 3)
  
  df <- readxl::read_excel(file, col_names = T, skip = 12) %>% 
    rename('date' = `Date/Time`, 'min' = `Time/Min.`) %>%
    separate(col = date, into = c('date', 'time'), sep = ' ') %>%
    mutate(date = paste0(date,"T",time)) %>%
    mutate(date = lubridate::mdy_hms(date)) %>%
    mutate(hour = lubridate::hour(date)) %>%
    mutate(hpf = as.numeric(hpf), pH = as.numeric(pH)) %>%
    mutate(row_id  = 1:nrow(.))
  
  return(df)
  
}

# df <- read_sdr(file)


df <- lapply(files, read_sdr)
head(df <- do.call(rbind, df))

df
# 
# df %>% 
#   ggplot(aes(y = B1, x = row_id)) + 
#   geom_point() +
#   facet_grid(hpf ~ pH,scales = 'free', space = 'free')

df %>% distinct(pH)

# -----
cols <- names(df)

cols <- cols[grepl('^[A-Z][0-9]$', cols)]

df %>% 
  select(row_id, date, min, hour, hpf, pH, all_of(cols)) %>%
  pivot_longer(cols = all_of(cols), names_to = 'Lane', values_to = 'Ox') %>%
  mutate(ID = paste0(hpf, '-', pH, '-', Lane)) %>%
  mutate(g = substr(Lane, 1,1)) %>%
  left_join(mtd_df, by = 'ID') %>%
  mutate(pH = recode_factor(pH, !!!level_key)) -> df_longer

# df_longer %>% ggplot(aes(value)) + geom_histogram()

df_longer %>% 
  drop_na(Design) %>%
  ggplot(aes(y = Ox, x = row_id, color = Design, group = Lane)) + 
  geom_path() + 
  facet_grid(hpf ~ pH,scales = 'free', space = 'free')



# # 2.1) evaluar por archivo la formula ( O2[B] - O2[A] ) / ( T[B] - T[A]) = delta 
# como hacer el diff time ???
min(df$date)-max(df$date)

int_diff(min(df$date), max(df$date))

df_longer %>% group_by(Lane) %>% 
  summarise(fOx = max(Ox) - min(Ox), ft = max(min) - min(min))
