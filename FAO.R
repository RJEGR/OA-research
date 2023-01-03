# https://github.com/openfigis/RefData/blob/gh-pages/FishStat/FSJ_TIMSERIES.csv
# DATA FROM https://www.fao.org/fishery/static/Data/Aquaculture_2022.1.0.zip

setwd("/Users/cigom/Documents/GitHub/OA-research/DIVULGACION/")

path <- paste0(getwd(), '/Aquaculture_2022.1.0/')

f <- list.files(path = path, pattern = 'csv', full.names = T)

library(tidyverse)

# COUNTRY.UN_CODE
# "SPECIES.ALPHA_3_CODE" 
#  "AREA.CODE"  

read_csv(f[8]) %>% filter(Code %in% 'Q_tlw') %>% select(Name_Es) %>% pull() -> MEASURE_UNITS

read_csv(f[5]) %>% filter(grepl('Haliotis', Scientific_Name)) %>% 
  distinct(`3A_Code`) %>% pull() -> ALPHA_3_CODE

read_csv(f[1]) %>% # Aquaculture_Quantity.csv
  filter(SPECIES.ALPHA_3_CODE %in% ALPHA_3_CODE) -> input


# Global abalone consumption (toneladas peso vivo) ----

input %>% 
  # mutate(year = lubridate::year(PERIOD)) # not neccesary
  group_by(PERIOD) %>%
  summarise(VALUE = sum(VALUE)) -> summary_in

summary_in %>% 
  filter(VALUE > 0) %>%
  group_by(PERIOD) %>%
  summarise(VALUE = sum(VALUE)) %>%
  ggplot(aes(x = PERIOD, y = VALUE)) +
  geom_path(size = 6, lineend = 'round') +
  # ggrepel::geom_text_repel(data = subset(summary_in, PERIOD == 2020), 
    # aes(label = scales::comma(round(VALUE))), position = 'stack',
    # family = 'GillSans', size = 7, color = 'blue') +
  labs(x = 'Periodo', y = expression(Log[10]), subtitle = MEASURE_UNITS) +
  # scale_y_continuous(labels = scales::comma) +
  scale_y_log10() +
  theme_bw(base_family = "GillSans", base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank(), legend.position = 'none') -> p

ggsave(p, filename = 'consumo_abulon.png', units = 'cm', width = 12, height = 8, path = getwd())
  
# scale_y_continuous(expression(~PO[4]~(µmol~Kg^{-1})),
  #   sec.axis = sec_axis(~.*50, 
  #     name = expression(~H[4]~SiO[4]~(µmol~Kg^{-1}))))

read_csv(f[5]) %>% filter(grepl('Haliotis', Scientific_Name)) %>%
  select(`3A_Code`, Name_Es, Scientific_Name) -> join_dat

input %>% 
  group_by(SPECIES.ALPHA_3_CODE, PERIOD) %>%
  summarise(VALUE = sum(VALUE)) %>%
  left_join(join_dat, by = c('SPECIES.ALPHA_3_CODE'='3A_Code')) %>%
  filter(VALUE > 0) -> summary_in


summary_in %>% 
  filter(PERIOD > 2000) %>%
  group_by(Scientific_Name) %>%
  summarise(VALUE = sum(VALUE)) %>%
  filter(!Scientific_Name %in% 'Haliotis spp') %>%
  mutate(VALUE = VALUE/sum(VALUE)) 

# porcentaje de los moluscos

read_csv(f[5]) %>% 
  filter(Major_Group_Es %in% 'MOLLUSCA') %>%
  distinct(`3A_Code`) %>% pull() -> ALPHA_3_CODE

read_csv(f[1]) %>% # Aquaculture_Quantity.csv
  filter(SPECIES.ALPHA_3_CODE %in% ALPHA_3_CODE) -> input

read_csv(f[5]) %>% filter(`3A_Code` %in% ALPHA_3_CODE) -> join_dat

# ALPHA_3_CODE
unique(input$MEASURE)

# CPC_Class_En: Abalone, live, fresh or chilled

join_dat %>% view()

input %>%
  filter(PERIOD == 2020) %>%
  group_by(SPECIES.ALPHA_3_CODE, PERIOD) %>%
  summarise(VALUE = sum(VALUE)) %>%
  filter(VALUE > 0) %>%
  left_join(join_dat, by = c('SPECIES.ALPHA_3_CODE'='3A_Code')) %>% 
  group_by(CPC_Class_En) %>%
  summarise(VALUE = sum(VALUE)) %>%
  mutate(PCT = VALUE/sum(VALUE)) %>% filter(PCT > 0.01) %>%
  mutate(CPC_Class_En = fct_reorder(CPC_Class_En, PCT, .desc = TRUE)) %>%
  ggplot(aes(PCT, CPC_Class_En)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent)
  

# pattern <- c("Coral and similar products", "Octopus, live, fresh or chilled", "Cuttle fish and squid")
# 
# ommit <- unique(join_dat$CPC_Class_En)
# ommit[grep(pattern, ommit)]
# 

input %>%
  group_by(SPECIES.ALPHA_3_CODE, PERIOD) %>%
  summarise(VALUE = sum(VALUE)) %>%
  filter(VALUE > 0) %>%
  left_join(join_dat, by = c('SPECIES.ALPHA_3_CODE'='3A_Code')) %>% 
  group_by(CPC_Class_En, PERIOD) %>%
  summarise(VALUE = sum(VALUE)) %>%
  # filter(grepl(ommit, CPC_Class_En))
  mutate(color = ifelse(grepl('Abalone', CPC_Class_En), 'Abalone', '')) %>%
  ggplot(aes(x = PERIOD, y = VALUE, group = CPC_Class_En, color = color)) +
  geom_path(size = 6, lineend = 'round') +
  scale_y_log10() +
  labs(x = 'Periodo', y = expression(Log[10]), subtitle = MEASURE_UNITS) +
  scale_color_manual(values = c('black', 'blue'))
  
# map ----

# granja abulones cultivados

granja <- data.frame(lat = 31.292671422993635, long = -116.41037611040018)

mex <- ggplot2::map_data("world", region = "Mexico") 
# table(mex$region)

mex %>%
  filter(lat > 15 & abs(long) > 110) -> mex

table(mex$group)

west_coast <- subset(map_data("state"), region %in% c("california", "oregon", "washington"))

map <- rbind(mex, west_coast)

ggplot(data = map) + 
  coord_quickmap() + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  geom_point(data = granja, 
    aes(x = long, y = lat), color = 'blue', size = 7) +
  theme_bw(base_family = "GillSans", base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank(), legend.position = 'none')

  