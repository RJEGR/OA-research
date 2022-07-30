
rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(readr.show_col_types = FALSE, stringsAsFactors = FALSE)

library(tidyverse)
library(ggplot2)
library(lubridate)
library(rstatix)
library(flextable)


pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",`8.0`= "#4575b4", `8`= "#4575b4")

ggsavepath <- paste0(getwd(), '/Figures/')

pattern <- 'MODELING.csv'

file <- list.files(path = getwd(), pattern = pattern, full.names = T)


df <- read_csv(file, show_col_types = FALSE) 

hpfL <- c(24, 48, 60, 108, 624)

df %>% 
  mutate(hpf = factor(hpf, levels = hpfL))  -> df

c('7.6', '7.8', '8') -> pHLevel

pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]

# Is any significant association between pH?

df %>% 
  # group_by(Input) %>%
  cor_test(`8`, method = 'spearman')

df %>% 
  cor_mat(vars = pHLevel, method = 'spearman') %>%
  cor_reorder() %>%
  pull_lower_triangle() #%>%
  # cor_plot(label = TRUE)


df %>% 
  pivot_longer(cols = all_of(pHLevel), names_to = 'pH') %>% 
  drop_na(value) %>% 
  # mutate(pH = as.numeric(pH)) -> df_longer
  mutate(pH = factor(pH, levels = pHLevel)) -> df_longer
# 
# df_longer %>% 
#   select(pH, Input, value ) %>%
#   mutate(id = 1:nrow(.)) %>%
#   pivot_wider( names_from = 'Input',values_from = 'value')

# df %>% pivot_wider(names_from = Input)


# Modeling
library(tidymodels)
# library(performance)
# library(readr)
# library(dotwhisker) 
# library(rstatix)

data <- df_longer

lm_mod <- linear_reg() %>%
  set_engine("lm")

form <- "Input ~ pH + hpf"

formula <- formula(form)

lm_fit <- lm_mod %>%
  fit(formula, data = data)
