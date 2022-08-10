# TEST: The chi-square test of independence is used to analyze the frequency table (i.e. contengency table) formed by two categorical variables. The chi-square test evaluates whether there is a significant association between the categories of the two variables 
# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

rm(list = ls())

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)

library(tidyverse)
library(flextable)
library(rstatix)

ggsavepath <- paste0(getwd(), '/Figures')

pattern <- "SUMMARY_PCT_TABLES.csv"

files <- list.files(path = getwd(), pattern = pattern, full.names = T)

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",`8.0`= "#4575b4", `8`= "#4575b4")

pHLevel <- c("8.0", "7.8", "7.6")

hpfL <- c("24",  "48",  "60", "108", "624")

pHpalette <- pHpalette[match( pHLevel, names(pHpalette))]

read_csv(files, comment = "#") %>% 
  mutate(Development = ifelse(hpf == 24 & g == "Trocophore", "B", Development)) %>%
  filter(Development %in% "A") %>%
  select(-Replica, -dpf) %>%
  pivot_longer(cols = all_of(pHLevel), names_to = "pH", values_to = "pct") %>%
  mutate(pH = factor(pH, levels = pHLevel)) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  mutate(Stress = factor(Stress, levels = c("Chronic", "Acute"))) -> df

df %>%
  group_by(Stress, pH, hpf, g) %>%
  rstatix::get_summary_stats(type = "quantile") %>% # "mean_ci"
  ggplot(aes(x = hpf, y = `50%`, color = pH, group = pH)) +
  facet_grid(~ Stress, scales = 'free_x',space = "free_x" ) +
  geom_point(position = position_dodge(width = 0.3), size = 2.5, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual("", values = pHpalette) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'Quantiles (25,50,75)') -> ps


ps + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> ps



ggsave(ps, filename = 'percentage.png', path = ggsavepath, 
  width = 5.5, height = 3)



df %>%
  filter(Stress %in% "Chronic") %>%
  ggplot(aes(y = pct, x = pH, color = pH, fill = pH)) +
  facet_wrap(~ hpf, nrow = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width = 0.3, outlier.alpha = 0) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'Quantiles (25,50,75)') -> ps


ps + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> ps

ps + stat_summary(fun=mean, geom ="line", aes(group = 2), size= 0.5, color = 'black')


# summary data to join 

df %>%
  group_by(Stress, pH, hpf, g) %>%
  rstatix::get_summary_stats(type = "mean_sd") %>%
  select(hpf, Stress, pH, mean, sd, n) %>%
  arrange(hpf) %>%
  flextable() %>% 
  autofit(add_w = 0, add_h = 0) %>%
  align(align = "center") %>%
  merge_v() %>%
  bg(~ pH == '8.0', 
    j = c('pH','mean', 'sd'), 
    bg = "#4575b4", part = "body") %>%
  bg(~ pH == '7.8', 
    j = c('pH','mean', 'sd'), 
    bg = "#abdda4", part = "body") %>%
  bg(~ pH == '7.6', 
    j = c('pH','mean', 'sd'), 
    bg = "#d73027", part = "body")

# data inputs for model ----


df %>%
  group_by(hpf, pH) %>%
  mutate(is.extreme = is_extreme(pct)) %>%
  filter(!is.extreme %in% TRUE) %>%
  select(-is.extreme) -> df_filtered


df_filtered %>% 
  group_by(Stress, pH, hpf, g) %>%
  rstatix::get_summary_stats(type = "mean_sd") %>%
  select(hpf, Stress, pH, mean, sd, n) %>%
  arrange(hpf) %>%
  filter(Stress %in% "Acute") %>%
  select(hpf, pH, mean) %>%
  pivot_wider(names_from = pH, values_from = mean)

# visto en: https://doi.org/10.1093/icesjms/fsx017
# To evaluate proportional ...  as a function of measured pH in combination with DO or calcification group, proportions data were logit transformed and evaluated for any significant response to pH and DO using Generalized Linear Models (GLMs) in R (Warton and Hui, 2011). In this paper, we demonstrated using theory, examples, and simulations that logistic regression and its random-effects counterpart have advantages over analysis of arcsine-transformed data in power and interpretability

# Transform dataset ----

# logit {car}
# Computes the logit transformation logit = log[p/(1 - p)] for the proportion p.

df_filtered %>% mutate(logit = car::logit(pct)) -> df_filtered

# revisar metodo en; https://peerj.com/articles/4794/

library(tidymodels)

formula <- formula('logit ~ pH:hpf')

log_fit <- glm(formula, data = df_filtered)

tidy(log_fit)

# model <- glm(formula, family = poisson, data = df)
# model <- rstanarm::stan_glm(formula = formula, data = df)

# check_model(model)
# rstanarm::prior_summary(model)
# check_zeroinflation(model)

