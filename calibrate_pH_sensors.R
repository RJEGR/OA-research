# Ricardo Gomez- Reyes
# comprobar la sensibilidad entre buffers y sensores de pH
# evaluamos una prueba priori y posteriori de diferencias entre los datos medidos entre los sensores

# Todos los sensores se introdujeron tanto en buffer de pH 4 y 7 

rm(list = ls())

options(stringsAsFactors = FALSE, show_col_types = FALSE)

library(readr)
library(tidyverse)
source(paste0(getwd(), "/stats.R"))

path <- '~/Documents/DOCTORADO/pH_measures/2022_/'

file <- 'pH_buffer*'

file <- list.files(path, pattern = file, full.names = TRUE)

df <- lapply(file, read_table, col_names = F)


df <- do.call(rbind, df)

namesL <- c('Canal-1', 'Canal-2', 'Canal-3', 'Canal-4')


df %>% ggplot(aes(X5, X10)) + geom_jitter() + ggpubr::stat_cor()

# ordering data

df <- df %>% select_if(is.numeric)

data <- df %>% select(any_of(c('X10','X11','X12','X13')))

df1 <- df %>% select(any_of(c('X5','X6','X7','X8')))


dist.method <- 'euclidean'; linkage.method <- 'complete'

hclust(dist(data, method = dist.method), 
  method = linkage.method) %>% cutree(2) -> cutree


data %>% 
  mutate(cutree = cutree) %>%
  pivot_longer(cols = names(data), values_to = 'x') %>%
  mutate(g = NA) %>%
  mutate(g = ifelse(between(x, 4.0,4.5), 'pH-4',
    ifelse(between(x, 7,7.5), 'pH-7', g))) %>%
  drop_na(g) -> data_longer

# clean data ----

library(rstatix)

data_longer %>%
  group_by(g, name) %>%
  mutate(is.outlier = is_outlier(x), 
    is.extreme = is_extreme(x)) -> data_longer


data_longer %>% filter(!is.outlier %in% TRUE) %>%
  ungroup(g) -> data_longer


data_longer %>%
  group_by(name,g) %>%
  summarise(a = mean(x), sd = sd(x), 
    IC = IC(x), n = length(x), se = sd/sqrt(n)) -> df_stats

df_stats

# data_longer %>% ggpubr::ggqqplot('x', color = 'g')

data_longer %>% 
  group_by(name, g) %>%
  # summarise(qqfun(x)) %>%
  shapiro_test(x) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

# 2) Homocelasticidad (TRUE) ----
# Before doing parametric or not test, lets to analyze homogeneity of variance across experimental Levene’s test:

data_longer %>%
  group_by(name) %>%
  levene_test(x ~ as.factor(g)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# we processed to continue w/ a non parametric analysis.

data_longer %>% 
  group_by(g) %>%
  kruskal_test(x ~ name) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> kruskal.stats

# Interpretation: As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.


data_longer %>% 
  ggplot(aes(y = x, x = name)) + 
  # geom_boxplot(outlier.alpha = 0) -> psave
  geom_point(alpha = 0.3) +
  stat_boxplot(geom ='errorbar',
    width = 0.2, position = position_dodge(0.6)) +
  facet_grid(~ g, scales = 'free_y') -> psave



# Additionally, The Wilcoxon rank sum test is a non-parametric alternative to the independent two samples t-test for comparing two independent groups of samples, in the situation where the data are not normally distributed

data_longer %>%
  group_by(g) %>%
  pairwise_wilcox_test(x ~ name, 
    conf.level = 0.95) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance("p") -> stats.test


stats.test %>% add_xy_position() -> stats

title <- get_pwc_label(stats)

subtitle <- get_test_label(kruskal.stats, detailed = TRUE)

# stats$y.position <- stats$y.position+out_stats$sd

psave + 
  ggpubr::stat_pvalue_manual(stats, hide.ns = T, tip.length = 0.001) +
  labs(title = title, subtitle = subtitle, 
    y = 'pH', x = '') +
  theme_classic(base_size = 10, base_family = "GillSans") 

# mVolts
df1 <- df

# c('X5','X6','X7','X8')

df1 %>% as_tibble() %>%
  mutate(Buffer = ifelse(between(X10, 4.0,4.15), '4',
    ifelse(between(X10, 7,7.15), '7', NA))) %>%
  drop_na(Buffer) %>%
  pivot_longer(cols = c('X5','X6','X7','X8'), names_to = 'Canal') %>%
  group_by(Buffer, Canal) %>%
  summarise(Mean = mean(value), n = n()) %>% 
  pivot_wider(names_from = Canal, values_from = Mean) %>% view()

# pHs

df1 %>% as_tibble() %>%
  mutate(Buffer = ifelse(between(V10, 4.0,4.15), '4',
    ifelse(between(V10, 7,7.15), '7', NA))) %>%
  drop_na(Buffer) %>%
  pivot_longer(cols = names(df1)[10:13], names_to = 'Canal') %>%
  group_by(Buffer, Canal) %>%
  summarise(Mean = mean(value), n = n()) %>% 
  pivot_wider(names_from = Canal, values_from = Mean) %>%
  view()


df1 %>% as_tibble() %>%
  select(!names(df1)[1:4]) %>%
  mutate(Buffer = ifelse(between(V10, 4,5), '4',
    ifelse(between(V10, 7,8), '7', 
      ifelse(between(V10, 9,10), '10', NA)))) %>%
  mutate(id = 1:nrow(df1)) %>%
  drop_na(Buffer) %>%
  select(id, Buffer, namesL) %>%
  pivot_longer(cols = namesL, names_to = 'Canal', values_to = 'mV') -> df_mV

names(df1)[10:13] <- paste0('pH-', namesL)

df1 %>% as_tibble() %>%
  select(!names(df1)[1:4]) %>%
  mutate(Buffer = ifelse(between(`pH-Canal-1`, 4,5), '4',
    ifelse(between(`pH-Canal-1`, 7,8), '7', 
      ifelse(between(`pH-Canal-1`, 9,10), '10', NA)))) %>%
  mutate(id = 1:nrow(df1)) %>%
  drop_na(Buffer) %>%
  select(id, Buffer, names(df1)[10:13]) %>%
  pivot_longer(cols = names(df1)[10:13], 
    names_to = 'Canal', values_to = 'pH') -> df_pH


dfplot <- cbind(df_mV, select(df_pH, pH))

head(dfplot)

zscore <- function(x) {
  z <- (x - mean(x)) / sd(x)
  return(z)
}


dfplot %>%
  mutate(Buffer = factor(Buffer, levels = c('4', '7', '10'))) %>%
  ggplot(aes(pH,mV, group = Buffer, color = Canal)) +
  facet_wrap(~ Buffer, scales = 'free') +
  geom_point() +
  theme_bw(base_family = "GillSans", base_size = 14)

dfplot %>%
  mutate(Buffer = factor(Buffer, levels = c('4', '7', '10'))) %>%
  group_by(Buffer, Canal) %>%
  mutate(z = zscore(mV),
    outlier = sum(abs(z > 3))) %>%
  ggplot(aes(id,mV, group = Buffer, color = Canal)) +
  # ggplot(aes(id,mV, group = Canal, color = as.factor(outlier))) +
  geom_point() +
  facet_wrap(~ Buffer, scales = 'free') +
  theme_bw(base_family = "GillSans", base_size = 14) +
  labs(x = '')

dfplot %>%
  mutate(Buffer = factor(Buffer, levels = c('4', '7', '10'))) %>%
  ggplot(aes(Canal,pH, group = Canal)) +
  geom_point(alpha = 0.5, position = position_dodge(0.6)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(0.6)) +
  geom_boxplot(width = 0.3, position = position_dodge(0.6),
    outlier.alpha = 0) +
  stat_summary(fun=mean, geom="point", shape=23,
    size=1, position = position_dodge(0.6)) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap(~ Buffer, scales = 'free')

#

library(rstatix)

IC <- function(x, conf = 0.99) {
  a <- mean(x)
  sd <- sd(x)
  n <- length(x)
  err <- qnorm(p = conf, mean = a, sd = sd, lower.tail = F) * sd / sqrt(n)
  
  return(err)
}

qqfun <- function(x) {
  x <- x[x > 0]
  qq <- qqnorm(x, plot.it = F) 
  qq %>% as_tibble()
}

zfun <- function(x) {
  x <- x[x > 0]
  z <- c((x - mean(x)) / sd(x))
  # z %>% as_tibble()
  return(z)
}

source("~/Documents/GitHub/Estadistica_UABC/anova_and_gaussianity.R")

dfplot %>%
  drop_na(mV) %>%
  group_by(Buffer, Canal) %>%
  summarise(qqfun(pH)) %>%
  mutate(z = zfun(y)) %>%
  mutate(outlier = ifelse(abs(z)>=3, TRUE, FALSE)) -> outliersdf

outliersdf

subtitle <- expression('Outlier detection by the i Standard Deviations from the Mean')
caption <- expression('Zscore = (x - mean(x)) / sd(x)')

#

outliersdf %>%
  ggplot(aes(x, z, color = outlier)) +
  geom_smooth(method = "lm", linetype="dashed", size = 0.5, alpha=0.5, 
    se = TRUE, na.rm = TRUE) +
  geom_point(size = 2.5, alpha = 0.8) + 
  geom_rug(aes(color = outlier), length = unit(0.01, "npc")) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  labs(x = "Expected", y = expression(Z[score]), caption = caption, subtitle = subtitle) +
  scale_color_manual(name = expression("Outlier-"~sigma), 
    values = c('black', 'red')) +
  theme(legend.position = "none") -> psave

psave +
  facet_grid(Buffer~Canal) +
  ggpubr::stat_cor(aes(group = Buffer), 
    method = "pearson", cor.coef.name = "R", p.accuracy = 0.001)


#

dfplot %>%
  drop_na(pH) %>%
  group_by(Buffer, Canal) %>%
  mutate(value = pH) %>%
  summarise(
    a = mean(value), sd = sd(value), IC = IC(value),
    upper = a+IC, lower = a-IC,
    zupper = a+(3*sd), zlower = a-(3*sd), n = n()) -> out_stats

out_stats %>%
  # mutate(name = gsub("[.]", " ", name)) %>%
  # mutate(hpf = as.factor(hpf)) %>%
  ggplot(aes(y = a, x = Canal)) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) +
  facet_grid(.~Buffer) +
  geom_bar(stat="identity", fill = 'white', color = 'grey67') + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  ylim(0,8) +
  # labs(y = expression("RE (" ~Log[10]~")"), x = "") +
  # scale_color_manual(values = c('black', 'Blue')) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(legend.position = 'none') -> psave

psave

dfplot %>%
  ggplot(aes(mV, pH)) +
  # facet_grid(Buffer~Canal, scales = 'free', space = 'free') +
  facet_wrap(~ Buffer + Canal, scales = 'free') +
  geom_smooth(method = "lm", linetype="dashed", size = 0.5, alpha=0.5, 
    se = TRUE, na.rm = TRUE) +
  geom_point(size = 2.5, alpha = 0.8) + 
  geom_rug(length = unit(0.01, "npc")) +
  ggpubr::stat_cor(aes(group = Buffer), 
    method = "pearson", cor.coef.name = "R", p.accuracy = 0.001) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(legend.position = "none")

# Testing mV ----

dfplot %>%
  drop_na(mV) %>%
  group_by(Buffer, Canal) %>%
  mutate(value = mV) %>%
  summarise(
    a = mean(value), sd = sd(value), IC = IC(value),
    upper = a+IC, lower = a-IC,
    zupper = a+(3*sd), zlower = a-(3*sd), n = n()) -> out_stats

out_stats %>%
  ggplot(aes(y = a, x = Canal)) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) +
  facet_grid(.~Buffer, scales = 'free_y') +
  geom_bar(stat="identity", fill = 'white', color = 'grey67') + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(legend.position = 'none') -> psave

psave

dfplot %>%
  mutate(Buffer = factor(Buffer, levels = c('4', '7', '10'))) %>%
  ggplot(aes(Canal,mV, group = Canal)) +
  geom_point(alpha = 0.5, position = position_dodge(0.6)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(0.6)) +
  geom_boxplot(width = 0.3, position = position_dodge(0.6),
    outlier.alpha = 0) +
  stat_summary(fun=mean, geom="point", shape=23,
    size=1, position = position_dodge(0.6)) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap(~ Buffer, scales = 'free')
