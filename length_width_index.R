# CLEAR OBJECT LIST AND IMAGE CANVAS   
rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)


library(tidyverse)
library(ggplot2)
library(rstatix)

ggsavepath <- paste0(getwd(), '/Figures/')

pattern <- 'length_width_dataset.csv'

file <- list.files(path = getwd(), pattern = pattern, full.names = T)

df <- read_csv(file, show_col_types = FALSE) %>% 
  rename('Length' = `L1 (um)`,'Width' =  `L2 (um)`) %>%
  drop_na(Length, Width) %>%
  mutate_at(vars("Length","Width"), function(x) x / 10) %>% # divide by factor 10X to convert units to um
  select(hpf, pH, group, Length, Width, Stage, Shell)

# prepare index

df %>%
  ggplot(aes(Length, Width, group = pH)) + # color = as.factor(pH),  
  facet_grid(~ pH ) +
  geom_point(alpha = 0.5, shape = 1) +
  scale_color_viridis_d('', option = "plasma", end = .7) +
  geom_smooth(se = F, method = lm, color = 'blue') +
  ggpubr::stat_cor(method = "pearson", cor.coef.name = "R", p.accuracy = 0.001) +
  labs(x = expression("Length ("*mu*"m)"), y = expression("Width ("*mu*"m)")) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank()) -> psave


ggsave(psave, filename = 'Body_index_cor.png', path = ggsavepath, width = 5, height = 3)



df %>%
  group_by(pH) %>%
  cor_test(Length,Width, method = 'pearson')

df %>%
  ggplot(aes(Length, Width, color = as.factor(hpf))) + # color = as.factor(pH),  
  facet_grid(~ pH ) +
  geom_point(alpha = 0.5, shape = 1) +
  labs(x = expression("Length ("*mu*"m)"), y = expression("Width ("*mu*"m)")) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank())

df %>%
  group_by(hpf, pH) %>%
  mutate(Index = sqrt(Length*Width)) -> df


# 0) Remove (index) outliers ----- 
nrow(df)

df %>% group_by(hpf, pH) %>% identify_outliers(Index) %>% group_by(hpf, pH) %>% tally(is.extreme)

df %>%
  group_by(hpf, pH) %>%
  mutate(is.extreme = is_extreme(Index)) %>%
  filter(!is.extreme %in% TRUE) %>% 
  select(-is.extreme) -> df_filtered


# 1) test if gaussianity (PARTIAL FALSE) ----

df_filtered %>% 
  group_by(hpf, pH) %>% 
  rstatix::shapiro_test(Index) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE)) 

# 2) Homocelasticidad (FALSE) ----
# Before doing parametric or not test, lets to analyze homogeneity of variance across experimental Levene’s test:

df_filtered %>%
  group_by(pH) %>%
  levene_test(Index ~ as.factor(hpf)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# If the p-value or significance is above  0.05, we can assume homogeneous variance based on the available data. Ex. The p value of 0.144 is greater than 0.05 so the null hypothesis is maintained and there is no difference between the variances


# we did not find normality and homocelasticity, therefore as priori we processed to non parametric tests

# 3) Statistical priori test----

# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation where there are more than two groups. It’s recommended when the assumptions of one-way ANOVA test are not met. 

df_filtered %>% 
  group_by(hpf) %>%
  rstatix::kruskal_test(Index ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> kruskal.stats

# we found difference between treatment in all/any of the development times

# 4) Statistical posteriori test ----

# En vista de que encontramos outliers y normalidad, pero no homocelasticidad, evaluamos a traves de un test de wilcoxon

df_filtered %>%
  group_by(hpf) %>%
  pairwise_wilcox_test(Index ~ pH,  conf.level = 0.95) %>%
  adjust_pvalue() %>%
  add_significance() -> stats.test

stats.test %>% add_xy_position(x = "pH", scales = "free_y") -> stats

stats$groups

title <- get_pwc_label(stats)

# get_test_label(kruskal.stats, detailed = TRUE)[1]

subtitle <- get_description(kruskal.stats)


df_filtered %>%
  mutate(pH = as.factor(pH)) %>%
  # mutate(vars = factor(vars, levels = varsf)) %>%
  ggplot(aes(y = Index, x = pH,  group = pH)) +
  facet_wrap(~ hpf,nrow = 1) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width = 0.3, outlier.alpha = 0) +
  # geom_point(alpha = 0.5, size = 0.5, position = position_dodge2(width = 0.3), aes(color = as.factor(pH))) +
  stat_summary(fun=median, geom ="line", aes(group = 2), size= 0.5, color = 'blue') +
  # scale_fill_viridis_d('', option = "plasma", end = .7) +
  scale_y_continuous(n.breaks = 5) -> psave


psave + theme(strip.background = element_rect(fill = 'white', color = 'white'),
  panel.border = element_blank()) +
  labs(y = 'Body Index', x = 'pH') -> psave

# psave

# psave +  ggpubr::stat_pvalue_manual(stats)
# 
# psave + 
#   ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
#     remove.bracket = F, tip.length = 0.01, linetype = 'dashed', hide.ns = T) +
#   labs(subtitle = subtitle, title = title, y = 'Body Index', x = 'pH') -> psave


ggsave(psave, filename = 'Body_index.png', path = ggsavepath, 
  width = 7, height = 3)

