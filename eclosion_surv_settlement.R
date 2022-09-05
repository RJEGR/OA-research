# Prep matrices de datos de:
# evaluar diff estadisticas de los datos 

# El n de los datos es 6, que corresponde a cada pecera por tratamiento o pull de spots por respirometria

# 1) Eclosion (ES NECESARIO DISCUTIR ESTE PROXI) = corresponde a los datos de respirometria, donde se evaluo el numero de individous que corresponden a larvas y huevos presentes en muestras a las 24 h

# 2) supervivencia al final del experimento
# 3) asentamiento
# datavz:
# 1) remover outliers para calclar promedio que se veran en barplots o boxplots
# 2) transformar datos a porcentajes
# 3) colocar los resultados estadisticos en los tres graficos

rm(list = ls())

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)

library(tidyverse)

library(rstatix)

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",`8.0`= "#4575b4", `8`= "#4575b4")

ggsavepath <- paste0(getwd(), '/Figures')

source(paste0(getwd(), "/stats.R"))

competency <- read_rds(paste0(getwd(), 'competency.rds'))

competency %>% distinct(pH) %>% pull(pH) %>% levels() -> pHLevel

pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]

#

namespH <- c("7.6","7.8","8")

recodeL <- c('Experimental-I', 'Experimental-II', 'Control')

level_key <- structure(namespH, names = recodeL)

# mini test ----
competency %>% 
  group_by(pH, hpf, name) %>%
  rstatix::get_summary_stats(value, type = "quantile") -> df_longer_stats

df_longer_stats %>%
  filter(name %in% 'Aspecto.2') %>%
  ggplot(aes(x = as.factor(hpf), y = `50%`, color = pH, group = pH)) +
  # facet_grid(~ name) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.1, position = position_dodge(width = 0.3)) +
  geom_path(position = position_dodge(width = 0.3), size = 1) +
  scale_color_manual("", values = pHpalette) +
  labs(y = 'Quantiles (25,50,75)', x = 'Time (hpf)') +
  scale_y_continuous(labels = scales::percent) -> ps

ps + 
  theme(strip.background = element_rect(fill = 'grey', color = 'white'),
    panel.border = element_blank(), legend.position = 'top')

ggsave(ps, filename = 'test.png', path = ggsavepath, 
  width = 5.5, height = 3)

# ECLOSION ---------


# df %>% mutate(pH = factor(as.character(pH), levels = pHLevel)) %>% as_tibble() -> df

files <- list.files(path = getwd(), pattern = 'df.csv')

head(ecl_df <- read.csv(files[1]) %>% as_tibble())

ecl_df %>% mutate(pH = factor(as.character(pH), levels = pHLevel)) -> ecl_df

ecl_df %>% view()
# recode_pH <- c('Experimental-I', 'Experimental-II', 'Control')

# level_key <- structure(recode_pH, names = unique(ecl_df$pH))

# ecl_df %>% ungroup() %>% mutate(pH = recode_factor(pH, !!!level_key)) -> ecl_df

# proportions

ecl_df %>% # data must be complete
  ungroup() %>%
  pivot_wider(names_from = group,  values_from = Count, values_fn = list) %>%
  unnest(cols = everything()) %>%
  drop_na() %>%
  mutate(Tot = Trochophore+Egg) %>%
  mutate(pct_troch = Trochophore/Tot, pct_egg = Egg/Tot) -> prop_df

# Conteos ----
# si desearamos trabajar sobre los conteos crudos, sin embargo, no hay distribucion interesante entre estos datos
# outlier

ecl_df %>% group_by(pH, group) %>% 
  mutate(is.outlier = is_outlier(Count), 
    is.extreme = is_extreme(Count)) -> ecl_df

ecl_df %>% group_by(pH, group, is.outlier) %>% tally() %>%
  filter(is.outlier %in% TRUE)

#
# Find if difference between groups across experiments and control
# 1) test if gaussianity (TRUE) ----

ecl_df %>% 
  filter(is.outlier == FALSE) %>% 
  group_by(pH, group) %>% rstatix::shapiro_test(Count) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

# From the output, if the p-value > 0.05 implying that the distribution of the data are NOT significantly different from a normal distribution. In other words, we can assume the normality. Otherwise, if the p-value < 0.05 implying that the distribution of the data are  significantly different from a normal distribution. Therefore, we can assume not gaussianity. In other words, we argue a non parametric test to priori and posteriri analysis.

# 2) Homocelasticidad (TRUE) ----
# Before doing parametric or not test, lets to analyze homogeneity of variance across experimental Levene’s test:

ecl_df %>%
  filter(is.outlier == FALSE) %>%
  group_by(group) %>%
  levene_test(Count ~ as.factor(pH)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# If the p-value or significance is above  0.05, we can assume homogeneous variance based on the available data. Ex. The p value of 0.149 is greater than 0.05 so the null hypothesis is maintained and there is no difference between the variances

# 3) test difference by parametric test ----

# 4) Priori statistical test ----

ecl_df %>% filter(is.outlier == FALSE) -> ecl_df

# ANOVA assumes that the variance of the residuals is equal for all groups.

ecl_df %>%
  group_by(group) %>%
  # ungroup() %>%
  rstatix::anova_test(Count ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> res_aov

res_aov

# 5) Posteriori test ----

# T test

ecl_df %>%
  group_by(group) %>%
  # rstatix::pairwise_t_test(Count ~ pH, paired = F) 
  tukey_hsd(Count ~ pH) %>% # If ANOVA used as priori
  adjust_pvalue() %>%
  add_significance()-> stat_test

stat_test %>% add_xy_position(x = "pH", scales = "free_y") -> stats


# ecl_df %>% group_by(group) %>% cohens_d(Count ~ pH, var.equal = TRUE)

# dataviz


title <- get_pwc_label(stats)

# subtitle <- get_description(res_aov)

# create_test_label(
#   statistic.text = "F", statistic = 71.82,
#   parameter = "4, 294",
#   p = "<0.0001",
#   description = "ANOVA",
#   type = "text"
# )

ecl_df %>%
  group_by(pH,group) %>%
  summarise(a = mean(Count), sd = sd(Count), n = n()) %>%
  mutate(ymin = a-sd, ymax = a+sd) -> out_stats

out_stats %>%
  ggplot(aes(y = a, x = as.factor(pH), fill = group)) +
  # geom_col() +
  geom_point() +
  facet_wrap(~ group, scales = 'free_y') +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
  geom_path(aes(group = group))

ecl_df %>%
  group_by(pH,group) %>%
   group_by(pH) %>% mutate(pct = Count/sum(Count)) %>%
  # filter(group == 'Egg') %>%
  ggplot() +
  geom_col(aes(y = pct, x = as.factor(pH), fill = group)) +
  facet_wrap(~ group, scales = 'free_y') -> psave


psave + theme(strip.background = element_rect(fill = 'white'),
  panel.border = element_blank(),
  axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, linetype = 'dashed', hide.ns = T) +
  labs(title = title) -> psave


# Proporciones -----

# The two-proportions z-test is used to compare two observed proportions.

# test anova for mean the prop_df

prop_df %>%
  select(-Tot,-Trochophore,-Egg) %>%
  pivot_longer(-pH) -> prop_df_longer

prop_df_longer %>%
  group_by(name, pH) %>%
  shapiro_test(value) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

prop_df_longer %>%
  group_by(name) %>%
  rstatix::anova_test(value ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") 

prop_df %>% group_by(pH) %>% summarise_if(is.numeric, mean)

# devtools::install_github("ricardo-bion/ggradar")
# 
# library(ggradar)
# 
# prop_df %>%
#   select(-Tot,-Trochophore,-Egg) %>%
#   t() -> dff
# 
# colnames(dff) <- dff[1,]
# 
# dff <- as.data.frame(apply(dff[-1,], 2, as.numeric), row.names = dff[1,])


# Using Z test
# http://www.sthda.com/english/wiki/chi-square-goodness-of-fit-test-in-r

# prop.test not working well
# prop.test(prop_df$pct_egg, prop_df$pct_troch, correct = T)


prop_df %>% 
  select(-Tot,-Trochophore,-Egg) %>%
  group_by(pH) %>% summarise_if(is.numeric, mean) -> xtab

xtab <- prop_df



xtab <- as.table(rbind(
  xtab$pct_troch,
  xtab$pct_egg
))

dimnames(xtab) <- list(
  g = c("pct_troch", "pct_egg"),
  pH = prop_df$pH # levels(prop_df$pH)
)


pairwise_prop_test(xtab)
pairwise_fisher_test(t(xtab))

# Other propotion test used are odds ratios and logistic regression (common used for proportions) and binomial test

# Logistic regression ----
library(tidymodels)

log_mod <- logistic_reg() %>%
  set_engine("glm") 

fit_log_mod <- log_mod %>%
  fit(as.factor(pH) ~ pct_troch, data = prop_df)

fit_log_mod %>% tidy()

# performance::check_model(fit_log_mod)

# priori: Testing Equality of Two Percentages
# testing the null hypothesis that two percentages are equal using a Fisher's Exact Test for an Effect—Dependent Samples 
# OR Performs proportion tests to either evaluate the homogeneity of proportions (probabilities of success) in several groups
# either fisher and prop_test requieres contigency matrix

# T test ----
#  t-test is powered to detect differences in mean proportion between these samples

prop_df_longer %>%
  group_by(name) %>%
  rstatix::pairwise_t_test(value ~ pH) -> stat_test

# stat_test

stat_test %>% add_xy_position(x = "pH", scales = "free_y") -> stats

title <- get_pwc_label(stats)


prop_df_longer %>%
  group_by(pH,name) %>%
  summarise(a = mean(value), sd = sd(value), n = n()) %>%
  mutate(ymin = a-sd, ymax = a+sd) -> out_stats

# pHLevs <- c('Control', 'Experimental-I', 'Experimental-II')

out_stats %>%
  # mutate(pH = factor(pH, levels = pHLevs)) %>%
  ggplot(aes(y = a, x = as.factor(pH), group = name)) +
  geom_col(fill = 'transparent', color = 'black') +
  geom_point() +
  facet_wrap(~ name, scales = 'free_y') +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
  geom_path(aes(group = name)) +
  scale_y_continuous(labels = scales::percent) -> psave


psave + 
  theme_classic(base_family = "GillSans", base_size = 10) +
  theme(strip.background = element_rect(fill = 'white'),
  panel.border = element_blank(),
  axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 7)) -> psave

subtitle <- 'data need to be test w/ glm'

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, 
    linetype = 'dashed', hide.ns = T) +
  labs(title = title, caption = subtitle ) +
  labs(y = '%', x = '') -> psave

ggsave(psave, 
  filename = 'eclosion_ratios.png', path = ggsavepath, 
  width = 5, height = 3.5)

# SUPERVIVENCIA  ----

head(df <- read.csv(files[2]) %>% as_tibble())
level_key <- structure(recode_pH, names = unique(df$pH))
df %>% ungroup() %>% mutate(pH = recode_factor(pH, !!!level_key)) -> df

df %>% 
  filter(group %in% 'Suvirval') %>%
  select(Rep, Count, pH, vol_x_count, initial_tot_count) %>%
  group_by(pH) %>%
  mutate(Count = as.numeric(Count)) %>%
  mutate(is.outlier = is_outlier(Count)) %>%
  filter(is.outlier == FALSE) -> surv_df

# 1) test if gaussianity (TRUE) ----

surv_df %>% 
  group_by(pH) %>% rstatix::shapiro_test(Count) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

# 2) Homocelasticidad (TRUE) ----
# Before doing parametric or not test, lets to analyze homogeneity of variance across experimental using Levene’s test:

surv_df %>%
  ungroup() %>%
  levene_test(Count ~ as.factor(pH)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# 3) Statistics ----
# 4) Priori statistical -----
# ANOVA assumes that the variance of the residuals is equal for all groups.

surv_df %>%
  ungroup() %>%
  rstatix::anova_test(Count ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> res_aov

res_aov

# 5) Posteriori test ----

surv_df %>%
  ungroup() %>%
  tukey_hsd(Count ~ pH) %>% # If ANOVA used as priori
  adjust_pvalue() %>%
  add_significance()-> stat_test

stat_test %>% add_xy_position(x = "pH", scales = "free_y") -> stats

title <- get_pwc_label(stats)

# subtitle <- get_description(res_aov)


surv_df %>%
  summarise(a = mean(Count), sd = sd(Count), n = n()) %>%
  mutate(ymin = a-sd, ymax = a+sd) -> out_stats

out_stats %>%
  ggplot(aes(y = a, x = pH)) +
  # geom_col() +
  geom_point() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
  geom_path(group = 2) -> psave


psave + theme(strip.background = element_rect(fill = 'white'),
  panel.border = element_blank(),
  axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, linetype = 'dashed', hide.ns = T) +
  labs(title = title) -> psave

# presentamos homogeneidad y homocelasticidad entre los conteos crudos, sin embargo queremos analizar los porcentajes, asi que tomaremos las colunas vol_x_count e initial_tot_count para calcular porcentajes y trabajar con una columna de pct

# *) Proporciones ----

df %>% 
  filter(group %in% 'Suvirval') %>%
  mutate(pct = vol_x_count/ initial_tot_count) %>%
  select(Rep, Count, pH, pct) %>%
  group_by(pH) %>%
  mutate(Count = as.numeric(pct)) %>%
  mutate(is.outlier = is_outlier(pct)) %>% # no es necesario pero ok
  filter(is.outlier == FALSE) -> surv_df_pct


surv_df_pct %>%
  ungroup() %>%
  # tukey_hsd(pct ~ pH)
  rstatix::pairwise_t_test(pct ~ pH) -> stat_test

# stat_test

stat_test %>% add_xy_position(x = "pH", scales = "free_y") -> stats

title <- get_pwc_label(stats)


surv_df_pct %>%
  group_by(pH) %>%
  summarise(a = mean(pct), sd = sd(pct), n = n()) %>%
  mutate(ymin = a-sd, ymax = a+sd) -> out_stats

out_stats %>%
  ggplot(aes(y = a, x = as.factor(pH))) +
  geom_col(fill = 'transparent', color = 'black') +
  geom_point() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
  geom_path(aes(group = 2)) +
  scale_y_continuous(labels = scales::percent)-> psave


psave + 
  theme_classic(base_family = "GillSans", base_size = 10) +
  theme(strip.background = element_rect(fill = 'white'),
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 45, 
      hjust = 1, vjust = 1, size = 7)) -> psave

subtitle <- 'data need to be test w/ glm'

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, 
    linetype = 'dashed', hide.ns = T) +
  labs(title = title, caption = subtitle ) +
  labs(y = '%', x = '') -> psave

ggsave(psave, 
  filename = 'survival_ratios.png', path = ggsavepath, 
  width = 5, height = 3.5)

# SETTLEMENT ----

initial_sett_larvae <- 5210

df %>% 
  filter(group %in% 'Settlement') %>%
  select(Rep, Count, pH, X) %>%
  rename('Design' = X) %>%
  group_by(pH, Design) %>%
  mutate(Count = as.numeric(Count)) %>%
  mutate(pct = Count / initial_sett_larvae) %>%
  mutate(is.outlier = is_outlier(Count)) -> sett_df

# source(paste0(getwd(), '/stats.R'))
# 
# sett_df %>%
#   summarise(qqfun(Count)) %>%
#   mutate(z = zfun(y)) %>%
#   mutate(outlier = is_outlier(y)) -> outliersdf
# 
# outliersdf %>%
#   ggplot(aes(x, y)) +
#   facet_grid(~pH) +
#   geom_smooth(method = "lm", linetype="dashed", size = 0.5, alpha=0.5, 
#     se = TRUE, na.rm = TRUE) +
#   geom_point(aes(color = Design), size = 2.5, alpha = 0.8) +
#   ggpubr::stat_cor(aes(group = pH), method = "pearson", cor.coef.name = "R", p.accuracy = 0.001) +
#   theme_bw(base_family = "GillSans", base_size = 14) +
#   # labs(x = "Theorical", y = expression(Z[score]), caption = caption, subtitle = subtitle) +
#   scale_color_manual(name = expression("Outlier-"~sigma), 
#     values = c('black', 'red')) +
#   theme(legend.position = "top")




# 1) test if gaussianity (PARCIAL TRUE) ----

sett_df %>% 
  filter(is.outlier == FALSE) %>% 
  group_by(pH, Design) %>% rstatix::shapiro_test(pct) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

# 2) Homocelasticidad (TRUE) ----

sett_df %>%
  filter(is.outlier == FALSE) %>%
  group_by(Design) %>%
  levene_test(Count ~ as.factor(pH)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# 3) Prior stat test ----
# we processed to continue w/ a non parametric analysis.

# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation where there are more than two groups. It’s recommended when the assumptions of one-way ANOVA test are not met. T

sett_df %>%
  group_by(Design) %>%
  rstatix::kruskal_test(Count ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> kruskal.stats

# 4) Posterior stat test ------
# Additionally, The Wilcoxon rank sum test is a non-parametric alternative to the independent two samples t-test for comparing two independent groups of samples, in the situation where the data are not normally distributed

sett_df %>%
  group_by(Design) %>%
  # pairwise_wilcox_test(Count ~ pH, conf.level = 0.95) %>%
  tukey_hsd(Count ~ pH) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() -> stats.test


stats.test %>% add_xy_position(x = 'pH') -> stats



title <- get_pwc_label(stats)

subtitle <- get_description(kruskal.stats)

sett_df %>%
  group_by(Design, pH) %>%
  mutate(x = Count) %>%
  summarise(
    a = mean(x), sd = sd(x), ymax = a+sd, 
    ymin = a-sd, n = n()) -> out_stats

stats$y.position <- stats$y.position+out_stats$sd

level_key

out_stats %>%
  ggplot(aes(y = a, x = pH)) +
  # geom_col(position = position_dodge2(width = .5)) +
  facet_grid(~ Design) +
  geom_col(fill = 'transparent', color = 'black') +
  geom_point() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
  geom_path(aes(group = 2)) +
  scale_y_continuous(labels = scales::percent) -> psave


psave + 
  theme_classic(base_family = "GillSans", base_size = 10) +
  theme(strip.background = element_rect(fill = 'white'),
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 45, 
      hjust = 1, vjust = 1, size = 7)) -> psave


psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, 
    linetype = 'dashed', hide.ns = T) +
  labs(title = title, subtitle = subtitle ) +
  labs(y = '%', x = '') -> psave

psave


ggsave(psave, 
  filename = 'settlement_ratios.png', path = ggsavepath, 
  width = 5, height = 3.5)

# *) MERGE PCT AND PLOT -------

cols <- c('hpf', 'pH', 'Design','pct')

competency %>% mutate(Design = 'Long-term Chronic stress', pct = value) %>% 
  filter(name %in% 'Aspecto.2') %>% select(cols) -> competency

# prop_df_longer # contiene los mismos datos que competency, so omitimos

surv_df_pct %>% mutate(hpf = 110, Design = 'Long-term Chronic stress') %>% select(cols) -> surv_df_pct
sett_df %>%  mutate(hpf = 26*24) %>% select(cols) -> sett_df

bind_data <- rbind(competency, surv_df_pct, sett_df) %>%   
  mutate(Design = ifelse(grepl('Chronic', Design), 'Chronic Stress', 'Acute Stress')) %>%
  mutate(Design = factor(Design, levels = c('Chronic Stress', 'Acute Stress'))) %>%
  mutate(time = round(hpf/24, digits = 2))

write_rds(bind_data,file = paste0(getwd(), '/bind_data_competencies.rds'))

#

bind_data <- read_rds(paste0(getwd(), '/bind_data_competencies.rds'))
bind_data %>% mutate(pH = recode_factor(pH, !!!level_key)) %>% 
  mutate(pH = factor(pH, levels = pHLevel)) -> bind_data
  
bind_data %>% 
  group_by(pH, hpf, Design) %>%
  rstatix::get_summary_stats(pct) %>% 
  mutate(ymin = mean-se, ymax = mean+se) %>%
  mutate(time = round(hpf/24, digits = 2)) -> df_longer_stats

df_longer_stats %>%
  ggplot(aes(x = as.factor(time), y = mean, color = pH, ymin = ymin, ymax = ymax, group = pH)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(width = 0.05, position = position_dodge(width = 0.2), size = 1) +
  geom_path(position = position_dodge(width = 0.2), size = 1) +
  facet_grid(~ Design, scales = 'free_x', space = 'free_x') +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("", values = pHpalette) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(strip.background = element_blank(),
    panel.border = element_blank(),
    legend.position = 'top') +
  labs(y = '% Competent Stage (Ind)',  x = 'Time (dpf)') -> psave

ggsave(psave, filename = 'competence_stages.png', path = ggsavepath, width = 7, height = 4)

# STATS W/ ANOVA AND TURKEY

bind_data %>%
  group_by(time, Design) %>%
  rstatix::anova_test(pct ~ pH) %>%
  # adjust_pvalue(method = "none") %>%
  add_significance("p") -> res_aov

res_aov

bind_data %>%
  group_by(time, Design) %>%
  # rstatix::pairwise_t_test(Count ~ pH, paired = F) 
  tukey_hsd(pct ~ pH) %>% # If ANOVA used as priori
  adjust_pvalue() %>%
  add_significance()-> stat_test


stat_test %>% filter(!p.adj.signif %in% 'ns') %>% select(Design, time, group1, group2, p.adj, p.adj.signif)

stat_test %>% add_xy_position(dodge = 0.2) -> stats

title <- get_pwc_label(stats)

# psave + 
#   ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
#     remove.bracket = F, tip.length = 0.01, linetype = 'dashed', hide.ns = T) +
#   labs(title = title)

# bind_data %>%
#   ggplot(aes(x = as.factor(hpf), y = pct, color = pH,  group = pH)) +
#   geom_point(position = position_dodge2(width = 0.2, reverse = T), size = 3, alpha = 0.4) +
#   stat_summary(fun=mean, geom ="line", aes(group = pH), size= 1, position = position_dodge2(width = 0.2, reverse = T)) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_color_viridis_d('', option = "plasma", end = .7) +
#   theme_classic(base_family = "GillSans", base_size = 10) +
#   theme(strip.background = element_rect(fill = 'white'),
#     panel.border = element_blank()) +
#   labs(y = '',  x = 'Time (hpf)') 


# COMPARE ACUTE VS CHRONIC DATA -----

df_longer_stats %>%
  filter(time == 26) %>%
  ggplot(aes(x = pH, y = mean, fill = Design, ymax = ymax, ymin = ymin, group = Design)) +
  geom_col(position = position_dodge(width = 1)) + # , color = 'black', size = 1
  geom_errorbar(width = 0.07, position = position_dodge(width = 1), size = 1, color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  scale_fill_manual('', values = c('red', 'blue')) +
  theme(strip.background = element_blank(),
    panel.border = element_blank(),
    legend.position = 'top') +
  labs(y = '% Competent Stage (Ind)',  x = '') -> psave

bind_data %>%
  filter(time == 26) %>%
  group_by(time, Design) %>%
  rstatix::anova_test(pct ~ pH) %>%
  # adjust_pvalue(method = "none") %>%
  add_significance("p") -> res_aov

res_aov

bind_data %>%
  filter(time == 26) %>%
  group_by(pH) %>%
  # rstatix::pairwise_t_test(Count ~ pH, paired = F) 
  tukey_hsd(pct ~ Design) %>% # If ANOVA used as priori
  adjust_pvalue() %>%
  add_significance()-> stat_test

stat_test

stat_test %>% filter(!p.adj.signif %in% 'ns')#%>% 
  # select(term, time, group1, group2, p.adj, p.adj.signif)

stat_test %>% add_xy_position(x = 'pH',dodge = 1) -> stats

title <- get_pwc_label(stats)

psave +
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif",
    remove.bracket = F, tip.length = 0.01, linetype = 'dashed', hide.ns = T) +
  labs(title = title)



ggsave(psave, filename = 'competence_stages_26d.png', path = ggsavepath, width = 4, height = 4)


# stats
# Logistic regression ----
library(tidymodels)

log_mod <- logistic_reg() %>%
  set_engine("glm") 

fit_log_mod <- log_mod %>%
  fit(pH ~ hpf*Design, data = bind_data)

fit_log_mod %>% tidy()

#

