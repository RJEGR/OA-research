# CLEAR OBJECT LIST AND IMAGE CANVAS   
rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)


library(tidyverse)
library(ggplot2)
library(rstatix)
library(flextable)

ggsavepath <- paste0(getwd(), '/Figures/')

pattern <- 'length_width_dataset.csv'

file <- list.files(path = getwd(), pattern = pattern, full.names = T)

# pHL <- c(8.0,7.8,7.6)

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",`8.0`= "#4575b4", `8`= "#4575b4")

df <- read_csv(file, show_col_types = FALSE) %>% 
  rename('Length' = `L1 (um)`,'Width' =  `L2 (um)`) %>%
  drop_na(Length, Width) %>%
  mutate_at(vars("Length","Width"), function(x) x / 4) %>% # divide by factor 4X to convienent units
  select(hpf, pH, group, Length, Width, Stage, Shell)

df %>% distinct(pH) %>% pull() -> pHLevel

df %>% distinct(hpf) %>% arrange(hpf) %>% pull() -> hpfL

df %>% mutate(pH = factor(pH, levels = pHLevel)) %>%
  mutate(hpf = factor(hpf, levels = hpfL))  -> df

pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]

# prepare index
df %>%
  group_by(hpf, pH) %>%
  mutate(Index = sqrt(Length*Width)) -> df

df %>%
  group_by(hpf, pH) %>%
  mutate(is.extreme = is_extreme(Index)) %>%
  filter(!is.extreme %in% TRUE) %>% 
  select(-is.extreme) -> df

# 0) Remove (index) outliers ----- 
nrow(df)

df %>% group_by(hpf, pH) %>% identify_outliers(Index) %>% group_by(hpf, pH) %>% tally(is.extreme)

# df_filtered <- df

df %>%
  group_by(hpf, pH) %>%
  mutate(is.extreme = is_extreme(Index)) %>%
  filter(!is.extreme %in% TRUE) %>%
  select(-is.extreme) -> df_filtered

#


df_filtered %>%
  ggplot(aes(Length, Width)) + # color = as.factor(pH),  group = pH
  facet_grid(~ pH ) +
  geom_point(alpha = 0.5, shape = 1) +
  # scale_color_viridis_d('', option = "plasma", end = .7) +
  geom_smooth(se = T, method = lm, color = 'blue', size = 0.7) +
  xlim(0,400) +
  ggpubr::stat_cor(method = "spearman", 
    cor.coef.name = "R", p.accuracy = 0.001, label.y = 250, family = "GillSans") +
  labs(x = expression("Length ("*mu*"m)"), y = expression("Width ("*mu*"m)")) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(strip.background = element_rect(fill = 'grey', color = 'white'),
    panel.border = element_blank()) -> psave


ggsave(psave, filename = 'Body_index_cor.png', path = ggsavepath, width = 5, height = 2)


df_filtered %>%
  ggplot(aes(Length, Width, color = as.factor(pH))) + 
  geom_smooth(se = F, method = lm, size = 1.3, alpha = 0.5) +
  xlim(0,400) +
  labs(x = expression("Length ("*mu*"m)"), y = expression("Width ("*mu*"m)")) +
  theme_classic(base_family = "GillSans", base_size = 16) +
  theme(strip.background = element_rect(fill = 'grey', color = 'white'),
    panel.border = element_blank()) +
  scale_color_manual("", values = pHpalette) +
  theme(legend.position = 'none')

df_filtered %>%
  group_by(pH) %>%
  cor_test(Length,Width, method = 'spearman') %>%
  add_significance("p") %>%
  select(pH, var1, var2, cor, method, p.signif) %>%
  flextable() %>% 
  # bold(~ p < 0.05, ~ p, bold = TRUE) %>%
  autofit(add_w = 0, add_h = 0) %>%
  align(align = "center")

# df %>%
#   ggplot(aes(Length, Width, color = as.factor(hpf))) + # color = as.factor(pH),  
#   facet_grid(~ pH ) +
#   geom_point(alpha = 0.5, shape = 1) +
#   labs(x = expression("Length ("*mu*"m)"), y = expression("Width ("*mu*"m)")) +
#   theme_classic(base_family = "GillSans", base_size = 14) +
#   theme(strip.background = element_rect(fill = 'white', color = 'white'),
#     panel.border = element_blank())



df_filtered %>%
  # filter(hpf %in% '24') %>%
  group_by(pH, Stage) %>%
  rstatix::get_summary_stats(type = "quantile") %>%
  flextable() %>% 
  # bold(~ p < 0.05, ~ p, bold = TRUE) %>%
  autofit(add_w = 0, add_h = 0) %>%
  align(align = "center")

# 1) test if gaussianity (FALSE) ----

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
  add_significance("p") -> prior.stats

# we found difference between treatment in all/any of the development times

# 4) Statistical posteriori test ----

# En vista de que encontramos outliers y normalidad, pero no homocelasticidad, evaluamos a traves de un test de wilcoxon

df_filtered %>%
  group_by(hpf) %>%
  pairwise_wilcox_test(Index ~ pH,  conf.level = 0.95) %>%
  adjust_pvalue(method = "fdr") %>%
  add_significance() -> post.test

post.test %>% add_xy_position(x = "pH") -> stats

title <- get_pwc_label(stats, "text")

# subtitle <- get_test_label(prior.stats, detailed = F)

subtitle <- get_description(prior.stats)

caption <- paste0(subtitle,"; ", title)

df_filtered %>%
  mutate(pH = as.factor(pH)) %>%
  # mutate(vars = factor(vars, levels = varsf)) %>%
  ggplot(aes(y = Index, x = pH,  group = pH, color = pH)) +
  facet_wrap(~ hpf,nrow = 1) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width = 0.3, outlier.alpha = 0) +
  stat_summary(fun=median, geom ="line", aes(group = 2), size= 0.5, color = 'blue') +
  scale_y_continuous(n.breaks = 5) +
  labs(y = 'Growth Index') +
  scale_color_manual("", values = pHpalette) -> psave

df_filtered %>%
  select(-Length, -Width) %>%
  rstatix::get_summary_stats(type = "quantile") %>%
  ggplot(aes(x = pH, y = `50%`, color = pH, group = pH)) +
  facet_wrap(~ hpf,nrow = 1) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 12) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual("", values = pHpalette) +
  labs(y = 'Growth Index') -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01,  hide.ns = T, size = 2.5) +
  labs(caption = caption) -> psave


psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> psave

ggsave(psave, filename = 'Body_index.png', path = ggsavepath, 
  width = 5, height = 3.5)

#  as line, only upper quantiles

df_filtered %>%
  group_by(hpf, pH) %>%
  rstatix::get_summary_stats(type = "quantile") %>%
  ggplot(aes(x = hpf, y = `50%`, color = pH, group = pH)) +
  facet_grid(~ variable) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.1, position = position_dodge(width = 0.3)) +
  geom_path(position = position_dodge(width = 0.3), size = 1) +
  scale_color_manual("", values = pHpalette) +
  labs(y = 'Quantiles (25,50,75)') -> ps

ps + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> ps

  

ggsave(ps, filename = 'Body_index_facet.png', path = ggsavepath, 
  width = 5.5, height = 3)

# 5) geometric morphometric shape analyses: ----
vars <- c('Length', 'Width', 'Index','Area')

df_filtered %>% 
  mutate(Area = pi*(Length * Width)) %>%
  # group_by(pH) %>%
  # rstatix::cor_test(vars = 'Area', method = 'spearman')
  ungroup() %>%
  rstatix::cor_mat(vars = vars, method = 'spearman')


df_filtered %>% 
  mutate(Area = sqrt(pi*(Length * Width))) %>%
  # group_by(pH) %>%
  select(Area) %>%
  rstatix::get_summary_stats(type = 'mean_sd')

# Area de un figura forma de huevo =
# D = r1 x r2 (ie. largo x ancho o radio mayor x radio menor)
# area = D*pi

vars <- c('Length', 'Width', 'Index','Area','h','b')

# a 24 hpf, tanto lado 1 y lado 2 son de una estructura de apariencia distinta al resto (ie. veligery trocofora)

df_filtered %>% 
  mutate(
    # P = pi * ((Length/2) + (Width/2)),
    Area = pi * ((Length/2) * (Width/2)),
    b = Length-Width, # base del triangulo equilatero
    a = Width/2, # altura del mismo
    h = sqrt((a)^2 + b^2), # pendiente (hipotenusa) del t. eq.
    y = a + b) %>%
  # ungroup() %>% rstatix::cor_mat(vars = vars, method = 'spearman')
  select(h) %>%
  rstatix::get_summary_stats(type = 'mean_sd') %>%
  ggplot(aes(y = mean, x = pH)) +
  geom_col() +
  facet_grid(~ hpf)

# the good one

df_filtered %>% 
  mutate(
    b = Length-Width, # base del triangulo equilatero
    a = Width/2, # altura del mismo
    h = sqrt((a)^2 + b^2)) %>%
  filter(!hpf %in% '24') -> tbl


tbl %>% group_by(hpf, pH) %>% 
  identify_outliers(Index) %>% 
  group_by(hpf, pH) %>% tally(is.outlier)

tbl %>%
  group_by(hpf, pH) %>%
  mutate(is.outlier = is_outlier(h)) %>%
  filter(!is.outlier %in% TRUE) %>%
  select(-is.outlier) -> tbl

# 5.1) GAuss (T) ----

tbl %>% 
  group_by(hpf, pH) %>% 
  rstatix::shapiro_test(h) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE)) 

# so
tbl %>%
  select(h) %>%
  rstatix::get_summary_stats(type = 'mean_sd') %>%
  ggplot(aes(y = mean, x = pH)) +
  geom_col() +
  facet_grid(~ hpf)

# 5.2) Homocelasticidad (PARTIAL T) ----

tbl %>%
  group_by(pH) %>%
  levene_test(h ~ as.factor(hpf)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# 5.3) Statistical priori test----

# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation where there are more than two groups. It’s recommended when the assumptions of one-way ANOVA test are not met. 

tbl %>% 
  group_by(hpf) %>%
  rstatix::kruskal_test(h ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> prior.stats

#
# we found difference between treatment in all/any of the development times

# 5.4) Statistical posteriori test ----

# En vista de que encontramos outliers y normalidad, pero no homocelasticidad, evaluamos a traves de un test de wilcoxon

tbl %>%
  group_by(hpf) %>%
  # pairwise_wilcox_test(h ~ pH,  conf.level = 0.95) %>%
  pairwise_t_test(h ~ pH) %>%
  adjust_pvalue(method = "fdr") %>%
  add_significance() -> post.test

post.test %>% add_xy_position(x = "pH") -> stats

title <- get_pwc_label(stats, "text")

# subtitle <- get_test_label(prior.stats, detailed = F)

subtitle <- get_description(prior.stats)

caption <- paste0(subtitle,"; ", title)

tbl %>% 
  ggplot(aes(x = pH, y = h, fill = pH, color = pH)) +
  facet_wrap(~ hpf,nrow = 1) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_jitter(width=0.1,alpha=0.2, height = 0.1, size = 0.7, shape = 1) +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot(aes(fill = pH), width = 0.3, outlier.alpha = 0, coef = 0) +
  labs(y = 'Shell Growth') +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01,  hide.ns = T, size = 2.5) +
  labs(caption = caption) -> psave


psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> psave

ggsave(psave, filename = 'transition_shell_growth.png', path = ggsavepath, 
  width = 5, height = 3.5)

# 6) TCD ----
# TCD (μm/día) = (LCi - LCf )/t; donde: LCf y LCi corresponden a la longitud final e inicial de las conchas de las larvas, y t es el tiempo entre las mediciones 

tbl %>% 
  mutate(P = pi * ((Length/2) + (Width/2))) %>%
  group_by(hpf, pH) %>% 
  rstatix::shapiro_test(P) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE)) 

show <- '50%'

df_filtered %>%
  mutate(P = pi * ((Length/2) + (Width/2))) %>%
  select(Index) %>%
  rstatix::get_summary_stats(type = 'quantile', show = show) %>%
  # rstatix::get_summary_stats(type = 'mean_sd', show = 'mean') %>%
  select(-n, -variable) %>%
  pivot_wider(names_from = hpf, values_from = show) %>%
  mutate(TDC = abs(`24` - `108`) / (108-24) ) %>% view()
  

library(tidymodels)

lm_mod <- linear_reg() %>%
  set_engine("lm")

formula <- formula( 'Area ~ pH:hpf')

lm_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(formula, data = df_filtered)

library(dotwhisker)

lm_fit %>% tidy() %>%
    dwplot(dot_args = list(size = 2, color = "black"),
      whisker_args = list(color = "black"),
      vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) + 
  xlab("b Coefficient")

# The beta estimate (or beta coeff) determines the direction and strength of the relationship between the two variables.
# A beta of zero suggests there is no association between the two variables. However, if the beta value is positive, the relationship is positive. If the value is negative, the relationship is negative. Further, the larger the number, the bigger the effect is.

# df_filtered %>%
#   group_by(hpf, pH) %>%
#   rstatix::get_summary_stats(type = "mean_se") %>%
  # mutate(ymax = mean+se, ymin = mean-se) %>%
  # ggplot(aes(x = hpf, y = mean, color = pH, group = pH)) +
  # facet_grid(~ variable) +
  # geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  # theme_bw(base_family = "GillSans", base_size = 14) +
  # geom_errorbar(aes(ymin = ymin, ymax = ymax),
  #   width = 0.1, position = position_dodge(width = 0.3)) +
  # geom_path(position = position_dodge(width = 0.3), size = 1) +
  # scale_color_manual("", values = pHpalette) +
  # labs(y = 'mean_se')

# Throchophore shell ----

df %>%
  filter(hpf %in% '24') %>%
  count(Stage) %>%
  group_by(pH) %>%
  mutate(n = n/sum(n), n = n*100) %>%
  mutate(n = paste0(round(n), ' %')) %>%
  pivot_wider(values_from = n, names_from = Stage) %>%
  flextable() %>% 
  # bold(~ p < 0.05, ~ p, bold = TRUE) %>%
  autofit(add_w = 0, add_h = 0) %>%
  align(align = "center")

# Normal shelled
# Abnormal shelled
# Unshelled

level_key <- c("nrs" = "Normal shelled", "ans" = "Abnormal shelled", "uns" = "Unshelled")

caption <- "Trochophore larvae scored as one of possible morphological groups"



df %>%
  filter(hpf %in% '24') %>%
  count(Shell) %>%
  mutate(Shell = recode_factor(Shell, !!!level_key)) %>%
  group_by(pH) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(y = pct, x = Shell, fill = pH)) +
  geom_col(position = "dodge2") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("", values = pHpalette) +
  labs(y = '%', x = '', caption = caption) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(panel.border = element_blank(), legend.position = 'top') -> ps


ggsave(ps, filename = 'throchophore_score.png', path = ggsavepath, 
  width = 4.5, height = 3)  

df %>%
  filter(hpf %in% '24') %>%
  group_by(pH, Stage) %>%
  rstatix::get_summary_stats(type = "quantile") %>%
  ggplot(aes(x = pH, y = `50%`, color = pH, group = pH)) +
  facet_grid(Stage ~ variable) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.1, position = position_dodge(width = 0.3)) +
  geom_path(position = position_dodge(width = 0.3), size = 1) +
  scale_color_manual("", values = pHpalette)
  
  
df %>%
  filter(hpf %in% '24') %>%
  count(pH) %>%
  ungroup() %>%
  # mutate(Shell = recode_factor(Shell, !!!level_key)) %>%
  # arrange(Shell) %>%
  select(pH, n) %>%
  flextable() %>% 
  # bold(~ p < 0.05, ~ p, bold = TRUE) %>%
  autofit(add_w = 0, add_h = 0) %>%
  align(align = "center") %>%
  merge_v() 
  # bg(~ pH == '8', 
  #   j = c('pH','n'), 
  #   bg = "#4575b4", part = "body") %>%
  # bg(~ pH == '7.8', 
  #   j = c('pH','n'), 
  #   bg = "#abdda4", part = "body") %>%
  # bg(~ pH == '7.6', 
  #   j = c('pH','n'), 
  #   bg = "#d73027", part = "body")



# data inputs for model ----

df_filtered %>%
  select(hpf, pH, Index) %>%
  group_by(hpf, pH) %>%
  rstatix::get_summary_stats(type = "quantile", probs = 0.5) %>%
  select(-n, - variable) %>%
  pivot_wider(names_from = pH, values_from = `50%`)

df_filtered %>%
  group_by(pH, hpf) %>%
  cor_test(Length,Width, method = 'spearman') %>%
  add_significance("p") %>%
  select(hpf, pH, cor) %>%
  pivot_wider(names_from = pH, values_from = cor)


df %>%
  filter(hpf %in% '24') %>%
  count(Shell) %>%
  mutate(Shell = recode_factor(Shell, !!!level_key)) %>%
  group_by(pH) %>%
  mutate(pct = n/sum(n)) %>% 
  filter(Shell %in% "Abnormal shelled") %>%
  select(hpf, pH, pct) %>%
  pivot_wider(names_from = pH, values_from = pct)
  
# Cluster analysis
# Based on Bayesian information Criterion (BIC) search (support) the presence of k size groups among the longged measurements. Coutts, F. J., et al (2018). Evidence of sensory-driven behavior in the Ediacaran organism Parvancorina: Implications and autecological interpretations.

# https://datasciencebook.ca/clustering.html#

library(tidymodels)

fit <- lm(Length ~ Width + hpf:pH, df_filtered)

tidy(fit)

glance(fit)

augment(fit) %>%
  ggplot(aes(Length, Width, color = pH)) +
  geom_point() +
  geom_line(aes(y = .fitted)) 
  # facet_grid(~ hpf)


# library(mclust)
library(mclust)
M <- df_filtered %>% ungroup() %>% select(Length,Width)

d_clust <- Mclust(as.matrix(log(M)), G=1:7, 
  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

# or

standardized_data <- M %>%
  mutate(across(everything(), scale))

df_clust <- kmeans(standardized_data, centers = 3)
df_clust


library(broom)

clustered_data <- augment(df_clust, standardized_data)

cluster_plot <- ggplot(clustered_data,
  aes(x = Length, 
    y = Width, 
    color = .cluster), 
  size = 2) +
  geom_point() +
  labs(x = "Length (standardized)", 
    y = "Width (standardized)", 
    color = "Cluster") + 
  scale_color_manual(values = c("dodgerblue3",
    "darkorange3",  
    "goldenrod1")) + 
  theme(text = element_text(size = 12))

cluster_plot

glance(df_clust)


clust_ks <- tibble(k = 1:9)
clust_ks <- tibble(k = 1:9) %>%
  rowwise() %>%
  mutate(clusts = list(kmeans(standardized_data, k)))

clust_ks %>%
  pull(clusts) %>%
  pluck(1)


clust_ks <- tibble(k = 1:9) %>%
  rowwise() %>%
  mutate(clusts = list(kmeans(standardized_data, k, nstart = 10, k)),
    glanced = list(glance(clusts)))


clustering_statistics <- clust_ks %>%
  unnest(glanced)

clustering_statistics

elbow_plot <- ggplot(clustering_statistics, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  xlab("K") +
  ylab("Total within-cluster sum of squares") +
  scale_x_continuous(breaks = 1:9) + 
  theme(text = element_text(size = 12))

elbow_plot

# ggord ----
library(ggord)
# principal components analysis with the iris data set
# prcomp

df_filtered %>% 
  sample_n(46, replace = TRUE) %>%
  mutate(n = 1:46) %>%
  ungroup() %>%
  mutate(n = paste0(n, "-", hpf)) %>%
  select(n, pH, Index) %>%
  # mutate(Index = log10(Index)) %>%
  pivot_wider(names_from = pH, values_from = Index) %>%
  unnest(cols = c(`8`, `7.8`, `7.6`)) %>%
  separate(n, into = c('id', 'hpf'), sep = '-')-> M

ord <- prcomp(M[, 3:5], scale. = T)

p <- ggord(ord,M$hpf, ellipse = FALSE, hull = TRUE, facet = F)
p

library(factoextra)
