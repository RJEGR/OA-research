# BIRREFRYGENCY
# Integrated Density - Calculates and displays two values: "IntDen" (the product of Area and Mean Gray Value) and "RawIntDen" (the sum of the values of the pixels in the image or selection)

# Area - Area of selection in square pixels (Pixel^2). Area is in calibrated units, such as square millimeters, if Analyze>Set Scale was used to spatially calibrate the image.

# Mean Gray Value - Average gray value within the selection. This is the sum of the gray values of all the pixels in the selection divided by the number of pixels. Reported in calibrated units (e.g., optical density) if Analyze>Calibrate was used to calibrate the image. For RGB images, the mean is calulated by converting each pixel to grayscale using the formula gray=0.299red+0.587green+0.114blue or the formula gray=(red+green+blue)/3 if "Unweighted RGB to Grayscale Conversion" is checked in Edit>Options>Conversions. Standard Deviation - Standard deviation of the gray values used to generate the mean gray value.

# Area Fraction - The percentage of pixels in the image or selection that have been highlighted in red using Image>Adjust>Threshold. For non-thresholded images, the percentage of non-zero pixels. (ie. mean / max)

# 1 micron in  x pixels
# pixel aspect ratio of the imageJ is not scaled (ie. Pixel aspect ratio 1.0)

# pix <- 0.0037823997617386 # depend on the screen space (NxN)

# Clean canvas and memory ----

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

pattern <- 'BIRREFRIGENCY.csv'

file <- list.files(path = getwd(), pattern = pattern, full.names = T)


df <- read_csv(file, show_col_types = FALSE) %>% 
  separate(g, into = c('hpf', 'pH'), sep = '_') %>%
  mutate(pH = recode(pH, '8.0' = '8'))
  # mutate(Frac = Mean/Max)

df %>% distinct(pH) %>% pull() -> pHLevel

hpfL <- c(48, 60, 108)

df %>% mutate(pH = factor(pH, levels = pHLevel)) %>%
  mutate(hpf = factor(hpf, levels = hpfL))  -> df


pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]

# EDA ----

df %>% group_by(hpf, pH) %>% tally()


df %>% group_by(hpf, pH) %>% select(Area, Frac, Mean, RawIntDen) %>% 
  mutate(Area = sqrt(Area)) %>%
  rstatix::get_summary_stats(type = "quantile") %>%
  ggplot(aes(x = pH, y = `50%`, color = pH, group = pH)) +
  facet_wrap(hpf ~ variable, scales = 'free') +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 12) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual("", values = pHpalette)

# RawIntDen, Area and y (sqrt(A)) are positive correlated

df %>% group_by(hpf, pH) %>% select(Area, Frac, Mean, RawIntDen) %>% 
  mutate(y = sqrt(Area)) %>%
  ungroup() %>%
  rstatix::cor_mat(vars = c('y','Area', 'Frac', 'Mean', 'RawIntDen'), method = 'spearman') %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)
  

# 0) Remove (index) outliers ----- 
nrow(df)

df %>% group_by(hpf, pH) %>% identify_outliers(Area) %>% group_by(hpf, pH) %>% tally(is.outlier)

  

# df_filtered <- df

df %>%
  group_by(hpf, pH) %>%
  mutate(outlier = is_outlier(Area)) %>%
  filter(!outlier %in% TRUE) %>%
  select(-outlier) %>%
  mutate(Area = sqrt(Area))-> df_filtered

write_rds(df_filtered, paste0(getwd(), '/birefrigence.rds'))

# 1) test if gaussianity (PARTIAL FALSE) ----

df_filtered %>% 
  group_by(hpf, pH) %>% 
  # ungroup() # caldo de datos es falso
  rstatix::shapiro_test(Area) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE)) 

# 2) Homocelasticidad (TRUE) ----
# Before doing parametric or not test, lets to analyze homogeneity of variance across experimental Levene’s test:

df_filtered %>%
  group_by(hpf) %>%
  levene_test(Mean ~ as.factor(pH)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# Convert pixels to microns
# 3) Statistical priori test----

# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation where there are more than two groups. It’s recommended when the assumptions of one-way ANOVA test are not met. 

df_filtered %>% 
  group_by(hpf) %>%
  rstatix::kruskal_test(Area ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> prior.stats

# we found difference between treatment in all/any of the development times

# 4) Statistical posteriori test ----

# En vista de que encontramos outliers y normalidad, pero no homocelasticidad, evaluamos a traves de un test de wilcoxon

df_filtered %>%
  group_by(hpf) %>%
  pairwise_wilcox_test(Area ~ pH,  conf.level = 0.95, ref.group = '8.0') %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() -> post.test


post.test %>% add_xy_position(x = "pH") -> stats

title <- get_pwc_label(stats, "text")

subtitle <- get_description(prior.stats)

caption <- paste0(subtitle,"; ", title)

df_filtered %>%  tally() %>% mutate(label = paste0(pH))

df_filtered %>%
  # mutate(vars = factor(vars, levels = varsf)) %>%
  ggplot(aes(y = Area, x = pH,  group = pH, color = pH)) +
  facet_wrap(~ hpf, nrow = 1) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_jitter(width=0.1,alpha=0.2, height = 0.1, size = 0.7, shape = 1) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(aes(fill = pH), width = 0.3, outlier.alpha = 0) +
  # stat_summary(fun=median, geom ="line", aes(group = 2), size= 0.5, color = 'blue') +
  scale_y_continuous(n.breaks = 5) +
  labs(y = expression("Birefringence (Pixels)")) + # if not sqrt(Area) use "Birefringence (Pixels"^2*")"
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) -> psave


# How to add n of values
# psave +  geom_text(stat = 'summary', vjust = -1,
#   color = 'black', family = "GillSans", size = 2.7,
#   fun.data = function(d) c(
#   y = quantile(d, 0.75, names = F) + 1.5 * IQR(d),
#   label = length(d)))

# # 

psave +  geom_text(stat = 'summary', vjust = -1,
  color = 'black', family = "GillSans", size = 2.7,
  fun.data = function(d) c(
  y = 1, label = length(d)))

# # 
# df_filtered %>%
#   select(Area) %>%
#   rstatix::get_summary_stats(type = "quantile") %>%
#   ggplot(aes(x = pH, y = `50%`, color = pH, group = pH)) +
#   facet_wrap(~ hpf,nrow = 1) +
#   geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
#   theme_bw(base_family = "GillSans", base_size = 12) +
#   geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
#     width = 0.2, position = position_dodge(width = 0.3)) +
#   scale_color_manual("", values = pHpalette) +
#   labs(y = expression("P"^2)) -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01,  hide.ns = T, size = 2.5) +
  labs(caption = caption) -> psave


psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> psave

# psave + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) -> psave


ggsave(psave, filename = 'birrefrigency.png', path = ggsavepath, 
  width = 5.7, height = 3.5)


# hten ----



df_filtered %>%
  group_by(hpf, pH) %>%
  rstatix::get_summary_stats(Area) %>%
  select(hpf, pH, n, mean, median, se, iqr, mad) -> summary_stats

stats %>% select(hpf, group2, p.adj.signif) %>% 
  rename('pH' = 'group2') %>%
  right_join(summary_stats) %>% # view()
  mutate(ymax = mean+se, ymin = mean-se) %>%
  mutate(pH = factor(pH, levels = pHLevel)) %>%
  # filter(hpf %in% '108') %>%
  ggplot(aes(x = pH, y = mean, color = pH, group = pH)) +
  facet_grid(~ hpf) +
  geom_text(aes(y = ymax + 10, label= p.adj.signif),
    size = 2.5, position = position_dodge(width = 0.3),
    family = 'GillSans',fontface = "bold",
    vjust= 0, color="black") +
  geom_point(size = 3, alpha = 0.5, position = position_dodge(width = 0.3)) +
  theme_classic(base_family = "GillSans", base_size = 16) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), 
    width = 0.1, position = position_dodge(width = 0.3)) +
  scale_color_manual("", values = pHpalette) +
  labs(y = 'Birefrigence (Pixels)') +
  # ylim(170, 250) +
  theme(strip.background = element_rect(fill = 'grey', color = 'white'),
    panel.border = element_blank(), legend.position = 'top') -> ps2

ggsave(ps2, filename = 'birefrigency_2.png', path = ggsavepath, 
  width = 3.6, height = 2.7)


# write_rds(df_filtered %>%, )

# TEsts

df_filtered %>%
  mutate(x = Area/Mean, y = sqrt(Area)) %>%
  ggplot(aes(Area, y)) +
  geom_jitter() 
 #

df_filtered %>%
  ggplot(aes(Frac, fill = pH)) +
  geom_histogram(bins = 100) +
  scale_fill_manual("", values = pHpalette) +
  facet_grid(~ pH)

df_filtered %>%
  ggplot(aes(RawIntDen, color = pH)) +
  geom_density() +
  scale_color_manual("", values = pHpalette) 
  # facet_grid(~ pH)

# Solo a las 108 horas se ve un decremento, pero a 60 resulta que los pixeles fueron mas completos en las meuestras de pH hacido, . Esto porque el n es muy pequeno, entonces los incremente

df_filtered %>%
  mutate(mineralized = 'Less') %>%
  mutate(mineralized = ifelse(between(Frac, 90, 100), 'Fully', mineralized)) %>%
  mutate(mineralized = ifelse(between(Frac, 70, 90), 'Partially', mineralized)) %>%
  count(mineralized) %>% mutate(pct = n / sum(n)) %>% 
  #summarise(sum(pct))
  ggplot(aes(y = pct, x = mineralized, fill = pH)) +
  geom_col(position = "dodge2") +
  facet_grid(~ hpf) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("", values = pHpalette) +
  labs(y = '%', x = 'Mineralized') +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(panel.border = element_blank(), legend.position = 'top') 

# 

vars <- ToothGrowth %>% get_summary_stats() %>% names()
vars <- vars[-c(1,2)]

df_filtered %>% group_by(hpf, pH) %>% 
  select(Area) %>% 
  # rstatix::get_summary_stats(type = "quantile") %>% view()
  rstatix::get_summary_stats(type = "full") %>% view()
rstatix::cor_mat(vars = vars, method = 'spearman') %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)
