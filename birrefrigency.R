# BIRREFRYGENCY
# Integrated Density - Calculates and displays two values: "IntDen" (the product of Area and Mean Gray Value) and "RawIntDen" (the sum of the values of the pixels in the image or selection)

# Area - Area of selection in square pixels (Pixel^2). Area is in calibrated units, such as square millimeters, if Analyze>Set Scale was used to spatially calibrate the image.

# Mean Gray Value - Average gray value within the selection. This is the sum of the gray values of all the pixels in the selection divided by the number of pixels. Reported in calibrated units (e.g., optical density) if Analyze>Calibrate was used to calibrate the image. For RGB images, the mean is calulated by converting each pixel to grayscale using the formula gray=0.299red+0.587green+0.114blue or the formula gray=(red+green+blue)/3 if "Unweighted RGB to Grayscale Conversion" is checked in Edit>Options>Conversions. Standard Deviation - Standard deviation of the gray values used to generate the mean gray value.

# Area Fraction - The percentage of pixels in the image or selection that have been highlighted in red using Image>Adjust>Threshold. For non-thresholded images, the percentage of non-zero pixels. (ie. mean / max)

# 1 micron in  x pixels
# pixel aspect ratio of the imageJ is not scaled (ie. Pixel aspect ratio 1.0)

# pix <- 0.0037823997617386 # depend on the screen space (NxN)

rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)

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
  separate(g, into = c('hpf', 'pH'), sep = '_') #%>%
  # mutate(Frac = Mean/Max)

df %>% distinct(pH) %>% pull() -> pHLevel

# df %>% distinct(hpf) %>% arrange(hpf) %>% pull() -> hpfL

df %>% mutate(pH = factor(pH, levels = pHLevel)) -> df
  # mutate(hpf = factor(hpf, levels = hpfL))  -> df


pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]

# EDA ----

df %>% group_by(hpf, pH) %>% select(Area, Frac, Mean, RawIntDen) %>% 
  mutate(Area = sqrt(Area)) %>%
  rstatix::get_summary_stats(type = "quantile") %>%
  ggplot(aes(x = pH, y = `50%`, color = pH, group = pH)) +
  facet_wrap(hpf ~ variable,nrow = 1, scales = 'free_y') +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 12) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual("", values = pHpalette)

# 0) Remove (index) outliers ----- 
nrow(df)

df %>% group_by(hpf, pH) %>% identify_outliers(Area) %>% group_by(hpf, pH) %>% tally(is.outlier)

# df_filtered <- df

df %>%
  group_by(hpf, pH) %>%
  mutate(outlier = is_outlier(Area)) %>%
  filter(!outlier %in% TRUE) %>%
  select(-outlier) -> df_filtered


# 1) test if gaussianity (FALSE) ----

df_filtered %>% 
  group_by(hpf, pH) %>% 
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
  pairwise_wilcox_test(Area ~ pH,  conf.level = 0.95) %>%
  adjust_pvalue(method = "fdr") %>%
  add_significance() -> post.test


post.test %>% add_xy_position(x = "pH") -> stats

title <- get_pwc_label(stats, "text")

subtitle <- get_description(prior.stats)

caption <- paste0(subtitle,"; ", title)

df_filtered %>%
  # mutate(vars = factor(vars, levels = varsf)) %>%
  ggplot(aes(y = Area, x = pH,  group = pH, color = pH)) +
  facet_wrap(~ hpf, nrow = 1) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width = 0.3, outlier.alpha = 0) +
  # stat_summary(fun=median, geom ="line", aes(group = 2), size= 0.5, color = 'blue') +
  scale_y_continuous(n.breaks = 5) +
  labs(y = expression("Birefringence (Pixels"^2*")")) +
  scale_color_manual("", values = pHpalette) -> psave

# 
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

psave + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) -> psave

ggsave(psave, filename = 'birrefrigency.png', path = ggsavepath, 
  width = 3.5, height = 4.5)

#

df_filtered %>%
  ggplot(aes(Frac, fill = pH)) +
  geom_histogram(bins = 100) +
  scale_fill_manual("", values = pHpalette) +
  facet_grid(~ pH)

# Solo a las 60 horas se ve un decremento, pero a 108 resulta que los pixeles fueron mas completos en las meuestras de pH hacido, . Esto porque el n es muy pequeno, 
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
