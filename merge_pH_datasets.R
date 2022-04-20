# Ricardo Gomez-Reyes April 2022
# Acidification experiment

# Read in three steps all the datasets from the pH measures within the OA experiment

# 1) Embryo to Larva: From Febraury 04 to 09 (pH_embryoLarvae_longer_set.rds)
# 2) Larva to settlement: From February 10 to March 07 (pH_larvaeSettlement_longer_set.rds)
# 3) Control measures: From February 28 to March 05 (pH_control_longer_set.rds)



rm(list = ls())

options(stringsAsFactors = FALSE)

library(tidyverse)

# namesL <- c('Canal-1', 'Canal-2', 'Canal-3', 'Canal-4')
# 
# getPalette <- RColorBrewer::brewer.pal(4, 'Paired')
# 
# getPalette <- structure(getPalette, names = namesL) 


ggsavepath <- paste0(getwd(), '/Figures')

source(paste0(getwd(), "/stats.R"))

files <- list.files(path = getwd(), pattern = 'rds$', full.names = T)

rds <- lapply(files, read_rds)

df_longer <- do.call(rbind, rds)

rm(rds)

# Evaluamos outliers ----

df_longer %>%
  group_by_at(c('date', 'name')) %>%
  mutate(z = zfun(Obs)) -> odf

ocount(df = df_longer, zscore = 3, cols = c('date'))

l <- list()

for(i in seq(1, 5, by = 1)) { l[[i]] <- ocount(df_longer, i, c('name')) }


subtitle <- expression('Outlier detection by the z Standard Deviations from the Mean')
caption <- expression('z = (x - mean(x)) / sd(x)')


do.call(rbind,l) %>%
  ungroup() %>%
  mutate(total = N+Y, pct = 100*(N/total)) %>%
  # filter(name %in% c('Canal-3', 'Canal-4')) %>%
  ggplot(aes(x = g, y = pct, color = name)) +
  geom_line(aes(group = name), orientation = "x", linetype = 'dashed') +
  geom_point() +
  labs(x ='Z score', y = '% data not removed', caption = caption) +
  # scale_color_manual(values = getPalette) +
  theme_bw(base_size = 10, base_family = "GillSans") 
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 7),
    strip.background = element_blank(),
    panel.grid = element_line(size = rel(0.5)),
    panel.grid.minor = element_line(size = rel(0)),
    panel.border = element_blank()) +
  guides(colour = guide_legend("")) -> psave

psave

# filtering datasets ----
library(rstatix)

# df_longer %>% group_by(name) %>% rstatix::identify_outliers(Obs, coef = 3)
df_longer %>% group_by(name, date) %>% mutate(is.outlier = is_outlier(Obs), is.extreme = is_extreme(Obs)) -> df_longer

df_longer %>% group_by(name, is.extreme) %>% tally()
df_longer %>% group_by(name, is.outlier) %>% tally()

# df_longer %>% ggplot(aes(Obs, fill = name)) + geom_histogram() + facet_grid(~is.outlier)
# df_longer %>% ggplot(aes(Obs, fill = name)) + geom_histogram() + facet_grid(~is.extreme)

# df_longer %>%
#   group_by(name, date) %>%
#   summarise(qqfun(Obs)) %>%
#   mutate(z = zfun(y)) %>%
#   mutate(z = ifelse(abs(z) >= 3, NA, z)) %>%
#   drop_na(z) -> df_summ

# Prepare filtered dataset
df_longer %>% filter(is.outlier == 'FALSE') -> df_longer

df_longer %>%
  group_by(date, name) %>%
  mutate(value = Obs) %>%
  summarise(
    a = mean(value), sd = sd(value), IC = IC(value),
    upper = a+IC, lower = a-IC,
    zupper = a+(3*sd), zlower = a-(3*sd), n = n()) -> out_stats

saveRDS(out_stats, df_longer, file = paste0(getwd(), '/pH_dataset.Rdata'))

# previz (omit) -----

distinct(out_stats, date) %>% pull(date) -> recode_date

struc_group <- c('Embryo', rep('Larvae', 5), rep('Settlement', 26))

level_key <- structure(struc_group, names = recode_date)

out_stats %>% mutate(g = recode_factor(date, !!!level_key)) -> out_stats

Labels <- c('Control', 'Experimental I', 'Experimental II')
  
out_stats %>%
  mutate(date = factor(date, levels = recode_date)) %>%
  ggplot(aes(y = a, x = date, color = name)) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) +
  # geom_point(aes(size = n), alpha = 0.5) +
  geom_line(aes(group = name), orientation = "x", linetype = 'dashed') +
  # scale_color_manual('', values = getPalette,
    # limits = namesL[-1], labels = Labels) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_y_continuous(n.breaks = 10) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  # facet_grid(.~g, scales = 'free_x', space = 'free', switch = 'x') +
  # theme(legend.position = 'top',
  #   panel.border = element_blank(), 
  #   panel.spacing.x = unit(0,"line"),
  #   strip.text = element_text(size = 7),
  #   strip.background = element_rect(linetype ='dashed', colour="black",fill="white"),
  #   axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'top',
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()) +
  labs(y = 'Average pH', x = '') -> psave

# Anadimos un bottom plot del conteo de datos que se usaron por dia

out_stats %>% 
  group_by(g, date) %>%
  summarise(l = length(name), N = sum(n), n = N/l) %>%
  # mutate(pct = n / sum(n)) %>%
  mutate(date = factor(date, levels = recode_date)) %>%
  ggplot() +
  geom_point(aes(size = n, x = date, y = as.factor(1))) +
  # geom_col(aes(y = n, x = date), width = 0.5) +
  # scale_fill_manual('', values = getPalette,
    # limits = namesL[-1], labels = Labels) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  facet_grid(.~g, scales = 'free_x', space = 'free', switch = 'x') +
  theme(legend.position = 'bottom',
    axis.line.x = element_blank(),
    panel.border = element_blank(), 
    panel.spacing.x = unit(0,"line"),
    panel.spacing.y = unit(0,"line"),
    strip.text = element_text(size = 7),
    strip.background = element_rect(size = 0.7, linetype = 'dashed', 
      colour="black",fill="white"),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1)) +
  # theme(legend.position = 'none',
  #   axis.text.x = element_blank(),
  #   axis.ticks.x = element_blank()) +
  labs(y = 'Size', x = '') -> psave2
  # scale_y_reverse() 

library(patchwork) 

psave/psave2 + patchwork::plot_layout(heights = c(4,1)) -> pout

pout


ggsave(pout, 
  filename = 'pH_times_series_average.png', 
  path = path, width = 7, height = 5)

out_stats %>%
  mutate(date = factor(date, levels = recode_date)) %>%
  mutate(y = upper - lower) %>%
  ggplot(aes(y = sd, x = n)) + 
  geom_smooth(method = "lm", linetype="dashed", size = 0.5, alpha=0.5,
    se = TRUE, na.rm = TRUE) +
  ggpubr::stat_cor(method = "pearson", cor.coef.name = "R", p.accuracy = 0.001) +
  geom_point() +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) +
  # scale_color_manual('', values = getPalette,
    # limits = namesL[-1], labels = Labels) +
  scale_y_continuous(n.breaks = 10) 
  # facet_grid(~ g, scales = 'free_x')



# Statistical test ----
# Buscamos diferencias entre las medias de pH en los sistemas. La intencion es evaluar si hubo o no diferencias entre los dias y/o en que grado. Una vez hecha la prueba, hacemos un promedio por sistema siempre y cuando no haya dif significativas entre las medias.

library(rstatix)

# Identify outliers
out_stats %>% group_by(name) %>% identify_outliers(a)

# The normality assumption can be checked by computing the Shapiro-Wilk test. If the data is normally distributed, the p-value should be greater than 0.05.

out_stats %>% group_by(name) %>% shapiro_test(a) %>% adjust_pvalue()

# A partir de la salida, si el valor p es mayor que el nivel de significación 0,05, indica que la distribución de los datos no es significativamente diferente de la distribución normal. En otras palabras, podemos asumir la normalidad.

# Statistical priori test----

# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation where there are more than two groups. It’s recommended when the assumptions of one-way ANOVA test are not met. T

# We want to know if there is any significant difference between the average of the 3 pH conditions.

out_stats %>%
  ungroup()%>%
  rstatix::kruskal_test(a ~ name) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> kruskal.stats


# Statistical posteriori test----

# En vista de que encontramos outliers y no normalidad, evaluamos a traves de un test de wilcoxon

out_stats %>%
  ungroup() %>%
  # t_test(a ~ name)
  pairwise_wilcox_test(a ~ name,  conf.level = 0.95) %>%
  adjust_pvalue() %>%
  add_significance("p") -> stats.test

stats.test %>% add_xy_position(x = "name") -> stats

# stats$y.position <- max(out_stats$a)+out_stats$sd

subtitle <- get_pwc_label(stats, "expression")

title <- get_pwc_label(stats)
subtitle <- get_test_label(kruskal.stats, detailed = TRUE)

# and plot

out_stats %>%
  ungroup() %>%
  # mutate(name = factor(name, levels = c('Canal-3', 'Canal-4', 'Canal-2'))) %>%
  ggplot(aes(y = a, x = as.factor(name))) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) +
  stat_boxplot(geom ='errorbar', width = 0.07) +
  geom_boxplot(width = 0.3, outlier.alpha = 0) +
  geom_point(aes(size = n), alpha = 0.5) +
  stat_summary(fun=mean, geom="point", shape=23, size=1, color = 'red') +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(legend.position = 'bottom') + 
  scale_y_continuous(n.breaks = 10) +
  # scale_x_discrete(labels = c("Experimental I", "Experimental II", 'Control')) +
  scale_size('Set Size', range = c(0,5)) +
  labs(y = 'Average pH', x = '') -> psave

# psave + coord_flip()  -> psave

psave + theme(panel.border = element_blank()) -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, linetype = 'dashed') +
  labs(subtitle = subtitle, title = title) -> psave

psave

out_stats %>%
  group_by(name) %>%
  summarise(a = mean(a))


ggsave(psave, filename = 'average_pH_boxplot.png', path = ggsavepath, 
  width = 5, height = 5)



