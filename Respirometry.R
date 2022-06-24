# https://csdaw.github.io/ggprism/articles/web-only/palettes.html
# https://csdaw.github.io/ggprism/articles/web-only/palettes.html

rm(list = ls()) # Limpiar la memoria de la sesion de R

options(stringsAsFactors = FALSE) # 

ggsavepath <- paste0(getwd(), '/Figures')

#

# Define Color Palette: ----

# colNames <- c("Aspecto.1",	"Aspecto.2")

# RColorBrewer::display.brewer.all() # Revisamos todas las paletas de colores que tenemos

getPalette <- RColorBrewer::brewer.pal(3, 'Set1')

# Cvalues <- structure(getPalette, names = colNames)

namesL <- c('8.0', '8.0', '7.6', '7.8')


getPalette <- RColorBrewer::brewer.pal(4, 'Paired')

getPalette <- structure(getPalette, names = namesL) 


#

library(tidyverse)


# 

path <- '~/Documents/DOCTORADO/Respirometry/'

file <- '.csv$'

file <- list.files(path, pattern = file, full.names = TRUE)

df <- lapply(file, read.csv) 

df <- do.call(rbind, df)

df %>% as_tibble()

# recode_pH <- c('Experimental-I', 'Experimental-II', 'Control')
# level_key <- structure(recode_pH, names = unique(df$pH))

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",`8.0`= "#4575b4", `8`= "#4575b4")

pHLevel <- rev(unique(df$pH))

pHpalette <- pHpalette[match( pHLevel, names(pHpalette))]
df %>% mutate(pH = factor(as.character(pH), levels = pHLevel)) %>% as_tibble() -> df

# df %>% ungroup() %>% mutate(pH = recode_factor(pH, !!!level_key)) -> df

# EDA -----

df %>%
  mutate(N = `Aspecto.1`+ `Aspecto.2`) %>%
  mutate(across(c(5:6),
    .fns = ~./N)) %>% 
  pivot_longer(cols = all_of(c('Aspecto.1','Aspecto.2'))) %>%
  drop_na(value) -> df_longer

df_longer %>%
  filter(hpf == 24) %>%
  ggplot(aes(x = ID, y = value*100, fill = name)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  coord_flip() +
  # scale_fill_manual('', values = getPalette) +
  theme_bw(base_size = 10, base_family = "GillSans") +
  theme(
    legend.position = 'bottom',
    legend.text = element_text(size = 5),
    strip.background = element_blank(),
    panel.border = element_blank()) -> psave

psave + 
  ggh4x::facet_nested(hpf + pH ~., 
    scales = "free", space = "free", switch = "y")

# hacer un barplot con barras de error para 24 hpf unicamente comparando el % de aspectos 

library(rstatix)
# remove Outlies?  ----
# 

df_longer %>% 
  group_by(pH, hpf) %>% 
  # identify_outliers(value)
  mutate(is.outlier = is_outlier(value), 
    is.extreme = is_extreme(value)) -> df_longer

df_longer %>% filter(!is.outlier %in% TRUE) %>% select(-is.outlier, -is.extreme) -> df_longer

write_rds(df_longer, file = paste0(getwd(), 'competency.rds'))


df_longer %>% group_by(pH, hpf, is.outlier) %>% tally() %>%
  filter(is.outlier %in% TRUE)

source(paste0(getwd(), '/stats.R'))

subtitle <- expression('Outlier detection by the i Standard Deviations from the Mean')
caption <- expression('Zscore = (x - mean(x)) / sd(x)')

df %>%
  group_by(hpf, pH) %>%
  summarise(qqfun(N)) %>%
  mutate(z = zfun(y)) %>%
  mutate(outlier = ifelse(abs(z)>=3, TRUE, FALSE)) -> outliersdf

outliersdf %>%
  ggplot(aes(x, z, color = outlier)) +
  geom_smooth(method = "lm", linetype="dashed", size = 0.5, alpha=0.5, 
    se = TRUE, na.rm = TRUE) +
  geom_point(size = 2.5, alpha = 0.8) + 
  facet_grid(hpf~pH) +
  ggpubr::stat_cor(aes(group = hpf), method = "pearson", cor.coef.name = "R", p.accuracy = 0.001) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  labs(x = "Expected", y = expression(Z[score]), caption = caption, subtitle = subtitle) +
  scale_color_manual(name = expression("Outlier-"~sigma), 
    values = c('black', 'red')) +
  theme(legend.position = "none")

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
  theme(legend.position = "none")

# 1) test if gaussianity (FALSE) ----

df_longer %>% 
  filter(is.outlier == FALSE) %>% 
  group_by(pH, hpf) %>% rstatix::shapiro_test(value) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))


# homocelasticidad (FALSE) ----

# (omit) homocelasticidad or homogeneity of variance across groups

df_longer %>%
  group_by(hpf) %>%
  levene_test(value ~ as.factor(pH)) %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# Lets test non parametric comparsion between pH across the times

# Nos interesa comparar el  % de larvas que, por tratamiento, se observo un mayor/menor % competencia entre una fase y otra. Para ello, consideramos el Aspecto 2, a las 24 hpf, donde se evalue el numero de individuos presentes, en cada replica, que presentaban caracteristicas de larvas trocoforas. Los valores de Aspecto 2 o 1 son normalizados respecto al total de los individuos (N) y aplico prueba a priori y posteriori para considerar diferencias entre los tratamientos. 

# 
# Asi que,

df_longer %>% 
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

# instead of summary version, try


df_longer %>%
  filter(name %in% 'Aspecto.2') %>%
  ggplot(aes(x = as.factor(hpf), y = value, color = pH)) +
  geom_point(alpha = 0.5, position = position_dodge2(width = 0.2)) +
  stat_summary(fun=median, geom ="line", aes(group = pH), size= 0.5, position = position_dodge2(width = 0.2)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d('', option = "plasma", end = .7) +
  theme_classic(base_family = "GillSans", base_size = 10) +
  theme(strip.background = element_rect(fill = 'white'),
    panel.border = element_blank()) +
  labs(y = '',  x = 'Time (hpf)') -> psave


df_longer %>%
  filter(name %in% 'Aspecto.2') %>%
  group_by(hpf) %>%
  mutate(pH = as.factor(pH)) %>%
  rstatix::kruskal_test(value ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> kruskal.stats

df_longer %>%
  filter(name %in% 'Aspecto.2') %>%
  group_by(hpf) %>%
  mutate(pH = as.factor(pH)) %>%
  pairwise_wilcox_test(value ~ pH, 
    # ref.group = "8", 
    conf.level = 0.95) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> stats.test

stats.test %>% add_xy_position(x = "hpf", group = 'pH', dodge = 0.2) -> stats

title <- get_pwc_label(stats)

subtitle <- get_description(kruskal.stats)


psave + 
  theme(legend.position = 'top') +
  ggpubr::stat_pvalue_manual(stats, label = "p.signif", 
    remove.bracket = T, tip.length = 0.01, linetype = 'dashed', hide.ns = F) +
    labs(title = title, subtitle = subtitle) -> psave

ggsave(psave, 
  filename = 'competency.png', path = ggsavepath, 
  width = 5, height = 3.5)


write_rds(df_longer, file = paste0(getwd(), 'competency.rds'))

# The Wilcoxon rank sum test is a non-parametric alternative to the independent two samples t-test for comparing two independent groups of samples, in the situation where the data are not normally distributed


# Make a barplot data based on the mean values and include significance differences as previous codes

# By N values

df %>%
  filter(hpf == 24) %>%
  group_by(hpf) %>%
  ggplot(aes(y = N,  x = as.factor(pH))) +
  geom_boxplot() -> psave


# By pct

# geom_bar(stat = "identity", width = 0.7, 
#   position = position_dodge(width = 0.4)) +
# ggh4x::facet_nested(Phylum  ~wrap, scales = "free", space = "free", switch = "y") +
# coord_flip() +
# geom_errorbar(aes(ymin = logFC - SE, 
#   ymax = logFC + SE), width = 0.2,
#   position = position_dodge(0.05), color = "black") + 
# geom_text(aes(y = logFC+2.5*sign(logFC), label=star), 
#   vjust=.7, color="black", position=position_dodge(width = .5)) +
# ggsci::scale_fill_uchicago()



df_longer %>% filter(hpf == 24) %>% filter(name %in% 'Aspecto.1') -> df4viz

df4viz <- df_longer %>% filter(name %in% 'Aspecto.1')

df4viz %>%
  drop_na(value) %>%
  group_by(hpf, name, pH) %>%
  summarise(
    a = mean(value), sd = sd(value), IC = IC(value),
    upper = a+IC, lower = a-IC,
    zupper = a+(3*sd), zlower = a-(3*sd), n = n()) -> out_stats


out_stats %>%
  mutate(name = gsub("[.]", " ", name)) %>%
  mutate(hpf = as.factor(hpf)) %>%
  ggplot(aes(y = a, x = as.factor(pH), fill = hpf)) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) +
  facet_grid(.~ hpf, scales = 'free_y') +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  ylim(0,1) +
  # labs(y = expression("RE (" ~Log[10]~")"), x = "") +
  scale_color_manual(values = c('black', 'Blue')) +
  scale_fill_brewer() +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(legend.position = 'none') -> psave

psave

# En vez del valor N debe ser pct

df4viz %>%
  group_by(hpf) %>%
  mutate(pH = as.factor(pH)) %>%
  pairwise_wilcox_test(value ~ pH, 
    # ref.group = "8", 
    conf.level = 0.95) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> stats.test

# stats.test %>% view()

stats.test %>% add_xy_position(x = "pH", group = '8') -> stats

stats$y.position <- max(out_stats$a)+out_stats$sd

subtitle <- get_pwc_label(stats)

psave + 
  ggpubr::stat_pvalue_manual(stats, hide.ns = T, label = "p.adj") +
  labs(subtitle = subtitle, y = 'Average', x = 'pH')
