# Evaluacion de los datos respirometricos.
# 1) Preparar datos de entrada para la formula
# 2) Inspeccionar toma de datos
# 2.1) Evaluar por archivo la formula ( O2[B] - O2[A] ) / ( T[B] - T[A]) = delta 
# delta - adjustment (ie. delta blank) / Indv
# 3) unir datos respirometria y conteos absolutos  

# Oxygen [cO2 [µmol/L]]
# Oxygen consumption was measured over 1.5 to 2 h and checked for linearity. Respiration rates were corrected for oxygen consumption in the controls and normalized to the number of larvae (μmol O2⋅larva− 1 ⋅ h− 1).

# CLEAR OBJECT LIST AND IMAGE CANVAS   

rm(list = ls());


if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE, readr.show_col_types = FALSE)

library(tidyverse)
library(ggplot2)
library(lubridate)
library(rstatix)
# library(flextable)

ggsavepath <- paste0('~/Documents/GitHub/OA-research/rproject/Figures/')

mtd_file <- 'MTD_RESPIROMETRY.csv'

path <- '~/Documents/DOCTORADO/Respitometría/'

pattern <- 'Oxygen.xlsx$'

files <- list.files(path = path, pattern = pattern, full.names = T)

# features <- list.files(path = path, pattern = 'conteos_abs_respirometrias', full.names = T)
# 
# features <- read_csv(features) %>% select(ID, Spot, N, Blank, blank_Lane)

mtd_df <- read_csv(list.files(path = path, pattern = mtd_file, full.names = T)) %>% 
  select(ID, N, Design) %>% mutate(Design = ifelse(Design %in% 'Control', 'Blank', Design))

#

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",`8.0`= "#4575b4", `8`= "#4575b4")

pHLevel <- c("8", "7.8", "7.6")

pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]


# hpfL <- c("24",  "48",  "60", "108", "624")
# Drop blanks spots were no larvae was found 

mtd_df %>% 
  mutate(N = ifelse(Design %in% 'Blank' & N > 0, NA, N)) %>%
  drop_na(N) -> mtd_df

mtd_df %>% count(Design)

mtd_df %>%
  filter(Design %in% "Experimental") %>%
  separate(ID, into = c("hpf", "pH", "spot"), sep = "-") %>%
  count(hpf, pH)

mtd_df %>%
  filter(Design %in% "Experimental") %>%
  separate(ID, into = c("hpf", "pH", "spot"), sep = "-") %>%
  # group_by(pH, hpf) %>%
  rstatix::get_summary_stats()
  # summarise(mean = mean(N), sd = (N), n = n())


mtd_df %>%
  separate(ID, into = c("hpf", "pH", "spot"), sep = "-") %>%
  ggplot()  + geom_boxplot(aes(y = N, x = pH))
# namespH <- c("7.6","7.8","8")

# recodeL <- c('Experimental-I', 'Experimental-II', 'Control')

# level_key <- structure(recodeL, names = namespH)


read_sdr <- function(file) {
  
  hpf <- sapply(strsplit(basename(file), "_"), `[`, 2)
  pH <- sapply(strsplit(basename(file), "_"), `[`, 3)
  
  df <- readxl::read_excel(file, col_names = T, skip = 12) %>% 
    rename('date' = `Date/Time`, 'min' = `Time/Min.`) %>%
    separate(col = date, into = c('date', 'time'), sep = ' ') %>%
    mutate(date = paste0(date,"T",time)) %>%
    mutate(date = lubridate::dmy_hms(date)) %>%
    mutate(hour = lubridate::hour(date)) %>%
    mutate(hpf = as.numeric(hpf), pH = as.numeric(pH)) %>%
    mutate(row_id  = 1:nrow(.))
  
  return(df)
  
}

# Load all datasets ----

df <- lapply(files, read_sdr)

print(df <- do.call(rbind, df))

df %>% distinct(pH) %>% pull() %>% rev() -> pHLevel

pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]

df %>% distinct(lubridate::date(date))

df %>% distinct(hpf) %>% arrange(hpf) %>% pull() -> hpfL


# average of , Tinternal ??
# 
df %>% group_by(pH, hpf) %>%
  mutate(TEMP = `Tm [°C]`) %>%
  # mutate(TEMP = `T_internal [°C]`) %>%
  summarise(a = mean(TEMP), 
    sd = sd(TEMP),
    var = var(TEMP)) %>%
  mutate(ymin = a-sd, ymax = a+sd) %>%
  mutate(hpf = factor(hpf, hpfL)) %>% view()

# Internal salinity

df %>% group_by(pH, hpf) %>%
  summarise(a = mean(`Salinity [g/1000g]`), 
    sd = sd(`Salinity [g/1000g]`),
    var = var((`Salinity [g/1000g]`))) %>%
  mutate(ymin = a-sd, ymax = a+sd) %>%
  mutate(hpf = factor(hpf, hpfL))

# wrangling data -----

cols <- names(df)

cols <- cols[grepl('^[A-Z][0-9]$', cols)]

df %>% 
  select(row_id, date, hpf, pH, all_of(cols)) %>% # min, hour,
  pivot_longer(cols = all_of(cols), names_to = 'Spot', values_to = 'Ox') %>%
  mutate(ID = paste0(hpf, '-', pH, '-', Spot)) %>%
  mutate(g = substr(Spot, 1,1)) -> df_longer

# convert to picomol/L
# df_longer <- df_longer %>% mutate(Ox = Ox*1E6)

mtd_df %>% distinct(ID)

sum(sort(unique(mtd_df$ID)) %in% sort(unique(df_longer$ID))) # must be 145 spots

# Previz ----
df_longer %>%
  right_join(mtd_df, by = 'ID') %>%
  mutate(hpf = factor(hpf, levels = hpfL)) -> dfviz

# ylab <- expression(O[2]~ "(" ~ mu~"mol"~h^-1~L^-1~")")

# Oxygen [cO2 [µmol/L]]

ylab <- expression(O[2]~ "(" ~ mu~"mol"~L^-1~")")

dfviz %>%
  group_by(Spot, Design, hpf, pH) %>%
  summarise(max = max(Ox)) %>% 
  ggplot(aes(x = as.factor(pH), y = max, fill = Design, color = Design)) + 
  geom_boxplot(width = 0.3, outlier.alpha = 1, outlier.color = 'red', 
    outlier.shape = 1,outlier.size = 0.5) +
  stat_summary(fun = mean, geom ="line", aes(group = Design), size= 0.5) +
  facet_grid(. ~ hpf) +
  labs(x = 'pH', y = ylab) -> p0

p0 <- p0 +
  theme_bw(base_family = "GillSans", base_size = 12) +
  theme(legend.position = "top",
    strip.background = element_rect(fill = 'grey89', color = 'white'),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0),
    plot.caption = element_text(hjust = 0),
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    # strip.background.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10))

ggsave(p0, 
  filename = 'max_oxygen_raw.png', path = ggsavepath, 
  width = 5, height = 2.5, device = png, dpi = 300)

fun.data.trend <- "mean_se" # "mean_cl_boot", "mean_sdl"

dfviz %>%
  filter(hpf != "24") %>%
  group_by(Spot, Design, hpf, pH) %>%
  summarise(max = max(Ox)) %>% 
  ggplot(aes(x = as.factor(pH), y = max, fill = Design, color = Design, group = Design)) + 
  facet_grid(Design ~., scales = "free_y") +
  geom_jitter(position=position_jitter(w=0.1,h=0), size = 1, alpha = 0.5) +
  stat_summary(fun.data = fun.data.trend, linewidth = 0.7, size = 0.7, alpha = 0.7) +
  stat_summary(fun = mean, geom = "line") +
  labs(x = 'pH', y = ylab)


dfviz %>%
  group_by(Spot, Design, hpf, pH) %>%
  summarise(max = max(Ox)) %>% 
  filter(Design %in% 'Blank') %>%
  group_by(hpf, pH) %>%
  # summarise(av = mean(max))
  ggplot(aes(x = hpf, y = max, color = as.factor(pH))) + 
  geom_boxplot(width = 0.3, outlier.alpha = 1, outlier.color = 'red',
    outlier.shape = 1,outlier.size = 0.5) +
  # geom_point(position = position_dodge(width = 0.3)) +
  scale_color_manual('', values = pHpalette) +
  labs(x = 'hpf', y = ylab, caption = 'Blank Spots') -> p0

p0

# ggsave(p0, 
#   filename = 'max_oxygen_raw_hpf.png', path = ggsavepath, 
#   width = 5, height = 2.5)

# test differences between max ox
# Design vs experimental inicial max values
# pH 

dfviz %>%
  group_by(Spot, Design, hpf, pH) %>%
  summarise(max = max(Ox)) %>%
  filter(Design %in% 'Blank') -> df_max

# 0) Outliers

df_max %>% group_by(Design, hpf, pH) %>% 
  # rstatix::identify_outliers(max)
  mutate(is.outlier = is_outlier(max)) %>%
  filter(!is.outlier %in% TRUE) -> df_max

df_max %>%  group_by(Design, hpf, pH) %>% tally()


df_max %>%
  group_by(hpf, pH) %>%
  summarise(a = mean(max), sd = sd(max), 
    ymin = max-sd, ymax = max+sd, n = n()) %>%
  ggplot(aes(x = hpf, y = a, color = as.factor(pH))) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), 
    width = 0.1, position = position_dodge(width = 0.3)) +
  geom_path(position = position_dodge(width = 0.3), size = 1) +
  scale_color_viridis_d('', option = "plasma", end = .7) 


# 1) test if gaussianity (FALSE)

df_max %>% 
  group_by(pH) %>%
  rstatix::shapiro_test(max) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

df_max %>% 
  group_by(pH)%>%
  levene_test(max ~ as.factor(hpf), center = 'median') %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# 2) Homocelasticidad (PARCIAL T) 

df_max %>% 
  group_by(hpf)%>%
  levene_test(max ~ as.factor(pH), center = 'median') %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# 3) Priori (non parametric test)

df_max %>% 
  group_by(hpf) %>%
  # ungroup() %>%
  rstatix::kruskal_test(max ~ pH) %>%
  adjust_pvalue() %>%
  add_significance("p.adj") -> kruskal.stats

# library(flextable)
# 
# # https://ardata-fr.github.io/flextable-book/
# 
# flextable(kruskal.stats) %>%
#   bold(~ p < 0.05, ~ p, bold = TRUE) %>%
#   autofit(add_w = 0, add_h = 0) %>%
#   align(align = "center")

# 4) Posteriori (Also non parametric)

df_max %>% 
  group_by(hpf) %>%
  # ungroup() %>%
  pairwise_wilcox_test(max ~ pH,  conf.level = 0.95) %>% # p.adjust.method = 'fdr'
  add_significance("p.adj") -> stats.test

# stats.test %>% filter(!p.adj.signif %in% 'ns') %>%
#   flextable() %>% 
#   bold(~ p < 0.05, ~ p, bold = TRUE) %>%
#   autofit(add_w = 0, add_h = 0) %>%
#   align(align = "center")

# test differences in max ox during the larval development (not between treatments)

df_max %>% 
  group_by(pH) %>%
  # ungroup() %>%
  rstatix::shapiro_test(max) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

df_max %>% 
  group_by(pH)%>%
  # ungroup() %>%
  levene_test(max ~ hpf, center = 'median') %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

df_max %>%
  ggplot(aes(x = as.factor(pH), y = max)) +
  geom_boxplot() +
  geom_point(aes(color = hpf))

# df_max %>% 
#   # arrange(pH)
#   group_by(pH) %>%
#   # ungroup() %>%
#   rstatix::kruskal_test(max ~ hpf) %>%
#   adjust_pvalue() %>%
#   add_significance("p.adj") %>%
#   flextable() %>% 
#   bold(~ p < 0.05, ~ p, bold = TRUE) %>%
#   autofit(add_w = 0, add_h = 0) %>%
#   align(align = "center")



# Max oxygen decrease as pH decrease
# 

dfviz %>%
  group_by(Spot, Design, hpf, pH) %>%
  summarise(max = max(Ox)) %>%
  group_by(hpf) %>%
  cor_test(vars = 'pH', vars2 = c('max'), method = 'pearson') %>%
  # rstatix::adjust_pvalue() %>%
  add_significance("p")
  

dfviz %>% 
  drop_na() %>%
  # mutate(pH = recode_factor(pH, !!!level_key)) %>%
  ggplot(aes(y = Ox, x = row_id, 
    color = as.factor(pH), group = Spot)) + 
  geom_path() + 
  theme_bw(base_family = "GillSans", base_size = 12) +
  scale_color_manual('', values = pHpalette) +
  facet_grid(as.factor(hpf) ~ Design, scales = 'free') -> p1

ggsave(p1, 
  filename = 'oxygen_rate_raw.png', path = ggsavepath, 
  width = 5, height = 3.5)

# percent

dfviz %>% 
  drop_na() %>%
  group_by(ID) %>%
  mutate(Rate = Ox/max(Ox)) %>%
  ggplot(aes(y = Rate, x = row_id, 
    color = as.factor(pH), group = pH)) + 
  geom_smooth(se = F, method = lm) +
  theme_bw(base_family = "GillSans", base_size = 12) +
  facet_grid(hpf ~ Design, scales = 'free_x') +
  scale_color_manual('', values = pHpalette) -> p2

ggsave(p2, 
  filename = 'oxygen_rate_lm.png', path = ggsavepath, 
  width = 5, height = 3.5)

# CONTINUE HERE ----

# Summarise average of rates (time and O2 consumption) by hpf, pH and Spot (ie Lane)
# Using function rate and time_rate

delta_time <- function(.x,.y) {
  x <- .x
  y <- .y
  pos1 <- which(y == max(y))[1]
  pos2 <- which(y == min(y))[1]
  # out <- x[pos1] - x[pos2]
  out <- data.frame(x = x[pos1], y = x[pos2] )
  
  return(out)
} # delta de tiempo donde se encontro la cantidad min y max de oxigeno, 

# Delta t no siempre es la misma posicion

scope_o2 <- function(x) { max(x) - min(x) } # cantidad de O2 consumido!!

# test 
# cutoff times????


# df_longer %>% 
#   group_by(ID) %>% # hpf, pH, Spot
#   summarise(#sd = sd(Ox), n = n(), 
#     scope = scope_o2(Ox), delta_time(date, Ox)) %>%
#   mutate(mins = abs(difftime(x,y)), hour = as.numeric(mins/60)) %>%
#   right_join(mtd_df, by = 'ID') %>%
#   separate(ID, sep = '-',into = c('hpf', 'pH', 'Spot')) %>%
#   mutate(hpf = factor(hpf, hpfL)) %>%
#   ggplot(aes(x,y)) + geom_point(aes(color = pH)) + 
#   facet_wrap(~hpf, scales = 'free', nrow = 1) 


df_longer %>% 
  group_by(ID) %>% # hpf, pH, Spot
  summarise(#sd = sd(Ox), n = n(), 
    scope = scope_o2(Ox), delta_time(date, Ox)) %>%
  mutate(mins = abs(difftime(x,y)), hour = as.numeric(mins/60)) %>% 
  right_join(mtd_df, by = 'ID') %>% 
  mutate(rate = scope/hour) %>%
  separate(ID, sep = '-',into = c('hpf', 'pH', 'Spot')) %>%
  select(-mins, -y,-x) %>%
  mutate(hpf = factor(hpf, hpfL)) %>%
  mutate(pH = factor(pH, levels = pHLevel)) -> df_stats 



# df_stats %>% group_by(hpf, pH, Design) %>% tally() 

df_stats  %>%
  filter(Design %in% 'Blank') %>%
  ggplot(aes(x = pH, y = rate, group = as.factor(pH))) +
  facet_grid(~ hpf) +
  geom_boxplot(outlier.alpha = 1, outlier.color = 'red',
  outlier.shape = 1, outlier.size = 1 ) +
  stat_summary(fun = median, geom ="line", aes(group = Design), size= 0.5)

df_stats  %>%
  filter(Design %in% 'Blank') %>%
  group_by(hpf, pH) %>%
  summarise(av_r_b = mean(rate),
    sd_r_b = sd(rate), 
    av_scope_b = mean(scope), 
    sd_scope_b = sd(scope), n = n()) -> df_stats_blank

df_stats %>%
  filter(!Design %in% 'Blank') %>%
  left_join(df_stats_blank, by = c('hpf', 'pH')) %>%
  mutate(scope_adj = (scope - av_scope_b) ,r_adj = (rate - av_r_b), r_ind = rate/N, r_ind_adj = r_adj/N) %>% 
  select(!c(hour, Design, av_r_b, sd_r_b, av_scope_b, sd_scope_b, n)) -> df_stats


# 0) Convertion of units:
# From (μmol O2⋅larva− 1 ⋅ h− 1 L-1)
# To (μmol O2⋅larva− 1 ⋅ h− 1).
# Then ((pmol O2⋅larva− 1 ⋅ h− 1))

# Vol of chamber: 1700 μL OR 0.0017 L

df_stats <- df_stats %>% 
  mutate(r_ind_adj = r_ind_adj * 0.0017) %>%
  mutate(r_ind_adj = r_ind_adj * 1E6)



df_stats %>%
  select(-hpf, -pH, -Spot) %>%
  cor_mat(method = 'spearman') %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

# df_stats %>% GGally::ggpairs(columns = 3:8, aes(colour=pH)) 

#  GaD
# 20/05/2022 !! aqi vamos bien -----
#
# 

# view outliers 

df_stats %>% select_if(is.double) %>% names() -> cols

cols <- c("scope","scope_adj", "rate","r_adj","r_ind","r_ind_adj")

df_stats %>%
  select(-N) %>%
  group_by(hpf, pH) %>%
  mutate(is.outlier = is_outlier(r_ind_adj)) %>%
  filter(!is.outlier %in% TRUE) %>%
  select(-is.outlier) %>%
  pivot_longer(cols = cols) %>%
  mutate(name = factor(name, levels = cols)) %>%
  filter(grepl("_adj", name)) %>%
  ggplot(aes(x = pH, y = value, color = pH, fill = pH)) +
  facet_grid(name ~ hpf, scales = 'free') +
  # theme_bw(base_family = "GillSans", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45,
    hjust = 1, vjust = 1, size = 10)) +
  stat_boxplot(geom ='errorbar', width = 0.07) +
  # geom_point(alpha = 0.5) +
  geom_boxplot(width = 0.3, outlier.alpha = 0, outlier.color = 'red') +
  # stat_summary(fun = median, geom ="line", aes(group = 2), size= 0.5, color = 'grey') +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) +
  theme_classic(base_family = "GillSans", base_size = 12) +
  labs(y = '') -> psave



psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> psave

ggsave(psave,
  filename = 'oxygen_rate_facet.png', path = ggsavepath,
  width = 4.5, height = 3.5)


# 0) Remove outliers ----- 

library(rstatix)

df_stats %>%
  group_by(hpf, pH) %>%
  mutate(is.outlier = is_outlier(r_ind_adj),
  is.extreme = is_extreme(r_ind_adj)) %>%
  filter(is.outlier %in% TRUE)

df_stats %>% 
  group_by(hpf, pH) %>%
  mutate(is.outlier = is_outlier(r_ind_adj)) %>%
  filter(!is.outlier %in% TRUE) %>%
  select(-is.outlier) -> df_stats

# 1) test if gaussianity (TRUE) ----

df_stats %>% 
  group_by(hpf, pH)%>% 
  rstatix::shapiro_test(r_ind_adj) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

# due to is gausian

# df_stats %>% 
#   summarise(a = mean(r_ind_adj), sd = sd(r_ind_adj), n = n()) %>%
#   mutate_if(is.double, function(x) round(x, digits = 3)) %>%
#   flextable() %>%
#   # bold(~ p < 0.05, ~ p, bold = TRUE) %>%
#   autofit(add_w = 0, add_h = 0) %>%
#   align(align = "center") %>%
#   merge_v() %>%
#   footnote(i = 1, j = 2,
#     value = as_paragraph("Average"),
#     ref_symbols = "a",
#     part = "header",inline=T)

# 2) Homocelasticidad (TRUE)  ----

df_stats %>% 
  ungroup() %>%
  levene_test(r_ind_adj ~ as.factor(pH), center = 'median') %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))


# 3) Parametric priori test----

df_stats %>% 
  group_by(hpf) %>%
  rstatix::welch_anova_test(r_ind_adj ~ pH) %>%
  # rstatix::anova_test(r_ind_adj ~ pH) %>%
  add_significance() -> prior.stats

# 4) Parametric posteriori test ----

df_stats %>%
  group_by(hpf) %>%
  tukey_hsd(r_ind_adj ~ pH) %>%
  # pairwise_t_test(r_ind_adj ~ pH)
  adjust_pvalue() %>%
  add_significance() -> post.test

post.test %>% add_xy_position(x = "pH") -> stats


title <- get_pwc_label(stats)

subtitle <- get_test_label(prior.stats, detailed = F)

# plor as boxplot
# o2/Ind (Umol h-1 L-1)

# ylabs <- expression("Rate ("*O[2]~mu*"mol"~h^-1~L^-1~Ind^-1*")")

ylabs <- expression("Rate ("*O[2]~"pmol"~h^-1~Ind^-1*")")

df_stats %>% 
  ggplot(aes(x = pH, y = r_ind_adj, group = pH)) +
  facet_grid(~ hpf) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  # geom_boxplot(aes(color = pH, fill = pH), width = 0.3) +
  # geom_jitter(aes(color = pH),
  #   width=0.1,alpha=0.2, height = 0.1, size = 1.2, shape = 1) +
  stat_boxplot(aes(color = pH),
    geom ='errorbar', width = 0.3) +
  geom_boxplot(aes(color = pH, fill = pH), 
    width = 0.3, outlier.alpha = 0) +
  # stat_boxplot(geom ='errorbar', width = 0.3,)
  # stat_summary(fun = median, geom ="line", aes(group = 2),size= 0.5, color = 'gray') +
  labs(y = ylabs) +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) -> psave


psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(),
  legend.position = "top") -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01,  hide.ns = T) +
  labs(caption = title) -> psave

psave

ggsave(psave,
  filename = 'oxygen_rate.png', path = ggsavepath,
  width = 5.5, height = 3.5)

path_out <- '~/Documents/MIRNA_HALIOTIS/'

# save(df_stats, stats, file = paste0(path_out, 'resp_rates.Rdata'))

# save(df_stats, stats, file = paste0(path_out, 'resp_rates.Rdata'))

# saveRDS(list(df_stats, stats), file = paste0(path_out, 'resp_rates.Rdata'))

write_rds(list(df_stats, stats), file = paste0(path_out, 'resp_rates.rds'))

# at the end

# df_stats %>%
#   group_by(hpf, pH) %>%
#   summarise(a = mean(r_ind_adj), sd = sd(r_ind_adj), 
#     ymin = a-sd, ymax = a+sd, n = n()) %>%

df_stats %>%
  select(r_ind_adj) %>%
  rstatix::get_summary_stats(type = 'quantile') %>%
  filter(!variable %in% 'N') %>%
  filter(grepl("_adj", variable)) %>%
  ggplot(aes(x = hpf, y = `50%`, color = pH, group = pH)) +
  # facet_grid(variable ~., scales = "free_y") +
  geom_point(position = position_dodge(width = 0), size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`), 
    width = 0.1, position = position_dodge(width = 0)) +
  geom_path(position = position_dodge(width = 0), size = 1) +
  # theme_bw(base_family = "GillSans", base_size = 14) +
  scale_color_manual("", values = pHpalette) +
  labs(y = ylabs) -> ps

ps + theme_bw(base_family = "GillSans", base_size = 12) + 
  theme(strip.background = element_rect(fill = 'grey', color = 'white'),
    panel.border = element_blank(),
    legend.position = "top") -> ps

ggsave(ps,
  filename = 'oxygen_rate_facet_by_hpf.png', path = ggsavepath,
  width = 3, height = 2.5)

# data input for model ------

df_stats %>%
  select(hpf, pH, r_ind_adj) %>%
  rstatix::get_summary_stats(type = 'mean_sd') %>%
  select(-variable, -n, -sd) %>%
  pivot_wider(names_from = pH, values_from = mean)



# Paso 2.1 ----

# Los controles (blancos) son siempre las columnas A[1-6]

df <- read_sdr(files[6])

cols <- names(df)

blank_cols <- cols[grepl('^A[1-6]$', cols)]

sample_cols <- cols[grepl('^[B][1-6]$', cols)] # '^[B-D][1-6]$


# df %>% head() %>% view()

df %>% 
  select(row_id, all_of(blank_cols)) %>%
  pivot_longer(cols = blank_cols) %>%
  ggplot(aes(x = name, y = row_id, fill = value)) + 
  # geom_tile(color = 'white', size = 0.2) +
  geom_raster() +
  scale_fill_viridis_c(name = "", direction = -1)

df %>% 
  select(row_id, all_of(sample_cols)) %>%
  pivot_longer(cols = sample_cols) %>%
  ggplot(aes(x = name, y = row_id, fill = value)) + 
  # geom_tile(color = 'white', size = 0.2) +
  geom_raster() +
  scale_fill_viridis_c(name = "", direction = -1)

df
# # 2.1) evaluar por archivo la formula ( O2[B] - O2[A] ) / ( T[B] - T[A]) = delta 
# como hacer el diff time ???

# Piensa como vector

vect <- df$A1

rate <- function(x) { max(x) - min(x) } # cantidad de O2 consumido!!

max(vect)-rate(vect) -> yend
max(vect)-rate(vect)

segment_df <- data.frame(
  y = max(vect), 
  yend = yend,
  x = df[which(vect == max(vect)), 'row_id']$row_id,
  xend = df[which(vect == min(vect)), 'row_id']$row_id
  )




# 
# 
# 
# df[which(vect == max(vect)), 'date']
# 
# ratev <- max(vect)-rate(vect)
# 
# df[which(vect == ratev), 'date']


ggplot() + 
  geom_path(data = df, aes(x = row_id, y = A1)) + 
  geom_point(data = subset(df, A1 == max(A1)), aes(y = A1, x = row_id), color = 'red') +
  geom_point(data = subset(df, A1 == min(A1)), aes(y = A1, x = row_id), color = 'red') +
  # geom_smooth(data = df, aes(x = row_id, y = A1), se = T, method = lm)
  geom_curve(data = segment_df, aes(x = x, y = y, xend = xend, yend = yend))

# test respR -----
# install.packages("respR")
# https://januarharianto.github.io/respR/

library(respR) # load the package

# 1. test auto-rate

df %>% 
  select(min, all_of(sample_cols)) %>%
  inspect(time = 1, oxygen = 2, plot = F) %>% # select columns 1 and 15 and inspect the data, then
  auto_rate() %>% # automatically determine most linear segment
  # plot_ar()
  convert_rate( # convert to units
    oxy.unit = "umol/L", 
    time.unit = "min", 
    output.unit = "umol/h", 
    volume = 2E-3) -> out

# 2. Calculate background rate
# Example https://www.mdpi.com/2673-1924/2/1/2/htm

df %>% 
  select(min, all_of(blank_cols)) %>%
  inspect(time = 1, oxygen = 2:7) %>%
  calc_rate.bg(time = 1) -> bg

out <- list()

for(i in sample_cols) {
  df %>%
    select_at(vars(min, i)) %>%
    auto_rate(plot = F) -> rate_df
  
  rate_df <- rate_df$summary
  
  rate_df <- mutate(rate_df, g = i)
  
  out[[i]] <- rate_df
}

do.call(rbind, out) -> rate_df


out_adj <- adjust_rate(rate_df$rate, by = bg)

summary(out_adj)

out_adj %>%
  convert_rate(
    oxy.unit = "umol/L", 
    time.unit = "min", 
    output.unit = "umol/h", 
    volume = 2E-3) %>%
  summary()
