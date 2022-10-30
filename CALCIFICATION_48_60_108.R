
# test reducition of dimensions from birefrigency an dbody size

rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(readr.show_col_types = FALSE, stringsAsFactors = FALSE)


pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4", `8`= "#4575b4")

library(tidyverse)
library(factoextra)
library("FactoMineR")

set.seed(123)

# select(hpf, pH, Index) %>%
level_key <- c("24" = "30")
  
body_size <- read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%select(hpf, pH, Index,) %>%
  # filter(hpf != 24) %>% 
  mutate(hpf = recode_factor(hpf, !!!level_key)) %>%
  mutate(g = 'growth') %>% rename('measure' = 'Index')

birefrigence <- read_rds(paste0(getwd(), '/birefrigence.rds')) %>% select(Area) %>% mutate(g = 'calcification') %>% rename('measure' = 'Area')

# BODY SIZE ----

phi <- (1+sqrt(5))/2 #  it is equal to 1.618033988749
# as_rad <- units::as_units(2*pi*(1-phi), "radians")
# as_deg <- units::set_units(as_rad, "degrees")
# as_deg

rad2deg <- function(rad) {
  (rad * 180) / (pi)
}

golden_mean <- rad2deg(2*pi*(1-phi))

# #

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  mutate(
    golden_ratio = (Length+Width)/Length
    # pred_Width = Length/phi,
    # pred_Length = Width*phi,
    # ratio = abs(1 - (golden_ratio/phi))
  ) %>% 
  select(hpf, pH, golden_ratio) %>%
  mutate(g = 'Symmetry') %>% 
  mutate(hpf = recode_factor(hpf, !!!level_key)) %>%
  rename('measure' = 'golden_ratio')-> body_ratio

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  mutate(measure = (Length+Width)/Length,
    # measure = (1+sqrt(5))/(2*measure),
    # measure = (1+sqrt(5))/(2) - measure,
    tetha = 2*pi*(1-measure)
    ) %>%
  # ggplot(aes(y = Index, x = golden, color = pH)) +
  # geom_point()
  ggplot(aes(x = tetha, color = pH)) +
  # geom_point()
  geom_density()  +
  # stat_ecdf(pad = F) +
  geom_vline(xintercept = 1, colour = "grey50", linetype = 2) +
  facet_grid(~ hpf)

# BIREFRYGENCY -----

# 
# read_rds(paste0(getwd(), '/birefrigence.rds')) %>%
#   mutate(mineralized = 'Less') %>%
#   mutate(mineralized = ifelse(between(Frac, 90, 100), 'Fully', mineralized)) %>%
#   mutate(mineralized = ifelse(between(Frac, 70, 89), 'Partially', mineralized)) %>%
#   group_by(pH) %>%
#   count(mineralized) %>% mutate(pct = n / sum(n)) %>%
#   #summarise(sum(pct))
#   ggplot(aes(y = pct, x = mineralized, fill = pH)) +
#   geom_col(position = "dodge2") +
#   # facet_grid(~ hpf) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_manual("", values = pHpalette) +
#   labs(y = '%', x = 'Mineralized') +
#   theme_bw(base_family = "GillSans", base_size = 14) +
#   theme(panel.border = element_blank(), legend.position = 'top')

birefrigence %>%
  mutate(ntile = ntile(measure, 3)) %>%
  ggplot(aes(measure, color = pH)) +
  stat_ecdf(pad = F) +
  scale_color_manual("", values = pHpalette) +
  facet_grid(hpf ~ ntile)

body_size %>%
  mutate(ntile = ntile(measure, 2)) %>%
  ggplot(aes(measure, fill = pH)) +
  geom_density(alpha = 0.5) + 
  # facet_grid(hpf ~ ntile) +
  scale_fill_manual("", values = pHpalette) 


# reduce dimensions ----

run_cmds <- function(x, dist_method = "euclidean", 
  hclust_method = 'ward.D', k = 3) {
  
  x <- df %>% ungroup() %>% select_if(is.double)
  
  g <- df %>% pull(names(df)[1])
  
  x %>%
    # use distance metric
    dist(method = dist_method) -> distances
  
  distances %>%
    # compute cMDS
    cmdscale() %>%
    data.frame() %>%
    # rownames_to_column(var= 'Domain') %>%
    mutate(g = g) -> cmds
  # 
  
  distances %>%
    hclust(method = hclust_method) %>%
    cutree(., k) -> cutree
  
  cmds <- cbind(cmds, cutree) %>% mutate(cutree = as.factor(cutree))
    
  
  return(cmds)
}


# rbind(birefrigence, body_size) %>%
body_size %>%
  group_by(hpf, pH, g) %>%
  mutate(ntile = ntile(measure, 2)) %>%
  ggplot(aes(y = measure, x = pH)) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width = 0.3, outlier.alpha = 0) +
  geom_jitter(alpha = 0.5, aes(color = as.factor(ntile))) +
  # scale_color_manual("", values = pHpalette) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  facet_grid(g ~ hpf, scales = 'free_y') +
  stat_summary(fun=mean, geom ="point")


birefrigence %>%
  # group_by(hpf, pH, g) %>%
  rstatix::get_summary_stats(measure, show = c('q1' ,'q3', 'mean')) %>%
  pivot_longer(cols = c('q1' ,'q3', 'mean'), values_to = 'y') %>%
  mutate(name = factor(name, levels = c('q1' ,'q3', 'mean'))) %>%
  ggplot(aes(y = y, x = pH, fill = pH)) +
  geom_col() +
  facet_grid(hpf ~ name) +
  scale_fill_manual("", values = pHpalette)

body_size %>%
  rstatix::get_summary_stats(measure, show = c('q1' ,'q3', 'mean')) %>%
  ggplot(aes(y = mean, x = pH, color = pH)) +
  geom_point() +
  theme_classic(base_family = "GillSans", base_size = 12) +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2) +
  facet_grid(hpf ~ .) +
  scale_color_manual("", values = pHpalette)

# stats
library(rstatix)

birefrigence %>%
  mutate(ntile = ntile(measure, 2)) %>%
  group_by(hpf, ntile) %>%
  rstatix::kruskal_test(measure ~ pH) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") 

birefrigence %>%
  mutate(ntile = ntile(measure, 2)) %>%
  group_by(hpf, ntile) %>%
  pairwise_wilcox_test(measure ~ pH,  conf.level = 0.95) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() 

# compare quantiles birefrigence


birefrigence %>% 
  mutate(ntile = ntile(measure, 2),
    # quartile = cut(measure, breaks = quantile(measure, probs = 0:4/4),
      # labels = 1:4, right = FALSE)
    ) %>%
  ggplot(aes(y = measure, x = pH, color = pH)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width = 0.3, outlier.alpha = 0) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  facet_grid(hpf ~ ntile , scales = 'free_y') +
  scale_color_manual("", values = pHpalette) 
 
# bonafide nos quedamos con el quatile 2 ----
# Interquantil test ----

birefrigence %>% 
  mutate(ntile = ntile(measure, 2)) %>%
  filter(ntile > 1) %>%  select(-ntile) -> birefrigence
    
# mad: median absolute deviation (see ?MAD)
rbind(birefrigence, body_size) %>%
  group_by(pH, hpf, g) %>%
  rstatix::get_summary_stats(measure, show = c('iqr' ,'mad')) %>%
  ggplot(aes(y = iqr, x = pH, fill = pH)) +
  geom_col()+
  theme_classic(base_family = "GillSans", base_size = 12) +
  # geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2) +
  facet_wrap(hpf ~ g, scales = 'free_y', ncol = 2) +
  scale_fill_manual("", values = pHpalette)
  

# birefrigence %>% tally()

# rbind(birefrigence, body_size) %>%

# WRANGLING INPUTS -----

birefrigence %>% # birefrigence not grouped good!
  # mutate(ntile = ntile(measure, 2)) %>%
  group_by(pH, hpf) %>% # tally()
  sample_n(20, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df1 
  # filter(ntile > 1) %>% ungroup() %>% select(-ntile) -> df1 

body_size %>% 
  group_by(pH, hpf) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df2

body_ratio %>% # birefrigence not grouped good!
  group_by(pH, hpf) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df3

df <- rbind(df1, df2, df3) # %>% mutate_if(is.double, log)

df

# "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

cmds <- run_cmds(df, k = 3)

cmds %>% 
  ggplot(aes(X1,X2, group = cutree, shape = as.factor(df$g))) + 
  ggforce::geom_mark_ellipse(color = NA, fill = 'grey', alpha = 0.5) +
  # coord_fixed() +
  geom_point(size = 4, aes(color = g), alpha = 0.5) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  labs(x = 'Dimension 1', y = 'Dimension 2') +
  scale_color_manual(values = pHpalette) 

# candisc canonical discriminant analysis (CDA) -----
# install.packages("candisc")
library(candisc)
# ref: Near-future level of CO2-driven ocean acidification radically affects larval survival and development in the brittlestar Ophiothrix fragilis (dpont 2008)

# Canonical discriminant analysis of the morphometric parameters Figure: used to separate the different pH treatments and time post fertiliza- tion. At Day 1, all the larvae from the 3 pH treatments are clustered together indi- cating that they share similar body proportions. At Day 4, larvae raised at pH 7.7 are discriminated from those growing at pH 8.1 (all days) and pH 7.9 (Day 4). Larvae raised at pH 7.7 possessed proportions that were never observed in those raised at normal pH.

# Concl: Moreover, even larvae with normal shape (not abnor- mal or asymmetric) raised at low pH have different morphometric proportions than those raised at normal pH (Fig. 5). This may also have consequences for larval orientation and thus fitness and survival.

# Method: Canonical discriminant analysis was used to assess the impacts of pH and/or exposure time on mor- phometric parameters

# Canonical discriminant analyses were performed on the morphometric parameters to assess individual variation within the treat- ments at Days 1 and 4 . At Day 1, all the larvae from the 3 pH treatments are clustered together indi- cating that they share similar body proportions. At Day 4, larvae raised at pH 7.7 are discriminated from those growing at pH 8.1 (all days) and pH 7.9 (Day 4). Larvae raised at pH 7.7 possessed proportions that were never observed in those raised at normal pH.

# mod <- lm(cbind(`8`,`7.8`,`7.6`) ~ hpf, data=df1)

# PCA ensena la metrica con mayor variacion, pero el analisis de discriminacion (lineal) ofrece una medida de separacion (u) entre dos grupos.. ie. LDA is like PCA but it focuses on maximizn the seperatibility among known categories (statquest)

# The relationship of the response variables to the canonical dimensions is shown by vectors (similar to a biplot). Each vector is defined by the correlations (structure coefficients) it has with the canonical dimensions. The structure coefficients determine only the angle of the vector with respect to the coordinate axes. The vectors are all multiplied by a scale factor, chosen so the vectors nearly fill the plot frame.↩

# LDA FOR BODY SIZE ----

body_size %>% # birefrigence not grouped good!
  group_by(pH, hpf, g) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = hpf, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df1

# body_size %>% 
#   group_by(pH, hpf, g) %>% 
#   sample_n(50, replace = TRUE) %>%
#   pivot_wider(names_from = hpf, values_from = measure, values_fn = list) %>%
#   unnest(cols = everything()) -> df

mod <- lm(cbind(`30`,`48`,`60`,`108`) ~ pH , data=df1)

# mod <- lm(cbind(`8`,`7.8`,`7.6`) ~ hpf, data=df)

Anova(mod, test.statistic = 'Roy')

Anova(update(mod, `108` ~ .))

# Anova(update(mod, `7.6` ~ .))
# Anova(update(mod, `7.8` ~ .))
# Anova(update(mod, `8` ~ .))

# heplot(candisc(mod, data = df1, term = 'pH', ndim = "1"))

can <- candisc(mod, data = df1, term = 'pH', ndim = "2")

# plot(can, col = rev(pHpalette))

# From this we can see:
  
# The first dimension (Can1) separates pH 7.6 from the other two treatments Can2 quite separates pH 7.8 from 8.0


ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'hpf') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'A) Growth') %>% as_tibble() -> scores_df

x_label <- paste0("Can1 (", round(can$pct[1], 1), "%)")
y_label <- paste0("Can2 (", round(can$pct[2], 1), "%)")

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH),  alpha = 0.5) +
  scale_color_manual("", values = rev(pHpalette)) +
  scale_fill_manual("", values = rev(pHpalette)) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, color = pH, fill = pH)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2),
    arrow = arrow(length = unit(0.07, "cm"))) +
  geom_text(data = segment_df, 
    aes(x = Can1+sign(Can1)*0.2, 
        y = Can2+sign(Can1)*0.2, label = hpf), 
    family = "GillSans") +
  coord_fixed() +
  labs(x = x_label, y = y_label) +
  theme(panel.border = element_blank(), legend.position = 'none')  -> p1

p1  + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'white', color = 'white')) +
  xlim(c(-2.5, 5)) +
  ylim(c(-2.5, 2.5)) -> p1

ggsavepath <- paste0(getwd(), '/Figures/')

ggsave(p1, path = ggsavepath, filename = 'candisc_growth.png',width = 4,height = 4, dpi = 300)

# Calcification candisc ----

birefrigence %>%
  group_by(pH, hpf, g) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = hpf, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df2

mod <- lm(cbind(`48`,`60`,`108`) ~ pH , data=df2)

can <- candisc(mod, data = df2, term = 'pH', ndim = "2")

ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'hpf') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'B) Calcification') %>% as_tibble() -> scores_df

x_label <- paste0("Can1 (", round(can$pct[1], 1), "%)")
y_label <- paste0("Can2 (", round(can$pct[2], 1), "%)")

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH),  alpha = 0.5) +
  scale_color_manual("", values = rev(pHpalette)) +
  scale_fill_manual("", values = rev(pHpalette)) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, color = pH, fill = pH)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2),
    arrow = arrow(length = unit(0.07, "cm"))) +
  geom_text(data = segment_df, 
    aes(x = Can1+sign(Can1)*0.2, 
      y = Can2+sign(Can1)*0.2, label = hpf), 
    family = "GillSans") +
  coord_fixed() +
  labs(x = x_label, y = y_label) +
  theme(panel.border = element_blank(), 
    legend.position = 'top')  -> p2

p2  + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'white', color = 'white')) +
  xlim(c(-2.5, 5)) +
  ylim(c(-2.5, 2.5)) -> p2

ggsavepath <- paste0(getwd(), '/Figures/')


ggsave(p2, path = ggsavepath, filename = 'candisc_calcification.png',width = 4,height = 4, dpi = 300)


# Symmetry candisc ----

body_ratio %>%
  group_by(pH, hpf, g) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = hpf, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df3

mod <- lm(cbind(`30`,`48`,`60`,`108`) ~ pH , data=df3)


can <- candisc(mod, data = df3, term = 'pH', ndim = "2")

ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'hpf') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'C) Symmetry') %>% as_tibble() -> scores_df

x_label <- paste0("Can1 (", round(can$pct[1], 1), "%)")
y_label <- paste0("Can2 (", round(can$pct[2], 1), "%)")

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH),  alpha = 0.5) +
  scale_color_manual("", values = rev(pHpalette)) +
  scale_fill_manual("", values = rev(pHpalette)) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, color = pH, fill = pH)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2),
    arrow = arrow(length = unit(0.07, "cm"))) +
  geom_text(data = segment_df, 
    aes(x = Can1+sign(Can1)*0.35, 
      y = Can2+sign(Can1)*0.35, label = hpf), 
    family = "GillSans") +
  coord_fixed() +
  labs(x = x_label, y = y_label) +
  theme(panel.border = element_blank(), legend.position = 'none')  -> p3

p3  + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'white', color = 'white')) + 
  xlim(c(-2.5, 5)) +
  ylim(c(-2.5, 2.5)) -> p3

ggsavepath <- paste0(getwd(), '/Figures/')

ggsave(p3, path = ggsavepath, filename = 'candisc_symmetry.png',width = 4,height = 4, dpi = 300)

# plot(can, col = rev(pHpalette))

# Facet ----

library(patchwork)

p1+p2+p3 -> collect_p


ggsave(collect_p, path = ggsavepath, filename = 'candisc_collect_metrics.png',width = 10,height = 4, dpi = 300)

# lm values ----

mod1 <- lm(cbind(`48`,`60`,`108`) ~ pH , data=df1)
mod2 <- lm(cbind(`48`,`60`,`108`) ~ pH , data=df2)
mod3 <- lm(cbind(`48`,`60`,`108`) ~ pH , data=df3)

cqplot(mod1)
heplot(mod1)
pairs(mod1)

# mod <- lm(cbind(`8`,`7.8`,`7.6`) ~ hpf, data=df)

Anova(mod1, test.statistic = 'Roy')
Anova(mod2, test.statistic = 'Roy')
Anova(mod3, test.statistic = 'Roy')

# OR


# # GLM ----

library(tidymodels)

# En estadística, la regresión logística (abarca el conjunto de glm) es un tipo de análisis de regresión utilizado para estimar el efecto

fit <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH*hpf, data = body_size)

fit2 <- linear_reg() %>%
  set_engine("lm") %>% # log_mod
  fit(measure ~ pH*hpf, data = birefrigence)


fit3 <- linear_reg() %>%
  set_engine("lm") %>% # log_mod
  fit(measure ~ pH*hpf, data = body_ratio)


Anova(fit$fit, test.statistic = 'F') %>% view()
Anova(fit2$fit, test.statistic = 'F') %>% view()
Anova(fit3$fit, test.statistic = 'F') %>% view()



fit %>% tidy() %>% add_significance("p.value") %>%
  mutate(g = 'Growth') %>%
  rbind(fit2 %>% tidy() %>% add_significance("p.value") %>%
      mutate(g = 'Calcification'),
        fit3 %>% tidy() %>% add_significance("p.value") %>%
      mutate(g = 'Symmetry')) %>%  view()
  separate(col = term, sep = ":", into = c("pH", "hpf")) %>%
  drop_na(hpf) %>%
  mutate(pH = sub('pH','', pH), hpf = sub('hpf','', hpf)) %>% 
  mutate(ymax = estimate+std.error, 
    ymin  = estimate-std.error) %>% 
  ggplot(aes(y = estimate, x = hpf, 
    color = pH, group = pH)) +
  geom_point(position = position_dodge2(width = 0.4)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.4,size = 0.7, 
    # color = 'black',
    position = position_dodge2(width = 0.4)) +
  geom_text(aes(y = ymax, label= p.value.signif), 
    position = position_dodge2(width = 0.4),
    vjust = -0.5) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  labs(y = 'Estimate efect', x = '') +
  facet_wrap(~ g, scales = 'free')

# clustering using parameters as STRUCTURE ------
# SEPARATING BY TIME

  
df <- rbind(df1, df2, df3) # %>% mutate_if(is.double, log)


rbind(body_size, body_ratio, birefrigence) %>%
  group_by(pH, hpf, g) %>% # tally()
  mutate(g = str_to_sentence(g)) %>%
  sample_n(50, replace = TRUE) %>%
  pivot_wider(
    names_from = g, 
    values_from = measure, 
    values_fn = list) %>%
  unnest(cols = everything()) -> data


# candisc 30 hpf -----

data %>% filter(hpf == 30) -> dat

mod <- lm(cbind(`Growth`,`Symmetry`) ~ pH , data=dat)

can <- candisc(mod, data = dat, term = 'pH', ndim = "2")


# heplot(candisc(mod, data = data, term = 'pH', ndim = "1"))

# plot(can, col = rev(pHpalette))


ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'Metric') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'A) 30 hpf') %>% as_tibble() -> scores_df

x_label <- paste0("Can1 (", round(can$pct[1], 1), "%)")
y_label <- paste0("Can2 (", round(can$pct[2], 1), "%)")

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH),  alpha = 0.5) +
  scale_color_manual("", values = rev(pHpalette)) +
  scale_fill_manual("", values = rev(pHpalette)) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, color = pH, fill = pH)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2),
    arrow = arrow(length = unit(0.07, "cm"))) +
  geom_text(data = segment_df, 
    aes(x = Can1+sign(Can1)*0.2, 
      y = Can2+sign(Can1)*0.2, label = Metric), 
    family = "GillSans") +
  coord_fixed() +
  labs(x = x_label, y = y_label) +
  theme(panel.border = element_blank(), legend.position = 'none')  -> p1

p1 + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'white', color = 'white')) +
  xlim(c(-3, 3)) +
  ylim(c(-2.5, 2.5)) -> p1


ggsave(p1, path = ggsavepath, 
  filename = 'candisc_30hpf.png',width = 4,height = 4, dpi = 300)

# candisc 48 hpf -----


data %>% filter(hpf == 48) -> dat

mod <- lm(cbind(`Growth`,`Symmetry`,`Calcification`) ~ pH , data=dat)

can <- candisc(mod, data = dat, term = 'pH', ndim = "2")


ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'Metric') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'B) 48 hpf') %>% as_tibble() -> scores_df

x_label <- paste0("Can1 (", round(can$pct[1], 1), "%)")
y_label <- paste0("Can2 (", round(can$pct[2], 1), "%)")

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH),  alpha = 0.5) +
  scale_color_manual("", values = rev(pHpalette)) +
  scale_fill_manual("", values = rev(pHpalette)) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, color = pH, fill = pH)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2),
    arrow = arrow(length = unit(0.07, "cm"))) +
  geom_text(data = segment_df, 
    aes(x = Can1+sign(Can1)*0.2, 
      y = Can2+sign(Can1)*0.2, label = Metric), 
    family = "GillSans") +
  coord_fixed() +
  labs(x = x_label, y = y_label) +
  theme(panel.border = element_blank(), legend.position = 'none')  -> p2

p2 + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'white', color = 'white')) +
  xlim(c(-3, 3)) +
  ylim(c(-2.5, 2.5)) -> p2


ggsave(p2, path = ggsavepath, 
  filename = 'candisc_48hpf.png',width = 4,height = 4, dpi = 300)

# candisc 60 hpf -----


data %>% filter(hpf == 60) -> dat

mod <- lm(cbind(`Growth`,`Symmetry`,`Calcification`) ~ pH , data=dat)

can <- candisc(mod, data = dat, term = 'pH', ndim = "2")


ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'Metric') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'C) 60 hpf') %>% as_tibble() -> scores_df

x_label <- paste0("Can1 (", round(can$pct[1], 1), "%)")
y_label <- paste0("Can2 (", round(can$pct[2], 1), "%)")

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH),  alpha = 0.5) +
  scale_color_manual("", values = rev(pHpalette)) +
  scale_fill_manual("", values = rev(pHpalette)) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, color = pH, fill = pH)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2),
    arrow = arrow(length = unit(0.07, "cm"))) +
  geom_text(data = segment_df, 
    aes(x = Can1+sign(Can1)*0.2, 
      y = Can2+sign(Can1)*0.2, label = Metric), 
    family = "GillSans") +
  coord_fixed() +
  labs(x = x_label, y = y_label) +
  theme(panel.border = element_blank(), legend.position = 'none')  -> p3

p3 + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'white', color = 'white')) +
  xlim(c(-3, 3)) +
  ylim(c(-2.5, 2.5)) -> p3


ggsave(p3, path = ggsavepath, 
  filename = 'candisc_60hpf.png',width = 4,height = 4, dpi = 300)

# candisc 108 hpf -----


data %>% filter(hpf == 108) -> dat

mod <- lm(cbind(`Growth`,`Symmetry`,`Calcification`) ~ pH , data=dat)

can <- candisc(mod, data = dat, term = 'pH', ndim = "2")

# heplot(candisc(mod, data = data, term = 'pH', ndim = "1"))

# plot(can, col = rev(pHpalette))

ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'Metric') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'D) 108 hpf') %>% as_tibble() -> scores_df

x_label <- paste0("Can1 (", round(can$pct[1], 1), "%)")
y_label <- paste0("Can2 (", round(can$pct[2], 1), "%)")

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH),  alpha = 0.5) +
  scale_color_manual("", values = rev(pHpalette)) +
  scale_fill_manual("", values = rev(pHpalette)) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, color = pH, fill = pH)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2),
    arrow = arrow(length = unit(0.07, "cm"))) +
  geom_text(data = segment_df, 
    aes(x = Can1+sign(Can1)*0.2, 
      y = Can2+sign(Can1)*0.2, label = Metric), 
    family = "GillSans") +
  coord_fixed() +
  labs(x = x_label, y = y_label) +
  theme(panel.border = element_blank(), legend.position = 'none')  -> p4

p4 + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'white', color = 'white')) +
  xlim(c(-3, 3)) +
  ylim(c(-2.5, 2.5)) -> p4


ggsave(p4, path = ggsavepath, 
  filename = 'candisc_108hpf.png',width = 4,height = 4, dpi = 300)

# Facet ----

library(patchwork)

p1+p2+p3+p4 + plot_layout(guides = "collect", nrow = 1) -> collect_p


ggsave(collect_p, path = ggsavepath, filename = 'candisc_collect_hpf.png',
  width = 12.5,height = 4, dpi = 300)


# then

rbind(body_size, body_ratio, birefrigence) %>%
  group_by(pH, hpf, g) %>% # tally()
  mutate(g = str_to_sentence(g)) %>%
  sample_n(50, replace = TRUE) -> fit_dat

data %>% filter(hpf == 108) -> dat

data %>% 
  pivot_longer(cols = c(`Growth`,`Symmetry`,`Calcification`), 
    values_to = 'measure',
    names_to = 'Metric') %>%
  drop_na(measure) -> fit_dat

fit <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH * hpf * Metric, data = fit_dat)

Anova(fit$fit, test.statistic = 'F') 

fit1 <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH * Metric, data = filter(fit_dat, hpf == "30"))

fit2 <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH * Metric, data = filter(fit_dat, hpf == "48"))

fit3 <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH * Metric, data = filter(fit_dat, hpf == "60"))

fit4 <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH * Metric, data = filter(fit_dat, hpf == "108"))


Anova(fit1$fit, test.statistic = 'F') %>% view()
Anova(fit2$fit, test.statistic = 'F') %>% view()
Anova(fit3$fit, test.statistic = 'F') %>% view()
Anova(fit4$fit, test.statistic = 'F') %>% view()



fit %>% tidy() %>% add_significance("p.value") %>%
  mutate(g = 'Growth') %>%
  rbind(fit2 %>% tidy() %>% add_significance("p.value") %>%
      mutate(g = 'Calcification'),
    fit3 %>% tidy() %>% add_significance("p.value") %>%
      mutate(g = 'Symmetry')) %>%  view()
separate(col = term, sep = ":", into = c("pH", "hpf")) %>%
  drop_na(hpf) %>%
  mutate(pH = sub('pH','', pH), hpf = sub('hpf','', hpf)) %>% 
  mutate(ymax = estimate+std.error, 
    ymin  = estimate-std.error) %>% 
  ggplot(aes(y = estimate, x = hpf, 
    color = pH, group = pH)) +
  geom_point(position = position_dodge2(width = 0.4)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.4,size = 0.7, 
    # color = 'black',
    position = position_dodge2(width = 0.4)) +
  geom_text(aes(y = ymax, label= p.value.signif), 
    position = position_dodge2(width = 0.4),
    vjust = -0.5) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  labs(y = 'Estimate efect', x = '') +
  facet_wrap(~ g, scales = 'free')

# then stats

data
