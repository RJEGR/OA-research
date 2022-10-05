
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

body_size <- read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%select(hpf, pH, Index) %>%
  filter(hpf != 24) %>% mutate(g = 'growth') %>% rename('measure' = 'Index')
birefrigence <- read_rds(paste0(getwd(), '/birefrigence.rds')) %>% select(Area) %>% mutate(g = 'calcification') %>% rename('measure' = 'Area')

# BODY SIZE ----

# phi <- 1.618033988749
# # 
# df_filtered %>%
#   mutate(
#     golden_ratio = (Length+Width)/Length,
#     predicted_Width = Length/phi,
#     predicted_Length = Width*phi
#   ) %>%
#   ggplot(aes(golden_ratio, color = pH)) + 
#   stat_ecdf(pad = F) + 
#   geom_vline(xintercept = phi, colour = "grey50", linetype = 2)

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

birefrigence %>% # birefrigence not grouped good!
  # mutate(ntile = ntile(measure, 2)) %>%
  group_by(pH, hpf) %>% # tally()
  sample_n(20, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df1 
  # filter(ntile > 1) %>% ungroup() %>% select(-ntile) -> df1 

body_size %>% # birefrigence not grouped good!
  group_by(pH, hpf, g) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df2

df <- rbind(df1, df2) # %>% mutate_if(is.double, log)

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

# # GLM ----

library(tidymodels)

# En estadística, la regresión logística (abarca el conjunto de glm) es un tipo de análisis de regresión utilizado para estimar el efecto

fit <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH, data = body_size)

# performance::check_model(fit)

Anova(fit$fit)

fit %>% tidy() %>% add_significance("p.value") %>%
  mutate(g = 'Growth') -> out

# fit %>% tidy() %>% add_significance("p.value") %>%
#   dotwhisker::dwplot(dot_args = list(size = 2, color = "black"),
#     whisker_args = list(color = "black"),
#     vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
#   geom_text(aes(y = term, x = std.error + 0.05, 
#     label= p.value.signif))
# theme_classic(base_size = 16, base_family = "GillSans") 

fit <- linear_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(measure ~ pH, data = birefrigence)

# fit <- linear_reg() %>%
#   set_engine("glm") %>% # log_mod
#   fit(measure ~ pH*g, data = rbind(birefrigence, body_size))

Anova(fit$fit)

fit %>% tidy() %>% add_significance("p.value") %>%
  mutate(g = 'Calcification') %>%
  rbind(out) %>%
  filter(grepl('pH', term)) %>%
  mutate(ymax = estimate+std.error, 
    ymin  = estimate-std.error) %>%
  mutate(term = sub('pH','', term)) %>%
  ggplot(aes(y = estimate, x = g, color = term, group = term)) +
  geom_point(position = position_dodge2(width = 0.4)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.4,size = 0.7, 
    # color = 'black',
    position = position_dodge2(width = 0.4)) +
  geom_text(aes(y = std.error + 0.05, 
    label= p.value.signif), position = position_dodge2(width = 0.4)) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  labs(y = 'Estimate efect', x = '')
  
# MANOVA -----
library(tidymodels)

fit <- linear_reg() %>%
  set_engine("lm") %>% 
  fit(measure ~ pH * hpf, data = body_size)

performance::check_model(fit)

Anova(fit$fit)

# candisc canonical discriminant analysis (CDA) -----
# install.packages("candisc")
library(candisc)
# ref: Near-future level of CO2-driven ocean acidification radically affects larval survival and development in the brittlestar Ophiothrix fragilis (dpont 2008)

# Canonical discriminant analysis of the morphometric parameters Figure: used to separate the different pH treatments and time post fertiliza- tion. At Day 1, all the larvae from the 3 pH treatments are clustered together indi- cating that they share similar body proportions. At Day 4, larvae raised at pH 7.7 are discriminated from those growing at pH 8.1 (all days) and pH 7.9 (Day 4). Larvae raised at pH 7.7 possessed proportions that were never observed in those raised at normal pH.

# Method: Canonical discriminant analysis was used to assess the impacts of pH and/or exposure time on mor- phometric parameters

# Canonical discriminant analyses were performed on the morphometric parameters to assess individual variation within the treat- ments at Days 1 and 4 . At Day 1, all the larvae from the 3 pH treatments are clustered together indi- cating that they share similar body proportions. At Day 4, larvae raised at pH 7.7 are discriminated from those growing at pH 8.1 (all days) and pH 7.9 (Day 4). Larvae raised at pH 7.7 possessed proportions that were never observed in those raised at normal pH.

# mod <- lm(cbind(`8`,`7.8`,`7.6`) ~ hpf, data=df1)

# PCA ensena la metrica con mayor variacion, pero el analisis de discriminacion (lineal) ofrece una medida de separacion (u) entre dos grupos.. ie. LDA is like PCA but it focuses on maximizn the seperatibility among known categories (statquest)

body_size %>% # birefrigence not grouped good!
  group_by(pH, hpf, g) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df

mod <- lm(cbind(`8`,`7.8`,`7.6`) ~ hpf*g, data=df)

# mod <- lm(cbind(`48`,`60`,`108`) ~ pH , data=df2)

Anova(mod, test.statistic = 'Roy')
Anova(update(mod, `7.6` ~ .))

can <- candisc(mod, data = df, term = 'pH', ndim = "2")

# plot(can, col = rev(pHpalette))

# From this we can see:
  
# The first dimension (Can1) separates pH 7.6 from the other two treatments Can2 quite separates pH 7.8 from 8.0


str(can)

ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'hpf') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'Growth') %>% as_tibble() -> scores_df

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH), alpha = 0.5) +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, fill = pH), 
    color = NA) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2)) +
  geom_text(data = segment_df, 
    aes(x = Can1,y = Can2, label = hpf), 
    family = "GillSans") +
  coord_fixed() +
  labs(x ='', y = '') +
  theme(panel.border = element_blank(), legend.position = 'top')  -> p1

p1  + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'grey', color = 'white')) -> p1

ggsavepath <- paste0(getwd(), '/Figures/')


ggsave(p1, path = ggsavepath, filename = 'candisc_growth.png',width = 4,height = 4, dpi = 300)

# Calcification candisc

mod <- lm(cbind(`48`,`60`,`108`) ~ pH , data=df1)


can <- candisc(mod, data = df1, term = 'pH', ndim = "2")

ellipse_df <- can$means %>%
  data.frame() %>%
  rownames_to_column(var= 'pH')

data.frame(can$structure+can$coeffs.std ) %>%
  rownames_to_column(var= 'hpf') -> segment_df

can$scores %>% 
  mutate(pH = factor(pH, levels = rev(names(pHpalette))),
    g = 'Calcification') %>% as_tibble() -> scores_df

ggplot() + 
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = scores_df, 
    aes(x = Can1,y = Can2, color = pH), alpha = 0.5) +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  ggforce::geom_mark_ellipse(
    data = ellipse_df, aes(Can1,y = Can2, fill = pH), 
    color = NA) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  geom_segment(data = segment_df, 
    aes(x = 0, y = 0, xend = Can1, yend = Can2)) +
  geom_text(data = segment_df, 
    aes(x = Can1,y = Can2, label = hpf), 
    family = "GillSans") +
  coord_fixed() +
  labs(x ='', y = '') +
  theme(panel.border = element_blank(), legend.position = 'top')  -> p1

p1  + facet_grid(~ g) + theme(
  strip.background = element_rect(fill = 'grey', color = 'white')) -> p1

ggsavepath <- paste0(getwd(), '/Figures/')


ggsave(p1, path = ggsavepath, filename = 'candisc_calcification.png',width = 4,height = 4, dpi = 300)
