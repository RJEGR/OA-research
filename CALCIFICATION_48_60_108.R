
# test reducition of dimensions from birefrigency an dbody size


rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(readr.show_col_types = FALSE, stringsAsFactors = FALSE)


pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4", `8`= "#4575b4")

library(tidyverse)
library(factoextra)
library("FactoMineR")

set.seed(123)

body_size <- read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>% select(hpf, pH, Index) %>% 
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
 
# birefrigence %>% tally()

# rbind(birefrigence, body_size) %>%

birefrigence %>% # birefrigence not grouped good!
  mutate(ntile = ntile(measure, 2)) %>%
  group_by(pH, hpf, ntile) %>% # tally()
  sample_n(20, replace = TRUE) %>%
  pivot_wider(names_from = hpf, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) %>%
  filter(ntile > 1) -> df 

body_size %>% # birefrigence not grouped good!
  group_by(pH, hpf, g) %>% # tally()
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = hpf, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df


# "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

cmds <- run_cmds(df, k = 3)

cmds %>% 
  ggplot(aes(X1,X2, group = cutree)) + # shape = as.factor(df$ntile))
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

fit %>% tidy() %>% add_significance("p.value") %>%
  mutate(g = 'Calcification') %>%
  rbind(out) %>%
  filter(grepl('pH', term)) %>%
  mutate(ymax = estimate+std.error, 
    ymin  = estimate-std.error) %>%
  ggplot(aes(y = estimate, x = term)) +
  facet_grid(g ~ .) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "grey50", linetype = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.15,size = 0.7, position = position_dodge(width = 0.7), color = 'black') +
  geom_text(aes(x = term, y = std.error + 0.05, 
    label= p.value.signif)) +
  theme_classic(base_size = 16, base_family = "GillSans") 
  


