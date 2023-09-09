
rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(readr.show_col_types = FALSE, stringsAsFactors = FALSE)


pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4", `8`= "#4575b4")



library(tidyverse)
library(candisc)

set.seed(123)

level_key <- c("24" = "30")

setwd("~/Documents/GitHub/OA-research/rproject/")

body_size <- read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  select(hpf, pH, Index) %>%
  # filter(hpf != 24) %>% 
  mutate(hpf = recode_factor(hpf, !!!level_key)) %>%
  mutate(g = 'growth') %>% rename('measure' = 'Index')

birefrigence <- read_rds(paste0(getwd(), '/birefrigence.rds')) %>% 
  select(Area) %>% mutate(g = 'calcification') %>% 
  rename('measure' = 'Area')

standard_ratio <- (1+sqrt(5))/2 # it is equal to 1.618033988749

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  mutate(
    golden_ratio = (Length+Width)/Length,
    assymetry = golden_ratio/standard_ratio) %>% 
  select(hpf, pH, golden_ratio) %>%
  mutate(g = 'Symmetry') %>% 
  mutate(hpf = recode_factor(hpf, !!!level_key)) %>%
  rename('measure' = 'golden_ratio')-> body_ratio

birefrigence %>% 
  group_by(pH, hpf) %>%
  mutate(ntile = ntile(measure, 2)) %>%
  filter(ntile > 1) %>%  select(-ntile) %>%
  sample_n(20, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df1 

body_size %>% 
  group_by(pH, hpf) %>%
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df2

body_ratio %>%
  group_by(pH, hpf) %>% 
  sample_n(50, replace = TRUE) %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df3


#  "hpf"     "pH"      "measure" "g" 

read_csv(f, comment = "#", col_names = T) %>%
  select(hpf, pH, `% Success`) %>%
  mutate(g = 'Development') %>%
  rename("measure" = "% Success") %>%
  mutate_at(vars(c("hpf", "pH")), as.factor) %>%
  group_by(hpf, pH) -> proportions_df

proportions_df %>%
  pivot_wider(names_from = pH, values_from = measure, values_fn = list) %>%
  unnest(cols = everything()) -> df4

# read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
#   mutate(
#     golden_ratio = (Length+Width)/Length,
#     assymetry = golden_ratio / standard_ratio) %>% 
#   ggplot(aes(y = golden_ratio, x = pH, color = pH)) + 
#   stat_boxplot(geom ='errorbar', width = 0.3) +
#   geom_boxplot(width = 0.3, outlier.alpha = 0) +
#   geom_jitter(alpha = 0.5) +
#   geom_hline(yintercept = standard_ratio, colour = "grey50", linetype = 2) +
#   ylim(c(1.55, 2)) + facet_grid(~ hpf)
# 
# read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
#   mutate(
#     golden_ratio = (Length+Width)/Length,
#     assymetry = golden_ratio / standard_ratio) %>% 
#   ggplot(aes(Index, golden_ratio, color = pH)) + 
#   facet_grid(~ hpf, scales = "free_x") +
#   geom_point(alpha = 0.5) +
#   ylim(c(1.55, 2)) +
#   geom_smooth(method = "lm") +
#   ggpubr::stat_cor()
#   

# data <- rbind(df1, df2, df3, df4) # %>% mutate_if(is.double, log)

rbind(body_size, birefrigence, body_ratio) %>%
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
  rownames_to_column(var= 'Metric') %>%
  filter(Metric %in% "Growth") -> segment_df

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


# ggsave(p1, path = ggsavepath, 
#   filename = 'candisc_30hpf.png',width = 4,height = 4, dpi = 300)

# candisc 48 hpf -----


data %>% filter(hpf == 48) -> dat

mod <- lm(cbind(`Growth`,`Calcification`) ~ pH , data=dat)

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


# ggsave(p2, path = ggsavepath, 
  # filename = 'candisc_48hpf.png',width = 4,height = 4, dpi = 300)

# candisc 60 hpf -----


data %>% filter(hpf == 60) -> dat

mod <- lm(cbind(`Growth`,`Calcification`) ~ pH , data=dat) # `Symmetry`

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


# ggsave(p3, path = ggsavepath, 
#   filename = 'candisc_60hpf.png',width = 4,height = 4, dpi = 300)

# candisc 108 hpf -----


data %>% filter(hpf == 108) -> dat

mod <- lm(cbind(`Growth`,`Calcification`) ~ pH , data=dat) # `Symmetry`

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


# ggsave(p4, path = ggsavepath, 
#   filename = 'candisc_108hpf.png',width = 4,height = 4, dpi = 300)

# Facet ----

library(patchwork)

p1+p2+p3+p4 + plot_layout(guides = "collect", nrow = 2) -> collect_p

ggsavepath <- paste0(getwd(), '/Figures/')

ggsave(collect_p, path = ggsavepath, filename = 'candisc_collect_hpf.png',
  width = 12.5,height = 7, dpi = 300, device = png)
