
library(tidyverse)

path <- "/Users/cigom/Documents/GitHub/OA-research/rproject"

pacific_df <- read_rds(paste0(path, '/pacific_codap_na_v2021.rds'))

# Carbonate_insitu_calculated
# CO3-2
# the continental shelf of western North America from Queen Charlotte Sound, Canada, to San Gregorio Baja California Sur, Mexico

vars <- c('Aragonite', 'Calcite', 'DIC') # 'pH', 'Carbonate',

var_levels <- c(
  "Aragonite" = expression(Omega["ara"]),
  "Calcite" = expression(Omega["cal"]),
  "DIC")

caption <- c('Plataforma continental: Latitudes 25N a 32N, 20 to 200 meters depth')



viz <- pacific_df %>% 
  filter(round(Latitude) <= 32.5) %>%
  filter(pH > 0) %>%
  filter(oce %in% c('Oc. Pacifico Norte', 'U.S. Costa Oeste')) %>%
  filter(Depth > 20 & Depth <= 100) %>% 
  pivot_longer(cols = all_of(vars), 
    names_to = "vars", values_to = "measure") %>%
  select(pH, measure, Carbonate, vars, pH)


viz$vars <- factor(viz$vars, levels = levels(factor(viz$vars)))


levels(viz$vars) <- var_levels
  
viz %>%
  # ggplot(aes(Aragonite, Carbonate, color = pH)) +
  ggplot(aes(measure, Carbonate, color = pH)) +
  facet_grid(~ vars, scales = "free_x", labeller=label_parsed) +
  # geom_vline(xintercept = 1, linetype = 'dashed') 
  geom_point(shape = 0.7) +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  labs(y = expression(CO[3]^{-2}~(µmol~Kg^{-1})),
    x = "Medición",
    caption = caption) +
  theme_minimal(base_family = "GillSans", base_size = 18) +
  theme(legend.position = 'top', 
    legend.text = element_text(family = "GillSans"),
    strip.background = element_rect(fill = 'white', color = 'white')) +
  guides(color = guide_colorbar(barheight = unit(0.25, "in"), 
    barwidth = unit(7, "in"), 
    ticks.colour = "black", 
    frame.colour = "black",
    label.theme = element_text(size = 15))) -> ps


ggsave(ps, path = path, 
  filename = 'FIGURE_3_TESIS.png', width = 10, height = 5, device = png)


# 
# ggsave(psva, path = path, 
#   filename = 'carbonato_vs_DIC.png', width = 7, height = 5)


# ggsave(psave, path = path, 
  # filename = 'carbonato_vs_ara.png', width = 7, height = 5)
