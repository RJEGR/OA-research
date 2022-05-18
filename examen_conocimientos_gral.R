

# CLEAR OBJECT LIST AND IMAGE CANVAS   
rm(list = ls());
if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)

library(tidyverse)
library(patchwork)

# Question 1 ----

# Test codap data
# # test viz from: https://odv.awi.de/
# DIC and TALL were variables calculated
# vars that are calculated from CO2SYS with DIC and TALK were:
# pH', 'fCO2', 'Carbonate', 'Aragonite', 'Calcite'


pathout <- "/Users/cigom/Documents/GitHub/OA-research/rproject"
pacific_df <- read_rds(paste0(pathout, '/pacific_codap_na_v2021.rds'))

pacific_df %>% count(oce)
pacific_df %>% distinct(var)

# vars <- c('pH', 'TALK', 'DIC') # 'Temp', 'Oxygen', 'fCO2'

vars <- c('pH', 'fCO2', 'Carbonate')


sbt <- pacific_df %>%   filter(var %in% vars) 
sbt$var <- factor(sbt$var, levels = levels(factor(sbt$var)))

# expression(Omega["ara"])
# expression(CO[3]^{-2}~(µmol~Kg^{-1})
# expression(pCO[2]~(uatm)
# expression(O[2]~(µmol~Kg^{-1}))

levels(sbt$var)
levels(sbt$var) <- c(expression(pCO[2]~(uatm)), "pH", expression(CO[3]^{-2}~(µmol~Kg^{-1})))


sbt %>% 
  filter(round(Latitude) == 25) %>%
  filter(value > 0) %>%
  ggplot(aes(y = Depth, x = value)) +
  facet_grid(.~ var, scales = 'free_x', labeller=label_parsed,  switch = 'x') +
  geom_point(shape = 0.7) +
  # scale_y_reverse(breaks = seq(0, 200, by = 50)) +
  scale_y_reverse(breaks = seq(0, 3500, by = 500)) +
  scale_x_continuous(position = 'top') +
  geom_smooth(orientation = "y", method = 'loess') +
  labs(caption = 'Perfil vertical del Pacifico Norte, (25°N)',
    y = expression(~Profundidad~(dbar)),
    x = '') +
  theme_bw(base_family = "GillSans", base_size = 10) +
  theme(strip.background = element_blank()) -> p1

p1

pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")

pacific_df %>% 
  filter(round(Latitude) == 25) %>%
  filter(var %in% c('pH', 'TALK', 'DIC')) %>%
  filter(value > 0) %>%
  drop_na(value) %>%
  pivot_wider(values_from = value, names_from = var) %>%
  ggplot(aes(DIC, TALK, color = pH)) +
  geom_smooth(method = 'lm', se = F, color = 'black') +
  ggpubr::stat_cor() +
  geom_point() +
  scale_color_gradientn(colours =  pal) +
  # facet_grid(~oce) +
  theme_bw(base_family = "GillSans", base_size = 10) +
  theme(strip.background = element_blank()) -> p2

p1 + p2 + 
  patchwork::plot_layout(widths = c(3,1))

# pH vs carbonate
# Carbonate_insitu_calculated
# CO3-2
# the continental shelf of western North America from Queen Charlotte Sound, Canada, to San Gregorio Baja California Sur, Mexico

caption <- 'Plataforma continental: Latitudes 25N a 32N'

pacific_df %>% 
  filter(round(Latitude) <= 32.5) %>%
  filter(pH > 0) %>%
  filter(oce %in% c('Oc. Pacifico Norte', 'U.S. Costa Oeste')) %>%
  filter(Depth > 20 & Depth <= 100) %>% 
  ggplot(aes(y = pH, x = Carbonate, color = Aragonite)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(shape = 0.7) +
  scale_x_reverse() +
  scale_color_gradientn(expression(Omega["ara"]),colours =  rev(pal)) +
  labs(x = expression(CO[3]^{-2}~(µmol~Kg^{-1})), y = "pH", caption = caption) +
  theme_classic(base_family = "GillSans", base_size = 18) +
  guides(color = guide_colorbar(barheight = unit(2, "in"), 
    barwidth = unit(0.25, "in"), 
    ticks.colour = "black", 
    frame.colour = "black",
    label.theme = element_text(size = 10))) 


# Question 6 ----
d1 <- data.frame(
  pH = rnorm(1e5, mean = 2, sd = 0.5),
  g = 'Vertebrate pH'
)


# quantile(rnorm(1e5, mean = 2, sd = 1))

d2 <- data.frame(
  pH = rnorm(1e5, mean = 11, sd = 0.5),
  g = 'Invertebrate pH'
)

d3 <- data.frame(
  pH = rnorm(1e5, mean = 6.6, sd = 0.5),
  g = ' pH Embrion'
)

d4 <- data.frame(
  pH = rnorm(1e5, mean = 7.2, sd = 0.5),
  g = ' pH Huevo'
)


df <- rbind(d1,d2, d3,d4) %>% as_tibble()

library(ggplot2)

label <- 'Optimal'

ggplot(df, aes(x = pH, fill = g)) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(0,14, by = 2)) +
  labs(y = 'Actividad relativa enzimatica') +
  theme_classic(base_size = 16) +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0.75, linetype="dashed", color = "black") +
  annotate(geom = 'text', 
    x = 9, y = 0.85, label = label, size=5, col="Black") +
  geom_density(show.legend = T, alpha = 0.7) +
  scale_fill_manual(values = wesanderson::wes_palette("Rushmore1"))

# qbox <- function(x) {quantile(x, probs = seq(0, 0.5, 0.1))}

# df %>% 
#   group_by(g) %>%
#   summarise(q = qbox(pH)) %>%
#   ggplot(aes(x = q, y = ))

    