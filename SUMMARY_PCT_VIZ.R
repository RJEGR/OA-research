rm(list = ls())

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)

library(tidyverse)

library(rstatix)


ggsavepath <- paste0(getwd(), '/Figures')

pattern <- "SUMMARY_PCT_TABLES.csv"

files <- list.files(path = getwd(), pattern = pattern, full.names = T)

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",`8.0`= "#4575b4", `8`= "#4575b4")


pHLevel <- c("8.0", "7.8", "7.6")

hpfL <- c("24",  "48",  "60", "108", "624")

pHpalette <- pHpalette[match( pHLevel, names(pHpalette))]

read_csv(files, comment = "#") %>% 
  filter(Development %in% "A") %>%
  filter(!g %in% "Eclosion") %>%
  select(-Replica, -dpf) %>%
  pivot_longer(cols = pHLevel, names_to = "pH", values_to = "pct") %>%
  mutate(pH = factor(pH, levels = pHLevel)) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  mutate(Stress = factor(Stress, levels = c("Chronic", "Acute"))) -> df

df %>%
  group_by(Stress, pH, hpf, g) %>%
  rstatix::get_summary_stats(type = "quantile") %>% # "mean_ci"
  ggplot(aes(x = hpf, y = `50%`, color = pH, group = pH)) +
  facet_grid(~ Stress, scales = 'free_x',space = "free_x" ) +
  geom_point(position = position_dodge(width = 0.3), size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
    width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual("", values = pHpalette) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'Quantiles (25,50,75)') -> ps


ps + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(), legend.position = 'top') -> ps



ggsave(ps, filename = 'percentage.png', path = ggsavepath, 
  width = 5.5, height = 3)
