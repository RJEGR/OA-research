# analyze chemistry ----

path <- '~/Documents/DOCTORADO/pH_measures/'

head(df <- read.csv(paste0(path, 'quimicas.csv')))

ylabs = expression(CO[3]^{-2}~(µmol~Kg^{-1}))
xlabs = expression(Omega["ara"])

# Lnames <- c(expression(Omega["ara"]),
#   expression(CO[3]^{-2}~(µmol~Kg^{-1})),
#   expression(pCO[2]~(uatm)),
#   expression(O[2]~(µmol~Kg^{-1})), "pH")


df %>%
  select(ID, c('CO3','DIC', 'TA', 'Ara')) %>%
  separate(col = ID, 
    into = c('hpf', 'pH', 'g'), sep = "-") %>%
  pivot_longer(cols = c('DIC', 'TA', 'Ara')) %>%
  ggplot(aes(value, `CO3`)) +
  geom_smooth(method = "lm", linetype="dashed", size = 0.5, alpha=0.5, 
    se = F, na.rm = TRUE) +
  geom_point() +
  geom_rug() +
  facet_grid(~name, scales = 'free_x') +
  theme_bw(base_family = "GillSans", base_size = 10) +
  theme(legend.position = 'none') +
  labs(y = ylabs, x = '')

df %>%
  ggplot(aes(DIC, TA, group = name)) +
  geom_smooth(method = "lm", linetype="dashed", color = 'black', se = T) +
  ggpubr::stat_cor(method = "pearson", cor.coef.name = "R", 
    p.accuracy = 0.001, label.y = 2300) +
  # ggrepel::geom_label_repel(aes(label = ID, color = pH)) +
  geom_point(size = 2.5, alpha = 0.5, aes(color = name)) +
  theme_bw(base_family = "GillSans", base_size = 10) +
  scale_color_manual('', values = structure(getPalette[-1], names =  Labels)) +
  facet_grid(~name)

# in comparison to 

dviz %>% 
  ungroup() %>%
  select(date, name, a) %>%
  pivot_wider(names_from = name, values_from = a) 

level_key <- structure(Labels, names = c('Canal-2', 'Canal-3', 'Canal-4'))

# como no hay pH de control al inicio, no vas a tener join con los datos de df
# sum(dviz$date %in% df$date)

dviz %>% ungroup() %>%
  mutate(name = recode_factor(name, !!!level_key)) %>%
  select(date, name, a) %>%
  inner_join(df) %>%
  ggplot(aes(a, DIC)) +
  geom_point(aes(color = name)) +
  geom_smooth(method = "lm", linetype="dashed", size = 0.5, alpha=0.5,
    se = F, na.rm = TRUE) +
  ggpubr::stat_cor(method = "pearson", cor.coef.name = "R", p.accuracy = 0.001)

# normalizar datos a la salinidad

df %>%
  mutate(nDIC = mean(df$Sal)*(DIC/Sal), nTA = mean(df$Sal)*(TA/Sal)) %>%
  select(name, nTA, nDIC, TA, DIC) %>%
  ggplot(aes(color = name)) +
  # geom_point(aes(nTA, TA))
  geom_point(aes(nDIC, DIC))

boxplot(df$Sal)
mean(df$Sal)

