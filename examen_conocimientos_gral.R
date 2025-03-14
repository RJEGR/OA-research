

# CLEAR OBJECT LIST AND IMAGE CANVAS   
rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE)

ggsavepath <- paste0(getwd(), '/Figures')

library(tidyverse)
library(patchwork)

# Question 1 ----
# Fuerza ionica, coef de actividad y coef de equilibrio (o disociacion)

# df %>%
pattern <- 'kinetics_equilibrium_co2_seawater_activity.csv'  

fcsv <- list.files(path = getwd(), pattern = pattern, full.names = T)

df <- read_csv(fcsv, comment = '#', skip_empty_rows = T) %>%
  drop_na() %>%
  arrange(desc(Measured)) %>%
  mutate(id = 1:nrow(.))
# 

Labels <- df$Ion


index <- paste0(ifelse(df$Electron > 1, df$Electron, ''), df$Charge)


df %>% mutate(Ion = paste0(Ion,"^'", index,"'")) -> df

Lorder <- df$Ion
# 
# df %>% mutate(Ion = forcats::fct_recode())

cols <- c("Measured", "Calculated", "Davies eq")

df %>% 
  pivot_longer(cols = cols, values_to = 'gamma') %>%
  filter(grepl('Calc', name))-> df_longer

ggplot(data = df_longer, aes(y = gamma, x = id, group = name)) +
  # geom_path(size = 1, color = 'grey', linetype = 'dashed') +
  geom_point(size = 2) +
  ggrepel::geom_label_repel(
    aes(label = Ion, y = gamma, x = id), parse = T, 
    label.size = 0, fill = 'Transparent', family = "GillSans")  +
  scale_color_manual('', values = c('black','blue')) +
  labs(y = expression('Total Activity'~ (gamma[t])), 
    x = "Ions in Seawater",
    caption = expression('T'[c] ~ '= 25 C and S = 35')) +
  theme_classic(base_size = 18, base_family = "GillSans") +
  theme(legend.position = 'top', axis.text.x = element_blank()) -> p1
 

ggsave(p1, filename = 'Total_activity.png', path = ggsavepath, width = 5, height = 3.5)

# then

pattern <- 'kinetics_equilibrium_co2_seawater_ion_strength.csv'

fcsv <- list.files(path = getwd(), pattern = pattern, full.names = T)

df2 <- read_csv(fcsv, comment = '#', skip_empty_rows = T) %>% drop_na()

Labels <- df2$Ion


index <- paste0(ifelse(df2$Electron > 1, df2$Electron, ''), df2$Charge)


df2 %>% mutate(Ion = paste0(Ion,"^'", index,"'")) %>%
  mutate(Ion = factor(Ion, levels = rev(Lorder))) %>%
  mutate(id = as.numeric(Ion)) %>% drop_na() -> df2



df2 %>%
  ggplot(aes(y = Ion, x = as.factor(I), fill = gama)) +
  geom_tile(color = 'white', size = 0.5) +
  scale_y_discrete(labels = sapply(rev(Lorder), function(i) parse(text=i))) +
  theme_classic(base_size = 12, base_family = "GillSans") +
  theme(legend.position = 'top') +
  labs(
    x = "Ionic strength", 
    y = "Ions in Seawater", fill = expression(gamma[f]),
    caption = expression('T'[c] ~ '= 25 C; Predicted Free Activity')) +
  scale_fill_gradient2(labels = scales::parse_format(), low = '#D61E00', mid = 'white', high = '#4521AC') -> p2

ggsave(p2, filename = 'ionic_strength.png', path = ggsavepath, width = 2.7, height = 4)


# Salinity vs ionic strength


pattern <- 'kinetics_equilibrium_co2_seawater_strength_and_salinity.csv'

fcsv <- list.files(path = getwd(), pattern = pattern, full.names = T)

df3 <- read_csv(fcsv, comment = '#', skip_empty_rows = T) %>% drop_na()

df3 %>% 
  # mutate(gama = I*gama) %>%
  ggplot(aes(y = I, x = Salinidad)) +
  geom_point() +
  xlim(10,NA) +
  scale_y_discrete(breaks = seq(0,1, by = 0.5)) +
  labs(y = "Ionic strength", x = "Salinity") +
  theme_classic(base_size = 18, base_family = "GillSans")


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

vars <- c('pH', 'fCO2', 'Carbonate', 'Aragonite')

vars <- c('Aragonite')


pacific_df %>% 
  select(Year_UTC, Month_UTC, Latitude, Longitude, Depth, vars) %>% 
  pivot_longer(cols = vars, names_to = 'var') %>%
  filter(value > 0) -> sbt


sbt <- sbt %>%   filter(var %in% vars) 

sbt$var <- factor(sbt$var, levels = levels(factor(sbt$var)))

# expression(Omega["ara"])
# expression(CO[3]^{-2}~(µmol~Kg^{-1})
# expression(pCO[2]~(uatm)
# expression(O[2]~(µmol~Kg^{-1}))

levels(sbt$var)

levels(sbt$var) <- c(expression(CO[3]^{-2}~(µmol~Kg^{-1})), expression(pCO[2]~(uatm)), "pH")


sbt %>% 
  filter(round(Latitude) == 25) %>%
  # filter(round(Latitude) <= 32.5) %>%
  filter(value > 0) %>%
  filter(Depth < 100) %>%
  ggplot(aes(y = Depth, x = value)) +
  facet_grid(.~ var, scales = 'free_x', labeller=label_parsed,  switch = 'x') +
  geom_point(shape = 0.7) +
  # scale_y_reverse(breaks = seq(0, 200, by = 50)) +
  scale_y_reverse(breaks = seq(0, 3500, by = 1000)) +
  scale_x_continuous(position = 'top') +
  geom_smooth(orientation = "y", method = 'loess') +
  labs(caption = 'Perfil vertical del Pacifico Norte, (25°N)',
    y = expression(~Depth~(dbar)),
    x = '') +
  theme_bw(base_family = "GillSans", base_size = 12) +
  theme(strip.background = element_blank(), 
    panel.border = element_blank()) -> p1

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

# Question 3 ------
# Estimates the Normal Distribution

SEED <- 1234
set.seed(SEED)
eta <- c(1, 0)
gamma <- c(1.8, 0.4)
N <- 200
x <- rnorm(N, 2, 2)
z <- rnorm(N, 0, 2)
mu <- binomial(link = logit)$linkinv(eta[1] + eta[2]*x)
phi <- binomial(link = log)$linkinv(gamma[1] + gamma[2]*z)
y <- rbeta(N, mu * phi, (1 - mu) * phi)

dens <- density(y)$y

df <- rbind(data.frame(x = 1:length(dens), g = 'Metabolite Production', dens), 
  data.frame(x = 1:length(dens),g = 'Oxigen Consumption', dens = 1-dens))

df %>%
  as_tibble() %>%
  ggplot(aes(y = dens, x = x, group = g)) +
  geom_hline(yintercept = c(0.15, 0.95), linetype="dashed", alpha=0.5, size = 1) +
  geom_line(size = 3, alpha = 0.7) +
  ylim(0,1) +
  xlim(0,300) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  labs(x = 'Tiempo', y = '[ . ]') +
  theme(legend.position = 'top',
    panel.border = element_blank(),
        axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

df %>%
  as_tibble() %>%
  mutate(densc = ifelse(grepl('Oxigen', g), 0.8, 0.25)) %>%
  ggplot(aes(y = densc, x = x, group = g)) +
  geom_hline(yintercept = c(0.15, 0.95), linetype="dashed", alpha=0.5, size = 1) +
  geom_line(size = 3, alpha = 0.7) +
  ylim(0,1) +
  xlim(0,150) +
  theme_classic(base_size = 16, base_family = "GillSans") +
  labs(x = 'Tiempo', y = '[ . ]') +
  theme(legend.position = 'top',
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())

# meta-analisis de respuesta de kroekel ----

pattern <- 'kroekel_meta_analysis_of_AO_effect.csv'

fcsv <- list.files(path = getwd(), pattern = pattern, full.names = T)

df <- read_csv(fcsv, comment = '#', skip_empty_rows = T) 

names(df)

cols <- c("Genus species" ,"Taxa", "Life stage", "Duration of exp. (days)","Calcification","Development","Growth", "Metabolism","Photosynthesis","Reproduction","Survival")

pivot <- c("Calcification","Development","Growth", "Metabolism","Photosynthesis","Reproduction","Survival")

df %>% distinct(`Life stage`)

df %>% select(cols) %>% 
  # filter(grepl('Haliotis', `Genus species`)) %>% 
  pivot_longer(cols = pivot, values_to =  'Experiments', 
    names_to = 'Response') %>% 
  drop_na(Experiments) %>%
  filter(grepl('larva|juvenile', `Life stage`)) %>%
  mutate(days = as.numeric(`Duration of exp. (days)`)) %>%
  drop_na(days) %>% filter(between(days, 0,50)) %>%
  mutate(Taxa = stringr::str_to_sentence(Taxa)) %>%
  mutate(`Life stage` = stringr::str_to_sentence(`Life stage`))-> df_long

df_long %>% distinct(`Life stage`)



df_long %>%group_by(Taxa) %>% 
  tally(Experiments, sort = T) %>% pull(Taxa) -> Ltaxa

df_long %>%  
  mutate(Taxa = factor(Taxa, levels = Ltaxa)) -> df_long


# 

df_long %>%  
  ggplot(aes(x = Taxa, y = Experiments)) +
  geom_col(fill = 'grey') +
  theme_classic(base_size = 14, base_family = "GillSans") +
  labs(y = '# Experiments', x = '') +
  theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()) -> p1
  # geom_bar(stat = "identity", position=position_dodge())

df_long %>%
  group_by(Taxa, Response) %>%
  tally(Experiments) %>%
  # mutate(Response = factor(Response, levels = pivot)) %>%
  mutate(Response = factor(Response, levels = rev(pivot))) %>%
  ggplot(aes(x = Taxa, y = Response, fill = Response)) +
  geom_tile(color = 'white', size = 0.2) +
  scale_y_discrete(limits = pivot) +
  geom_text(aes(label = n), color = 'white', family= "GillSans") +
  theme_classic(base_size = 14, base_family = "GillSans") +
  theme(legend.position = 'nonea',
    axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10)) -> p2

p1 / p2 + patchwork::plot_layout(heights = c(1,3))


df_long %>%
  filter(Taxa %in% Ltaxa[1:3]) %>%
  mutate(genus = sapply(strsplit(`Genus species`, " "), "[", 1)) %>%
  group_by(genus) %>%
  tally(Experiments, sort = T) %>%
  pull(genus) -> Lgenus


df_long %>%
  filter(Taxa %in% Ltaxa[1:3]) %>%
  mutate(genus = sapply(strsplit(`Genus species`, " "), "[", 1)) %>%
  group_by(genus, Taxa, Response) %>%
  tally(Experiments, sort = T) %>% 
  group_by(genus) %>% mutate(pct = n/sum(n)) %>%
  mutate(genus = factor(genus, levels = rev(Lgenus))) %>%
  mutate(Response = factor(Response, levels = rev(pivot))) %>%
  # mutate(pct = Experiments/sum(Experiments)) %>%
  # mutate(Taxa = factor(Taxa, levels = rev(Ltaxa))) %>%
  ggplot(aes(x = pct, y = genus, fill = Response)) +
  geom_col() + facet_grid(Taxa ~ ., scales = 'free_y', space = 'free') +
  theme_classic(base_size = 14, base_family = "GillSans") +
  theme(axis.text.x = element_text(angle = 45, 
    hjust = 1, vjust = 1, size = 10),
    strip.background = element_rect(color = 'white'))

# sapply(strsplit(string, ":"), "[", 2)



df_long %>%
  group_by(Taxa) %>%
  mutate(pct = Experiments/sum(Experiments)) %>%
  mutate(Taxa = factor(Taxa, levels = rev(Ltaxa))) %>%
  ggplot(aes(x = pct, y = Taxa, fill = Response)) +
  geom_col()

df_long %>%
  mutate(Taxa = factor(Taxa, levels = rev(Ltaxa))) %>%
  # aes(x = Taxa, y = Experiments)
  ggplot(aes(fill = Experiments, y = Taxa, x = Response)) +
  geom_tile(color = 'white', size = 0.2)

  
# 
df_long %>%
  ggplot(aes(days, fill = `Life stage`)) +
  geom_histogram(alpha = 0.5)


# Manua loa
# https://hahana.soest.hawaii.edu/hot/
pattern <- 'HOT_surface_CO2.csv'  

fcsv <- list.files(path = getwd(), pattern = pattern, full.names = T)

library(lubridate)
library(tidyverse)

vars <- c('date', 'pHcalc_insitu', 
  'pCO2calc_insitu', 
  'freeCO2_insitu', 'carbonate_insitu')

# 'aragsatcalc_insitu', 

Levelg <- c("CO2","pHcalc_insitu", "carbonate_insitu")


read_tsv(fcsv, skip = 7) %>%
  mutate(date = lubridate::dmy(date))  %>%
  select(vars) %>%
  # mutate(freeCO2_insitu = pCO2calc_insitu+freeCO2_insitu) %>%
  # mutate(carbonate_insitu = carbonate_insitu/pHcalc_insitu) %>%
  pivot_longer(-date, names_to = 'var') %>%
  filter(value > 0) %>%
  mutate(g = ifelse(grepl('CO2', var), 'CO2', var)) %>%
  mutate(g = factor(g, levels = Levelg)) -> tbl


levels(tbl$g)

levels(tbl$g) <- c(expression(CO[2]~(mu*atm)), "pH", 
  expression(CO[3]^{-2}~(µmol~Kg^{-1})))

# wesanderson::wes_palette("Royal1", 4, type = "discrete")

library(NatParksPalettes)

values <- natparks.pals("Yellowstone", 4, direction = -1) # "Yellowstone"


tbl %>%
  # filter(grepl('CO2', var)) %>%
  filter(grepl('carbonate', var)) %>%
  ggplot(aes(x = date, y = value, color = var, group = var)) +
  # facet_wrap(g  ~ ., scales = 'free_y', labeller=label_parsed, nrow = 3) +
  geom_smooth(method = 'lm', se = F)  +
  geom_point(alpha = 0.5, size = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  scale_y_continuous(position = 'left') +
  theme(strip.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    legend.position = 'none') +
  scale_color_manual(values = values) +
  labs(y = "", x = "Year") 
  # scale_y_continuous(name = 'pH', 
  # sec.axis = sec_axis(~.*8, name = expression(~pCO[2])))

#


tbl %>%
  filter(grepl('pH', var)) %>% # pH, carbonate
  ggplot(aes(x = date, value, color = var, group = var)) +
  facet_wrap(g  ~ ., scales = 'free_y', labeller=label_parsed, nrow = 3) +
  geom_smooth(method = 'lm', se = F)  +
  geom_point(alpha = 0.5, size = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  scale_y_continuous(position = 'left', breaks = seq(8,8.2, by = 0.05)) +
  theme(strip.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    legend.position = 'none') +
  scale_color_manual(values = "#CB7223") +
  labs(y = "", x = "Year") 

#

tbl %>%
  filter(grepl('carbonate', var)) %>% # pH, carbonate
  ggplot(aes(x = date, value, color = var, group = var)) +
  facet_wrap(g  ~ ., scales = 'free_y', labeller=label_parsed, nrow = 3) +
  geom_smooth(method = 'lm', se = F)  +
  geom_point(alpha = 0.5, size = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  scale_y_continuous(position = 'left') +
  theme(strip.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    legend.position = 'none') +
  scale_color_manual(values = "#289A84") +
  labs(y = "", x = "Year") 


vars <- c('pHcalc_insitu', 'carbonate_insitu')


read_tsv(fcsv, skip = 7) %>%
  mutate(date = lubridate::dmy(date))  %>%
  select(date, vars) %>%
  filter_all( all_vars(. > 0)) %>%
  ggplot() +
  geom_point(aes(x = date, y = pHcalc_insitu*30), color = '#CB7223',
    alpha = 0.5, size = 0.5) +
  geom_point(aes(x = date, y = carbonate_insitu), color = '#289A84',
    alpha = 0.5, size = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(strip.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    legend.position = 'none') +
  scale_color_manual(values = rev(values)) +
  labs(y = "", x = "Year") +
  scale_y_continuous(name = expression(CO[3]^{-2}~(µmol~Kg^{-1})), 
    sec.axis = sec_axis(~./30, name = "pH"))

  

vars <- c('pHcalc_insitu', 
  'pCO2calc_insitu','carbonate_insitu')


library(rstatix)

read_tsv(fcsv, skip = 7) %>%
  # mutate(date = lubridate::dmy(date))  %>%
  select(vars) %>%
  filter_all( all_vars(. > 0)) %>%
  rstatix::cor_mat(method = 'spearman') %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE, method = 'square')

