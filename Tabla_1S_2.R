
# Ricardo Gomez-Reyes
# Table_1S - hpf contiene las siguientes lista de tablas (30 hpf, 48 hpf y 60 hpf)
# Cada tabla contiene las siguintes columnas:
# hpf = Tiempo del muestreo en Horas pos fertilizacion (hpf)	
# pH = pH experimental (Factor B)
# Ontogeny = Clasificacion larval segun el aspecto (Revisar comentarios "#" de Tabla_1S)
# Count	= Valor absoluto de Larvas
# Design = Estres Acumulativo (Cronico) y Agudo (Factor A)
# Pct = Valor Relativo de Larvas

rm(list = ls())

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE, readr.show_col_types = FALSE)

bind_d_1 <- paste0(getwd(),'/Table_1S - bind_data.rds')

# bind_dat <- read_rds(bind_d_1)

fileN <- 'Table_1S'

f <- list.files(path = getwd(), pattern = fileN, full.names = T)

f <- f[grepl('hpf', f)]

read_f <- function(f) { 
  
  df <- read_csv(f) 
  
  df %>% distinct(Ontogeny) %>% pull() -> names_from
  
  df %>% 
    pivot_wider(names_from = Ontogeny, values_from = Count) %>%
    mutate_at(all_of(names_from), function(x) {x / rowSums(.[names_from])}) %>%
    pivot_longer(cols = all_of(names_from), values_to = 'Pct') %>%
    arrange(rev(name)) %>% select(Pct) %>%
    cbind(df, . ) %>% drop_na() %>% as_tibble() }

f <- lapply(f, read_f)

bind_data <- do.call(rbind, f) %>%
  rename(Replicate = 'id')

good_perf <- c("Trochophore_30", "Veliger_48", "Shelled_60")
bad_perf <- c("Egg_30", "Trochophore_48", "Unshelled_60")


bind_data %>%
  mutate(group = paste0(Ontogeny, "_",hpf)) %>%
  mutate(Development =  ifelse(group %in% good_perf, 'A', 'B')) %>%
  select(-group) -> bind_data


pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4",
  `8.0`= "#4575b4", `8`= "#4575b4")

bind_data %>% distinct(pH) %>% pull(pH)
bind_data %>% distinct(hpf) %>% pull(hpf)

pHL <- c(8.0, 7.8, 7.6)

pHpalette <- pHpalette[match(pHL, names(pHpalette))]

hpfL <- c(30, 48, 60)

# Create pooled data ----

.bind_data <- read_rds(bind_d_1)

keep <- names(.bind_data) %in% names(bind_data)
select_col <- names(.bind_data)[keep]

bind_data %>%
  ungroup() %>%
  filter(Development %in% 'A') %>%
  select(all_of(select_col)) -> x

# x %>% distinct(hpf) %>% pull(hpf)

.bind_data %>%
  ungroup() %>%
  select(all_of(select_col)) -> y

# y %>% distinct(hpf) %>% pull(hpf)

hpfL <- c(24, 30, 48, 60, 108, 624)

df <- rbind(x,y) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  mutate(Design = factor(Design, levels = c('Chronic', 'Acute'))) 

df %>% distinct(hpf) %>% pull(hpf)

file_out <- paste0(getwd(),'/Table_1S_Pooled.rds')

write_rds(df, file = file_out)


# Global relationship to pH ----

bind_data %>% 
  filter(Development %in% 'A') %>%
  ggplot(aes(y = Pct, x = pH)) + 
  # facet_grid(~ hpf) + # Separate by development
  stat_smooth(method = 'glm', se = F, color = 'blue') +
  geom_jitter(shape = 1) + # color = as.factor(pH)
  ggpubr::stat_cor(method = "spearman", family = "GillSans") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_reverse(breaks = pHL) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  theme(panel.border = element_blank()) +
  labs(x = '', y = '') +
  scale_color_manual("", values = pHpalette)
  

bind_data %>% # bind_data
  filter(Development %in% 'A') %>%
  # select(Replicate, pH, Pct) %>%
  # pivot_wider(names_from = pH, values_from = Pct)
  # group_by(hpf) %>% # Separate by development
  cor_test(vars = 'pH', vars2 = 'Pct', method = 'spearman')

bind_data %>% distinct(pH) %>% pull(pH)
bind_data %>% distinct(hpf) %>% pull(hpf)

pHL <- c(8.0, 7.8, 7.6)

pHpalette <- pHpalette[match(pHL, names(pHpalette))]

hpfL <- c(30, 48, 60)

bind_data %>% 
  mutate(pH = factor(pH, levels = pHL)) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) -> bind_data


# 0) Outliers removal ----

bind_data %>%
  ggplot(aes(y = Count, x = pH, color = Development, fill = Development)) +
  facet_wrap( ~ hpf, scales = 'free_y') +
  geom_jitter(width = 0.2,alpha=0.2, height = 0.1, size = 2) + # shape = 1
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot(width = 0.2, outlier.alpha = 0, coef = 0) +
  theme_bw(base_family = "GillSans", base_size = 14) 


# bind_data %>% group_by(pH, hpf,Ontogeny) %>% tally()

bind_data %>% 
  group_by(pH, Ontogeny, hpf) %>% 
  mutate(is.outlier = is_outlier(Count)) %>%
  filter(!is.outlier %in% TRUE) %>%
  select(-is.outlier) -> bind_data

# bind_data %>% group_by(pH, Ontogeny, hpf) %>% tally()

# 1) Gaussianity (PARTIAL TRUE) ----

bind_data %>% 
  group_by(hpf, Development) %>% 
  rstatix::shapiro_test(Count) %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

# Because normality (Partially)

type_stats <- 'mean_se'

bind_data %>%
  filter(Development %in% 'A') %>%
  group_by(hpf, pH, Design) %>%
  select(-Replicate, -Count, -Development) %>%
  get_summary_stats(type = type_stats) %>%
  mutate(ymax = mean+se, ymin = mean-se) -> stat_out

# table

stat_out %>%
  select(hpf, pH, mean) %>%
  pivot_wider(names_from = pH, values_from = mean) %>%
  view()

# Radar

library(ggradar)

# stat_out %>% view()

stat_out %>%
  select(hpf, pH, mean) %>%
  pivot_wider(names_from = hpf, values_from = mean) %>%
  ggradar(font.radar = "GillSans") +
  scale_color_manual("", values = pHpalette)

# Merge w/ bind_dat 1

hpfL <- c(24, 30, 48, 60, 108, 624)

read_rds(bind_d_1) %>%
  group_by(hpf, pH, Design) %>%
  select(-Replicate, -Count) %>%
  get_summary_stats(type = type_stats) %>%
  mutate(ymax = mean+se, ymin = mean-se) %>%
  select(names(stat_out)) %>%
  rbind(stat_out) %>%
  mutate(Design = factor(Design, levels = c('Chronic', 'Acute'))) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  mutate(pH = factor(pH, levels = pHL)) -> stat_out

stat_out %>%
  filter(Design %in% "Chronic") %>%
  select(hpf, pH, mean) %>%
  pivot_wider(names_from = hpf, values_from = mean) %>%
  select(pH,`24`,`30`,`48`,`60`,`108`,`624`) %>%
  ggradar(font.radar = "GillSans", legend.position = "top") +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette)

stat_out %>%
  ggplot(aes(x = hpf, y = mean, color = pH, group = pH)) +
  facet_grid( ~ Design, space = 'free_x', scales = 'free') +
  geom_point(position = position_dodge(width = 0.3), 
    size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.1, position = position_dodge(width = 0.3)) +
  # geom_path(position = position_dodge(width = 0.3), size = 1) +
  scale_color_manual("", values = pHpalette) +
  labs(y = type_stats) +
  scale_y_continuous(labels = scales::percent)

# OR pooled data ----

.bind_data <- read_rds(bind_d_1)

keep <- names(.bind_data) %in% names(bind_data)
select_col <- names(.bind_data)[keep]

bind_data %>%
  ungroup() %>%
  filter(Development %in% 'A') %>%
  select(all_of(select_col)) -> x

.bind_data %>%
  ungroup() %>%
  select(all_of(select_col)) -> y

df <- rbind(x,y) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  mutate(Design = factor(Design, levels = c('Chronic', 'Acute'))) 
  
  


df %>%
  ggplot(aes(x = hpf, y = mean, color = pH, group = pH)) +
  facet_grid( ~ Design, space = 'free_x', scales = 'free') +
  geom_jitter(size = 3, alpha = 0.5) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.1, position = position_dodge(width = 0.3)) +
  # geom_path(position = position_dodge(width = 0.3), size = 1) +
  scale_color_manual("", values = pHpalette) +
  labs(y = type_stats) +
  scale_y_continuous(labels = scales::percent)

# 
# 
# Transform dataset ----

# logit {car}
# Computes the logit transformation logit = log[p/(1 - p)] for the proportion p.

bind_data %>% mutate(logit = car::logit(Pct)) -> bind_data

bind_data %>% ggplot(aes(logit, Pct)) + geom_jitter()

# Logistic regression ----

fit <- logistic_reg() %>%
  set_engine("glm") %>% # log_mod
  fit(pH:hpf ~ logit, data = bind_data)

fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(logit ~ pH, data = bind_data)


# fit %>% tidy()


performance::check_model(fit)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
fit <- lm(logit ~ pH, data = bind_data)

segmented.fit <- segmented(fit, seg.Z = ~ pH, )
#view summary of segmented model
summary(segmented.fit)




