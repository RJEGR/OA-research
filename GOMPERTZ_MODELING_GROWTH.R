
# Ricardo Gomez-Reyes
# Table_1S - bind_data contiene las siguientes columnas
# Design = Estres Acumulativo (Cronico) y Agudo (Factor A)
# pH = pH experimental (Factor B)
# Average	= Valor promedio (%)
# hpf = Tiempo del muestreo en Horas pos fertilizacion (hpf)	
# g	= Analisis del rendimiento elaborado para: (Eclosion, Supervivencia y Asentamiento)

# logit = To evaluate proportional .. proportions data were logit transformed and evaluated
# https://doi.org/10.1890/10-0340.1

rm(list = ls())

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE, readr.show_col_types = FALSE)

library(tidyverse)
library(rstatix)
library(tidymodels)

fileN <- 'Table_1S - bind_data.csv'

f <- list.files(path = getwd(), pattern = fileN, full.names = T)

bind_data <- read_csv(f, comment = "#", col_names = T, skip = 1)

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4", `8`= "#4575b4")

bind_data %>% distinct(pH) %>% pull(pH)

bind_data %>% distinct(hpf) %>% pull(hpf)

pHL <- c(8.0, 7.8, 7.6)

pHpalette <- pHpalette[match(pHL, names(pHpalette))]

hpfL <- sort(unique(bind_data$hpf))

bind_data %>% 
  mutate(pH = factor(pH, levels = pHL)) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  mutate(Design = factor(Design, levels = c('Chronic', 'Acute'))) -> bind_data



# CALCIFICATIO AND GROWTH MEAN -----
# WRAP CALCIFICATION AND GROWTH INDEX

which_Assess <- c("Growth Index", "Birefrigence")

position <- position_dodge(width = 0.7)

# label_a <- "A) Growth (Index)"
# 
# label_b <- "B) Calcification (Pixels)"
# 

label_a <- "A) Crecimiento (Índice)"

label_b <- "B) Calcificación (Píxeles)"


level_key <- c("Growth Index" = label_a, "Birefrigence" = label_b)

bind_data %>% 
  filter(Assessment %in% which_Assess) %>%
  mutate(Assessment = recode_factor(Assessment, !!!level_key)) %>%
  filter(pH == 8) -> subset_dat


# hpf_key <- structure(c("Early", "Middle", "Mature", "Competent"), names = c(30, 48, 60, 108))

hpf_key <- structure(c("Temprana", "Media", "Madura", "Competente"), names = c(30, 48, 60, 108))

source("https://raw.githubusercontent.com/NightingaleHealth/ggforestplot/master/R/geom_stripes.R")

bind_data %>% 
  filter(Assessment %in% which_Assess) %>%
  mutate(Assessment = recode_factor(Assessment, !!!level_key))  %>%
  mutate(Panel = recode_factor(hpf, !!!hpf_key)) %>%
  ggplot(aes(x = hpf, average, color = pH, group = pH, fill = pH)) +
  # facet_wrap(~ Assessment, nrow = 2, scales = 'free_y',strip.position = "left") +
  facet_grid(Assessment ~ Panel, scales = "free", switch = "y") +
  labs(y = "") +
  geom_point(position = position, size = 1.4, shape = 1) +
  # geom_bar(stat = "identity", width = 0.6, position = position) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.1, size = 0.4, position = position) + #  color = "black"
  geom_text(aes(x = hpf, y = ymax, label= star), size = 1.9, 
    color = "black", family = "GillSans", position = position, vjust = -0.5) +
  # geom_path(data = subset_dat, size = 0.7,position = position_dodge2(width = -1)) 
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) +
  theme_classic(base_family = "GillSans", base_size = 10) +
  theme(strip.background.y = element_rect(fill = 'white', color = 'white'),
    strip.background.x = element_rect(fill = 'grey86', color = 'white'),
    # panel.border = element_rect(linetype = "dashed", fill = NA),
    panel.border = element_blank(),
    legend.position = 'top',
    strip.placement = "outside") -> p


# p +geom_stripes(odd = "#33333333", even = "#00000000", )

p

ggsavepath <- paste0(getwd(), '/Figures')

ggsave(p, filename = 'growth_calcification_rate_ES.png', path = ggsavepath, 
  width = 4.5, height = 3.5, dpi = 300, device = png)  

# only 108

bind_data %>% 
  filter(Assessment %in% which_Assess) %>%
  mutate(Assessment = recode_factor(Assessment, !!!level_key))  %>%
  filter(hpf == '108') %>%
  ggplot(aes(x = hpf, y= average, color = pH, group = pH, fill = pH)) +
  facet_wrap( Assessment ~., nrow = 1, scales = 'free_y', 
    strip.position = "top") +
  labs(y = "") +
  # geom_point(position = position, size = 0.7) +
  geom_bar(stat = "identity", width = 0.6, position = position) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
    width = 0.2, size = 0.5, position = position) +
  geom_text(aes(x = hpf, y = ymax, label= star), size = 1.7, 
    color = "black", family = "GillSans", position = position, vjust = -0.5) +
  # geom_path(aes(group = pH), size = 0.7,
  #     position = position) +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) +
  theme_classic(base_family = "GillSans", base_size = 12) +
  theme(strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank(), legend.position = 'none',
    strip.placement = "outside") -> p

ggsavepath <- paste0(getwd(), '/Figures')

ggsave(p, filename = 'growth_calcification_rate_108.png', path = ggsavepath, 
  width = 2.7, height = 1.5, dpi = 300) 

# Model growth based on gomperz model ----

# install.packages('easynls')
library(easynls)

# fit data to the gompertz model
weight=c(280,340,430,480,550,580,590,600,590,600)
age=c(8,12,24,36,48,60,72,84,96,108)

data1=data.frame(age, weight)

# gompertz
nlsfit(data1, model=10, start=c(max(data1[,-1]),4,0.05))

#plot data
nlsplot(data1, model=10, start=c(max(data1[,-1]),4,0.05), 
  xlab = "Days" , ylab = "Tumor Volume", position = 1)
# 

# OBS DATA

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  group_by(pH, hpf) %>%
  arrange(Index) %>%
  mutate(id = 1:length(Index)) %>%
  ggplot(aes(x = id, y = Index, group = pH, color =pH)) +
  stat_smooth() +
  facet_grid(~ hpf, scales = 'free', space = 'free') +
  geom_point() +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  theme_bw(base_family = "GillSans", base_size = 14)
  # ggpubr::stat_cor(method = "pearson")

# Rate ----

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  group_by(pH, hpf) %>%
  summarise(n = n(), av = mean(Index), 
    min = min(Index), max = max(Index)) %>%
  arrange(pH) %>%
  mutate(scope = max-min, rate = scope/24)

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  select(pH, hpf, Index) %>%
  # filter(pH == 8 & hpf == 24) %>%
  ungroup() %>%
  mutate(g = paste0(pH, "-", hpf)) -> out

Index= out$Index

data <- data.frame(id = 1:length(Index), Index = sort(Index))

nlsplot(data, model=10, start=c(250, 0.5, 0.1), 
  xlab = "Time" , ylab = "Growth", position = 1)


nlsfit(data, model=10, start=c(max(data[,-1]),4,0.05))

pred_gompertz <- function(y) {
  
  # start = c(max(y), 0.5, 0.1)
  
  # SSgompertz(y,Asym = max(y)-min(y), b2 = 1 ,b3 = length(y))
  
  # s = start

  s <- c(round(quantile(y, probs = 0.75)), 0.5, 0.01)
  
  y <- round(y)
  
  se = 1:length(y)# seq(min(y), max(y))
  
  data <- data.frame(x = se, y = sort(y))
  
  names(data) = c("x", "y")
  
  f10_stat = function(data, start) {
    
    R2 <- function(m) {
      gl <- length(fitted(m)) - 1
      sqt <- var((fitted(m) + resid(m))) * gl
      r1 <- (sqt - deviance(m))/sqt
      r1 = round(r1, 4)
      return(r1)
    }
    R3 <- function(m) {
      gl <- length(fitted(m)) - 1
      sqt <- var((fitted(m) + resid(m))) * gl
      r1 <- (sqt - deviance(m))/sqt
      p1 <- (gl/((gl + 1) - (length(coef(m) + 1))) * (1 - 
          r1))
      r2 <- 1 - p1
      r2 = round(r2, 4)
      return(r2)
    }
    
    
    # stats::nls {Nonlinear Least Squares}
    # start = a named list or named numeric vector of starting estimates.
    
    m = nls(y ~ a * exp(-b * exp(-c * x)), data = data, 
      start = list(a = s[1], b = s[2], c = s[3]), 
      control = nls.control(maxiter = 6000))
    
    c = coef(m)
    s = summary(m)
    a = c[1]
    b = c[2]
    c = c[3]
    l = c(a, b, c, summary(m)[11][[1]][10], summary(m)[11][[1]][11], 
      summary(m)[11][[1]][12], R2(m), R3(m), AIC(m), BIC(m))
    l = round(l, 8)
    l = as.data.frame(l)
    rownames(l) = c("coefficient a", "coefficient b", "coefficient c", 
      "p-value t.test for a", "p-value t.test for b", 
      "p-value t.test for c", "r-squared", "adjusted r-squared", 
      "AIC", "BIC")
    return(l)
  }
  
  
  res <- f10_stat(data, start = s)
  
  # return(res)
  
  f10 = function(i) res[1, i] * exp(-res[2, i] * exp(-res[3,
    i] * se))
  # 
  # se = seq(min(data[, 1]), max(data[, 1])) # , by = 0.01
  
  i = 1:(ncol(data) - 1)
  pred = lapply(i, f10)
  u = unlist(pred)
  mat = matrix(u, ncol = length(pred))
  pred = as.data.frame(mat)
  names(pred) <- 'Model'
  dd = data.frame(data, se, pred)
  # n = names(data)
  # ylab = n[-1]
  
  return(dd)
}


# Test
read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  select(pH, hpf, Index) %>%
  filter(pH == 7.6 & hpf == 48) %>%
  ungroup() %>%
  pull(Index) -> Index

quantile(Index, probs = 0.5)

pred_gompertz(Index) %>% as_tibble()



groups <- unique(out$g)

df <- list()

for(i in sort(groups)) {
  
  cat('\nModeling', i,'group\n')
  
  y <-  out %>% filter(g %in% i) %>% pull(Index)

  tbl <- pred_gompertz(y) %>%  mutate(g = i) %>% as_tibble()
  
  df[[i]] <- tbl
}

do.call(rbind, df) -> plot_data

plot_data %>%
  separate(col = g, into = c('pH', 'hpf'), sep = '-') %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  mutate(hpf = recode_factor(hpf, !!!c("24" = "30")))  -> plot_data
  
plot_data %>%
  ggplot(aes(x = Model, y = y, color = pH)) +
  facet_grid(~ hpf, scales = 'free' ) +
  # geom_point(aes(y = y), shape = 1, size = 0.4) +
  geom_line(size = 2, alpha = 0.7) +
  ggpubr::stat_cor(aes(y = y, x = Model)) +
  # geom_line(aes(y = Model)) +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  labs(x = expression('Expected'~ (µm)), y = expression('Observed'~ (µm))) +
  theme(panel.border = element_blank(), legend.position = 'none', 
    panel.grid.major = element_blank(),
    strip.background = element_rect(color = 'white', fill = 'white')) -> p1
  
level_key <- c("-1" = "Failure", "1" = "Success")

library(NatParksPalettes)

values <- natparks.pals("Arches", 2, direction = 1) # "Yellowstone"

plot_data %>%
  mutate(diag = y - Model, sign = sign(diag)) %>% # diag = n veces negativo que el modelo
  group_by(pH, sign) %>% count() %>%
  mutate(sign = recode_factor(sign, !!!level_key)) %>% 
  pivot_wider(names_from = sign, values_from = n) %>%
  mutate(Failure = Failure/sum(Failure+Success), Success = 1 - Failure) %>% view()
  pivot_longer(cols = c('Failure','Success')) %>% 
  mutate(name = factor(name, levels = c(c('Success', 'Failure')))) %>%
  mutate(pH = factor(pH, levels = pHL)) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>% #view()
  ggplot(aes(y = value, x = pH, fill = name)) + 
  facet_grid(~  hpf) +
  # ggh4x::facet_nested(~ Assessment + hpf, nest_line = T) +
  geom_col(position = position_stack(reverse = T)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = '') +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(panel.border = element_blank(), legend.position = 'top') +
  scale_fill_manual("", values=values) +
  theme(
    strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank(), 
    panel.grid = element_blank(),
    legend.position = 'none') -> ps


ggsavepath <- paste0(getwd(), '/Figures/')

ggsave(ps, 
  filename = 'development_rate.png', path = ggsavepath, 
  width = 5, height = 3) 

# i.e


plot_data %>%
  mutate(diag = y - Model) %>%
  group_by(pH, hpf) %>%
  ggplot(aes(diag, fill = pH, color = pH)) +
  # facet_grid(~ hpf, scales = 'free' ) +
  geom_histogram(binwidth = 0.5, )+
  # ggplot2::stat_ecdf(pad = F) +
  geom_rug(outside = F) +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  scale_color_manual(values = pHpalette) +
  scale_fill_manual(values = pHpalette) +
  theme_classic(base_family = "GillSans", base_size = 14) +
  labs(x = expression('Growth delay'~ (µm)), y = 'Count') +
  theme(panel.border = element_blank(), legend.position = 'none', 
    panel.grid.major = element_blank(),
    strip.background = element_rect(color = 'white', fill = 'white')) -> p2

ggsave(p2, 
  filename = 'development_rate_hist.png', path = ggsavepath, 
  width = 3, height = 3) 

p2 + facet_grid(hpf ~ pH)

# PLOT ONLY BY PH ====

groups <- c(unique(out$pH))

df <- list()

for(i in sort(groups)) {
  
  cat('\nModeling', i,'group\n')
  
  y <-  out %>% filter(pH %in% i) %>% pull(Index)
  
  tbl <- pred_gompertz(y) %>%  mutate(g = i) %>% as_tibble()
  
  df[[i]] <- tbl
}

do.call(rbind, df) -> plot_data


plot_data %>%
  group_by(g) %>%
  rstatix::cor_test(y, Model)

# y~a*exp(-b*exp(-c*x))

plot_data %>%
  mutate(g = factor(g, levels = pHL)) %>%
  ggplot(aes(x = x, color = g)) +
  geom_point(aes(y = y), shape = 1, alpha = 0.3, size = 0.5) +
  # geom_hex(aes(y = y, fill = g), alpha = 0.3) +
  geom_line(aes(y = Model), size = 1) +
  scale_color_manual("",values = pHpalette) +
  scale_fill_manual("",values = pHpalette) +
  theme_classic(base_family = "GillSans", base_size = 16) +
  labs(x = expression('Rows'), y = expression('Growth Index')) +
  theme(panel.border = element_blank(), legend.position = 'top', 
    panel.grid.major = element_blank(),
    strip.background = element_rect(color = 'white', fill = 'white')) -> ps

ggsave(ps, 
  filename = 'gompertz_modeling_growth.png', path = ggsavepath, 
  width = 3.5, height = 3.5, dpi = 500, device = png) 

# estos datos pueden testearse al modelo y hacer un estadistico de valores p, respecto al valor modelo. Entonces graficar valores (transformados) vs valores p


# https://www.cyclismo.org/tutorial/R/pValues.html#calculating-many-p-values-from-a-t-distribution

# VOLCANO ----

se <- function(x,y) { sd(x)*sd(x)/x+sd(y)*sd(y)/y}
  
-log10(0.05)

plot_data %>%
  group_by(pH, hpf) %>%
  mutate(t = (Model-y)/se(Model,y), 
    p = pt(-abs(t), df=pmin(Model,y)-1), sign = sign(t)) %>%
  rstatix::adjust_pvalue("p", method = "bonferroni") %>%
  filter(p.adj < 0.05) %>%
  # la misma proporcion de sign que se reporta en el barplot superior es la misma aqui, pero determinamos la significancia
  ggplot(aes(y = -log10(p.adj), x = t)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey') +
  geom_hline(yintercept = -log10(0.05), linetype = 'dashed', color = 'grey') +
  geom_jitter(shape = 1, alpha = 0.3) +
  # facet_grid(hpf ~ pH) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(panel.border = element_blank(), legend.position = 'top') +
  theme(
    strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = 'top')

# statistical different

plot_data %>%
  group_by(pH, hpf) %>%
  mutate(t = (Model-y)/se(Model,y), 
    p = pt(-abs(t), df=pmin(Model,y)-1), sign = sign(t)) %>%
  rstatix::adjust_pvalue("p", method = "bonferroni") %>%
  filter(p.adj < 0.05) %>%
  group_by(pH,hpf, sign) %>% count() %>% 
  mutate(sign = recode_factor(sign, !!!level_key)) %>%
  pivot_wider(names_from = sign, values_from = n) %>%
  mutate(Failure = Failure/sum(Failure+Success), Success = 1 - Failure) %>%
  pivot_longer(cols = c('Failure','Success')) %>%
  mutate(name = factor(name, levels = c(c('Success', 'Failure')))) %>%
  mutate(pH = factor(pH, levels = pHL)) %>%
  mutate(hpf = factor(hpf, levels = hpfL)) %>%
  ggplot(aes(y = value, x = pH, fill = name)) + 
  facet_grid(~  hpf) +
  # ggh4x::facet_nested(~ Assessment + hpf, nest_line = T) +
  geom_col(position = position_stack(reverse = T)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = '') +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(panel.border = element_blank(), legend.position = 'top') +
  scale_fill_manual("", values=values) +
  theme(
    strip.background = element_rect(fill = 'white', color = 'white'),
    panel.border = element_blank(), 
    panel.grid = element_blank(),
    legend.position = 'top')


plot_data %>%
  group_by(pH, hpf) %>%
  mutate(t = (Model-y)/se(Model,y), 
    p = pt(-abs(t), df=pmin(Model,y)-1), sign = sign(t)) %>%
  rstatix::adjust_pvalue("p", method = "bonferroni") %>%
  filter(p.adj < 0.05) %>%
  ggplot(aes(y, color = pH)) +
  facet_grid(~ hpf) +
  stat_ecdf(pad = F)

plot_data %>%
  ggplot(aes(y, color = pH)) +
  facet_grid(~ hpf) +
  stat_ecdf(pad = F)
  


# figure text: Length-at-age for Atlantic bluefin tuna (Thunnus thynnus)
# individuals landed in the 1970s (gold; N ¼ 350), 1990s (blue;
#   N ¼ 234), and 2010s (green; N ¼ 1359) (top panel). Lines connect
# mean length-at-age; clouds are 61 SD. Mixed-stock and westernstock estimated von Bertalanffy growth curves (points indicate
#   predicted mean size-at-age) for Atlantic bluefin tuna individuals
# landed in the 1970s (gold), 1990s (blue), and 2010s (green) in
# comparison to the accepted growth curve for the western stock
# (red; Restrepo et al., 2010) (bottom panel). Mixed-stock and
# western-stock curves overlap. Please refer to the online version of
# this publication to view color images
# 

# read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
#   select(pH, hpf, Index) %>%
#   group_by(pH, hpf) %>% # count()
#   # summarize(pred_gompertz(Index))
#   doo(~ pred_gompertz(Index) ) %>% 
#   ggplot(aes(x = se, y = y, group = pH, color = pH)) +
#   facet_grid(~ hpf, scales = 'free', space = ) +
#   geom_point(shape = 3) 
#   ggpubr::stat_cor()

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  select(pH, hpf, Index) -> out



  
# 
# 
matplot(dd[, 1], dd[, -1], type = "l", bty = "n", 
  # xlab = xlab,
  ylab = ylab, col = c(1:(ncol(data) - 1)), lty = c(1:(ncol(data) -
      1)))


# dd %>%
  
# modeling delay in growth based on body index

read_rds(paste0(getwd(), '/length_width_dataset.rds')) %>%
  # select(pH, hpf, Index) %>%
  group_by(Stage, hpf, pH) %>%
  count()
