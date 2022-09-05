# beta coeff https://jhudatascience.org/tidyversecourse/model.html
# https://www.gmudatamining.com/lesson-10-r-tutorial.html#Introduction_to_Linear_Regression
# modelo de regresion 
# estimacion =  (objetivo ultimo de un m dergresion) la relacion que tiene una variable dependiente de una/mas variables independiente. De manera puntual o por intervalos. Coef beta (puntual) e intervalo de confianza de los coeficiente. Se obtiene una estimacion fija y plausible, pero posiblemente sesgada (pero no precisamente incorrecta)
# Prediccion = utilizacion de modelo para estimar parametros

# evaluar modelo para predecir TA y DIC usando datos de pH : https://www.youtube.com/playlist?list=PLjjEKe1DTAUYUwqYCYZOffYGkiCQhDfVf or DIC and TA are predicted from regressions given in section 4.2.1 (equations (7) and (8)). https://www.researchgate.net/publication/281356004_Anthropogenic_CO2_in_the_Southern_Ocean_Distribution_and_inventory_at_the_Indian-Atlantic_boundary_World_Ocean_Circulation_Experiment_line_I6

# https://www.tidymodels.org/start/models/
# Revisar graficas de aragonita, pH predicho vs pH medido y salinidad de Barton 2012 para graficar despues del modelado lineal. (cite": The Pacific oyster, Crassostrea gigas, shows negative correlation to naturally elevated carbon dioxide levels: Implications for near-term ocean acidification effects Barton L et al )

# Nota: Tomar en cuenta que con relacion al DIC/TA, el pH control es generado a traves de una distribucion normal

# CLEAR OBJECT LIST AND IMAGE CANVAS   

rm(list = ls());

if(!is.null(dev.list())) dev.off()

options(stringsAsFactors = FALSE, show_col_types = FALSE)

library(tidymodels)
library(performance)
library(readr)
library(dotwhisker) 
library(rstatix)

source(paste0(getwd(), "/stats.R"))


path <- '~/Documents/DOCTORADO/pH_measures/'

# path <- "/Users/cigom/Documents/GitHub/OA-research/"

ggsavepath <- paste0(getwd(), '/Figures')

nameLevs <- c('Experimental-I', 'Experimental-II', 'Control')

level_key <- c("Experimental-I"="7.6","Experimental-II"="7.8","Control"="8")

pHpalette <- c(`7.6`="#d73027", `7.8`= "#abdda4", `8`= "#4575b4")

pHLevel <- c("8", "7.8", "7.6")

pHpalette <- pHpalette[match(pHLevel, names(pHpalette))]


varsf <- c('DIC', 'TA', 'HCO3', 'CO3', 'Aragonite', 'Calcite', 'pCO2_matm')


np_file <- '/pH_aLLdatasets_by_hour_stats.rds' # '/pH_aLLdatasets_stats.rds'

new_points <- read_rds(paste0(getwd(), np_file)) %>% 
  ungroup() %>%
  rename('pH' = a) %>%
  # mutate(name = factor(name, levels = nameLevs)) %>%
  select(date, hour, name, pH, sd, se)

#
# 669 datos para pH 7.6 y 7.8
# Solo 308 (o 374) registros para pH 8


new_points %>% ungroup() %>% 
  filter(name %in% '8') %>%
  distinct(date, hour) -> date_ph_8

# needs  name     pH      sd    id columns
# rnorm(1E5, mean = fill_ph$a, sd = fill_ph$sd)

new_points %>% filter(name%in%  '8') %>% 
  summarise(a = mean(pH), sd = sd(pH), 
    se = sd/sqrt(n())) -> obs_dataset


dat <- rnorm(1E5, mean = obs_dataset$a, sd = obs_dataset$sd)

pool_ph <- function(x, n = 100) { 
  
  # x <- rnorm(n, mean = obs_dataset$a, sd = obs_dataset$sd)
  x <- dat
  
  y <-  sample(x, n, replace = T)
  
  a <- mean(y)
  
  sd <- sd(y)
  
  se <- sd/sqrt(n)
  
  # z <-  (max(y) - mean(y)) / sd(y)
  
  df <- data.frame(a,sd, se)
  
  return(df)
}

# pool_ph(fill_ph)

new_points %>% ungroup() %>% 
  distinct(date, hour) %>% 
  anti_join(date_ph_8) %>%
  group_by(date, hour) %>%
  doo(~ pool_ph(fill_ph)) %>%
  # rename() %>%
  mutate('pH' = a, name = '8') %>%
  select(names(new_points)) %>%
  rbind(new_points) %>% 
  as_tibble() -> new_points
#

new_points %>% ungroup() %>% 
  mutate(hour = reorder(hour, date)) %>% 
  group_by(name) %>% mutate(id = 1:length(hour)) -> new_points

new_points %>% count(name)


new_points %>% filter(!name %in% 'Control') %>% 
  filter(grepl('2022-02-22', date)) %>% filter(id == 370) # hour 20 when data control start to record

# new_points %>%  mutate(id = ifelse(name %in% 'Control', id+370, id)) -> new_points


csv_file <- 'DIC_TA_Predicted_vars.csv' # replaced for 'quimicas.csv'

measured_vars <- c('DIC','TA','pH_measured','sd','n', 'Sal')
calculate_vars <- c('pHNBS_predicted','HCO3', 'CO3', 'CO2', 'pCO2_matm','Aragonite', 'Calcite') # mmol/kgSW



# new_points %>%
#   mutate(hour = lubridate::hour(date)) %>%
#   mutate(date = lubridate::date(date)) -> new_points

read_csv(paste0(getwd(), "/",csv_file), skip = 1, show_col_types = FALSE) %>% 
  select(date, name, measured_vars, calculate_vars) %>%
  mutate(date = lubridate::dmy(date)) %>%
  # left_join(new_points) %>%
  # mutate(across(where(is.numeric), replace_na, 8)) %>%
  # mutate(pH = ifelse(is.na(pH), pH8_values, pH)) %>%
  arrange(name) %>%
  rename('pH' = pH_measured) %>%
  filter(!grepl('2021', date)) %>%
  mutate(name = factor(name, levels = nameLevs)) -> df

which_vars <- c('DIC', 'TA', 'CO3', 'Aragonite')

df %>%
  group_by(name) %>%
  rstatix::get_summary_stats(all_of(which_vars)) %>%
  select(name , variable, n, mean, median, sd, se) %>%
  view()

df$pH

# Replace Control pH using a normal data from the set 

# (omit) --------
# Cual es la distribucion acumulativa de correlaciones usando boostrap 


n_vals <- df %>% filter(name%in% 'Control') %>% pull(pH) %>% is.na() %>% sum()

set.seed(20220505)

new_points %>% filter(name%in%  '8') %>% 
  summarise(a = mean(pH), sd = sd(pH)) -> fill_ph

fill_ph <- rnorm(1E5, mean = fill_ph$a, sd = fill_ph$sd)

# plot(ecdf(fill_ph))

# 
# df %>%
#   filter(name %in% 'Control') %>%
#   pull(pHNBS_predicted) -> pHNBS_predicted
# 
# # 
# out <- vector()
# 
# boostrap <- 1E5
# 
# for(i in 1:boostrap) {
#   
#   pHdist <- sample(fill_ph, length(pHNBS_predicted))
#   out[i] <- cor(pHNBS_predicted,pHdist)
#   
# }
# 
# plot(ecdf(out))
# 
# sum(out > 0.7)
# head(which(out > 0.7), 1)
# cuantas veces tiene que haber un sampleo para que halla una combinacion de datos con correlacion mayor a i?
# Continue -----

sd(fill_ph)

df %>%
  mutate(pH = ifelse(is.na(pH), sample(fill_ph, n_vals), pH)) %>%
  
  # Factors are very helpful for modeling, so we convert one column
  mutate(name = factor(name, levels = nameLevs)) %>%
  group_by(name) %>%
  mutate(nDIC = mean(Sal)*(DIC/Sal), nTA = mean(Sal)*(TA/Sal)) -> df


which_vars <- c('DIC', 'TA', 'CO3', 'Aragonite')

df %>%
  group_by(name) %>%
  rstatix::get_summary_stats(all_of(which_vars)) %>%
  select(name , variable, n, mean, median, sd, se) %>%
  view()

# clean outliers from df
write_tsv(df, file = paste0(path, '/quimicas_v2.tsv'))

sum(df$date %in% unique(new_points$date)) # sin embargo, no tengo datos de pH de control para las mediciones de DIC/TA hechas en esas fechas, pero suponemos, debido a los bajos numeros de sd y error standar, las estimaciones son apropiadas como se hicieron.


  
  

# DIC/TA vs pH

# cols <- c("DIC", "TA", "nDIC", "nTA","Sal", "HCO3", "CO3","Aragonite" ,"pHNBS_predicted")

# 0) outliers -----

df %>% ungroup() %>% select_if(is.numeric) %>% select(-pH) %>% names() -> cols

df %>%
  pivot_longer(cols = all_of(cols), names_to = 'vars') %>%
  group_by(name, vars) %>% 
  mutate(is.outlier = is_outlier(value), 
    is.extreme = is_extreme(value)) %>% 
  mutate(vars = factor(vars, levels = cols)) -> df_longer

# 1) Gaussianity (TRUE) ----
# Esto nos indica si o no el DIC y TA se mantuvieron iguales en los sistemas a lo largo de los dias de experimentacion.

df_longer %>%
  filter(is.outlier == FALSE) %>%
  group_by(name, vars) %>% 
  filter(vars %in% c('TA', 'DIC')) %>%
  shapiro_test(value) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE)) -> shapiro_df

# for pH

df %>%
  group_by(name) %>% 
  shapiro_test(pH) %>% # using pHNBS_predicted the results is gauss == TRUE
  adjust_pvalue(method = "none") %>%
  add_significance("p") %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

shapiro_df 

# A tibble: 6 × 6
# name            vars  variable statistic      p  p.adj p.signif gauss
# <fct>           <fct> <chr>        <dbl>  <dbl>  <dbl> <chr>    <lgl>
#   1 Control         DIC   value        0.928 0.567  0.567  ns       TRUE 
# 2 Control         TA    value        0.810 0.0726 0.0726 ns       TRUE 
# 3 Experimental-I  DIC   value        0.860 0.190  0.190  ns       TRUE 
# 4 Experimental-I  TA    value        0.896 0.350  0.350  ns       TRUE 
# 5 Experimental-II DIC   value        0.930 0.599  0.599  ns       TRUE 
# 6 Experimental-II TA    value        0.832 0.113  0.113  ns       TRUE 

# A tibble: 3 × 7
# name            variable statistic     p p.adj p.signif gauss
# <fct>           <chr>        <dbl> <dbl> <dbl> <chr>    <lgl>
#   1 Control         pH           0.905 0.402 0.402 ns       TRUE 
# 2 Experimental-I  pH           0.905 0.402 0.402 ns       TRUE 
# 3 Experimental-II pH           0.994 0.997 0.997 ns       TRUE 

# 2) Homocelasticidad (PARCIAL FALSE) ----

df_longer %>% 
  filter(is.outlier == FALSE) %>%
  filter(vars %in% c('TA', 'DIC')) %>%
  group_by(vars) %>%
  levene_test(value ~ as.factor(name), center = 'median') %>%
  mutate(hom_var = ifelse(p > 0.05, TRUE, FALSE))

# 3) filtering data ----

df_longer %>% filter(is.outlier == FALSE) %>% 
  select(-is.outlier, -is.extreme) %>%
  pivot_wider(names_from = vars, values_from = value) %>%
  drop_na() -> df

write_tsv(df, file = paste0(path, '/quimicas_filtered.tsv'))


# due to data are normal, mean can explain the results

df %>% rstatix::get_summary_stats() %>%
  filter(variable %in% c('DIC', 'TA', 'CO3', 'Aragonite')) %>%
  select(name ,variable, n, mean, median, sd) %>% 
  arrange(variable)

# 4) Statistical test ----
# prepare statistical priori and posteriori w.w inference

# 4.1) Statistical priori test----
# Debido a que no hay homocelasticidad entre todas las vars

# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation where there are more than two groups. It’s recommended when the assumptions of one-way ANOVA test are not met. 

# We want to know if there is any significant difference between the variables from DIC/TA from the three conditions.

# varsf <- c('DIC', 'TA', 'HCO3', 'CO3', 'Aragonite')


df_longer %>% ungroup() %>% filter(vars %in% varsf) %>% filter(is.outlier == FALSE) %>%
  mutate(vars = factor(vars, levels = varsf))-> df_longer_stast

df_longer_stast %>% 
  mutate(name = recode_factor(name, !!!level_key)) %>%
  mutate(name = factor(name, levels = pHLevel)) -> df_longer_stast

df_longer_stast %>% 
  group_by(vars)%>%
  rstatix::kruskal_test(value ~ name) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> kruskal.stats

kruskal.stats

df_longer_stast %>% 
  group_by(vars)%>%
  rstatix::welch_anova_test(value ~ name) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") 


# 4.2) Statistical posteriori test----

# En vista de que encontramos outliers y normalidad, pero no homocelasticidad, evaluamos a traves de un test de wilcoxon

df_longer_stast %>%
  group_by(vars) %>%
  pairwise_wilcox_test(value ~ name,  conf.level = 0.95) %>%
  # rstatix::tukey_hsd(value ~ name) %>%
  adjust_pvalue() %>%
  add_significance() -> stats.test

stats.test %>% add_xy_position(x = "name", scales = "free_y") -> stats

# stats$y.position <- max(out_stats$a)+out_stats$sd

title <- get_pwc_label(stats)

# get_test_label(kruskal.stats, detailed = TRUE)[1]

subtitle <- get_description(kruskal.stats)

# and plot

# value ~ name

df_longer_stast %>%
  mutate(vars = factor(vars, levels = varsf)) %>%
  ggplot(aes(y = value, x = name)) +
  facet_wrap(~ vars, scales = 'free_y', nrow = 1) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  stat_boxplot(aes(color = name),geom ='errorbar', width = 0.07) +
  geom_boxplot(aes(fill = name, color = name), width = 0.3, outlier.alpha = 0) +
  geom_jitter(aes(color = name), width=0.1,alpha=0.2, 
    height = 0.1, size = 2, shape = 1) +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) +
  # stat_summary(fun=median, geom ="line", aes(group = 2), size= 0.5, color = 'blue') +
  # stat_summary(fun=mean, geom ="point", aes(group = 2), size= 0.5, color = 'black') 
  theme_bw(base_family = "GillSans", base_size = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(y = '', x = '') -> psave

psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank(),
  axis.text.x = element_text(
    # angle = 45, 
  hjust = 1, vjust = 1, size = 10)) -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, linetype = 'dashed', hide.ns = T) +
  labs(subtitle = subtitle, title = title) -> psave


ggsave(psave, filename = 'stasts_vars.png', path = ggsavepath, 
  width = 10, height = 3.5)


# 5) Veredicto -----

# Debido a que no hay diferencias entre los datos, haremos una regresion lineal para evaluar el comportamiento de los datos. Y buscaremos de los valores B, diferencias estadisticamente significativas

# * Estimacion usando modelo lineal generalizado multivariable ----
# 6) Verificar linearidad ----

df_longer_stast %>% ungroup() %>% distinct(vars)

df_longer_stast %>%
  ggplot(aes(y = pH, x = value, color = name)) +
  geom_point(aes(shape = is.outlier), size = 3) +
  geom_smooth(se = F, method = lm) +
  facet_grid(~ vars, scales = 'free_x') +
  ggpubr::stat_cor(method = "pearson", 
    cor.coef.name = "R", 
    p.accuracy = 0.001) +
  labs(x = '') +
  scale_color_manual(values = pHpalette) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  theme(legend.position = 'top') -> psave


psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank()) -> psave

ggsave(psave, filename = 'ph_vs_vars_cor.png', path = ggsavepath, width = 17, height = 5)

# Vars vs pH cor test ----

df_longer_stast %>%
  filter(!vars %in% 'Sal') %>%
  group_by(name, vars) %>%
  rstatix::cor_test(pH,value) %>%
  rstatix::add_significance() -> cor_df


m <- cor_df %>% 
  select(name, vars, cor) %>%
  pivot_wider(values_from = cor, names_from = name) %>%
  as.data.frame(row.names = vars)

hc <- hclust(dist(m[-1]), method = "ward.D")

hc$labels <- m$vars

cor_df$p

caption = c('vars vs pH Pearson Coefficients.\nIt highlight the degree of association between variables')

cor_df %>%
  # mutate(p.signif = ifelse(p.signif == 'ns', NA, p.signif)) %>%
  ggplot(aes(name, vars, fill = cor)) + 
  geom_tile(color = 'white', size = 0.5) + # aes(alpha = 1-p)
  geom_text(aes(label = cor), color = 'black') +
  theme_classic(base_size = 12, base_family = "GillSans") +
  ggsci::scale_fill_gsea(name="Corr", reverse = T) +
  scale_fill_gradient2(low = '#D61E00', mid = 'white', high = '#4521AC') +
  scale_x_discrete(position = 'top') +
  labs(x = '', caption = caption) +
  ggh4x::scale_y_dendrogram(hclust = hc) +
  theme(axis.ticks.length = unit(3, "pt")) -> corheatplot

# 
# ggsave(corheatplot, filename = 'ph_vs_vars_cor_heatmap.png', path = ggsavepath, width =5, height = 2.5)


# Clustering ----
df %>% 
  mutate(name = recode_factor(name, !!!level_key)) %>%
  mutate(name = factor(name, levels = pHLevel)) -> df

df %>%
  # filter(!grepl('2021', date)) %>%
  ggplot(aes(y = TA, x = DIC, group = name, color = pH)) +
  facet_grid(~ name) +
  geom_smooth(se = F, method = lm, color = 'black', linetype = 'dashed') +
  # ggforce::geom_mark_hull(fill = 'grey', con.colour = 'grey') +
  # geom_point(size = 4, alpha = 0.7) +
  geom_text(aes(label = round(pH, 3))) +
  # scale_color_viridis_d(option = "plasma", end = .7) +
  theme(legend.position = 'top') +
  ggpubr::stat_cor(method = "pearson", 
    cor.coef.name = "R", 
    p.accuracy = 0.001) +
  theme_bw(base_family = "GillSans", base_size = 10) +
  scale_color_viridis_c('', option = "plasma", end = .7, direction = -1) +
  theme(legend.position = 'top') -> psave

ggsave(psave, filename = 'TA_DIC.png', path = ggsavepath, width = 6, height = 2.5)


# test cor b/ pH (measure vs predic) ----

# (a) Correlation between pH NBS,calc calculated from TA and TCO2 measured on discrete samples and pH NBS,sonde, corrected to the total hydrogen ion scale from the NBS-scale pH measured with the YSI 6000 EDS sonde, for those samples with good T, S agreement. pHT 5 0.45pHT,YSI +4.22, F1,12 5 26.80, p 5 0.0002, R2 5 0.70. (b) Relationship between aragonite saturation state (VA) and pHT,calc for all discrete samples (VA 5 20.288 + 0.9274e(1.7345DpHt), F2,23 5 248.84, pseudo-R2 5 0.96, p , 0.0001, where DpHT 5 pHT 2 7.5 and pseudo-R2 5 1 2 [residual sums of squares divided by the total corrected sums of squares], appropriate for nonlinear regressions).
# 

df %>%
  ggplot(aes(pH, pHNBS_predicted)) +
  geom_smooth(se = F, method = lm) +
  geom_point(alpha = 1, shape = 1) + # , aes(color = name)
  ggpubr::stat_cor(method = "pearson",
    cor.coef.name = "R", alternative = 'greater',
    p.accuracy = 0.001, size = 2.5, family = "GillSans") +
  scale_color_manual("", values = pHpalette) +
  theme_classic(base_family = "GillSans", base_size = 12) +
  labs(y  = expression("pH"["NBS"]~"(CO"[2]*"Sys)"), x = expression("pH"["NBS"])) -> p

ggsave(p, filename = 'predicted_measured_pH.png', 
  path = ggsavepath, width = 2.5, height = 2.5)


# test anova for difference between pH datasets ---- 

df %>%
  select(pH, pHNBS_predicted) %>%
  pivot_longer(-name, names_to = 'pH') %>%
  # shapiro_test(value) %>%
  # mutate(gauss = ifelse(p > 0.05, TRUE, FALSE)) 
  ungroup() %>%
  rstatix::anova_test(value ~ pH) 



# test  
# ggplot(df, aes(y = nDIC, x = pH, group = name, col = name)) +
#   geom_point() +
#   geom_smooth(se = F, method = lm, ) +
#   ggpubr::stat_cor(method = "pearson",
#     cor.coef.name = "R",
#     p.accuracy = 0.001) +
#   labs(y = 'DIC (Measured)') +
#   scale_color_viridis_d(option = "plasma", end = .7) +
#   theme(legend.position = 'top') -> ps
# 
# ps

# as wai cai 2022 

ggplot(df, aes(y = TA-DIC, x = pH, group = name, col = name)) + 
  geom_point() + 
  geom_smooth(se = F, method = lm, ) +
  ggpubr::stat_cor(method = "pearson", 
    cor.coef.name = "R", 
    p.accuracy = 0.001) +
  # labs(y = 'DIC (Measured)') +
  scale_color_viridis_d('', option = "plasma", end = .7) +
  theme(legend.position = 'top') +
  theme_bw(base_family = "GillSans", base_size = 10) +
  theme(legend.position = 'top') -> psave

ggsave(psave, filename = 'TA_less_DIC_pH.png', path = ggsavepath, width = 4, height = 3)

# THEREFORE LINEARITY
# Start regresion ----

lm_mod <- linear_reg() %>%
  set_engine("lm")

# Prediction ----
# Based on the pH, lets predict vars across names ----

vars <- c('DIC','TA','Sal', 'pHNBS_predicted','HCO3', 'CO3', 'pCO2_matm', 'Aragonite', 'Calcite') # mmol/kgSW


predict_vars <- function(form, data, new_data, lev = 0.95) {
  
  # formula <- formula(paste0(var, ' ~ pH * name'))
  formula <- formula(form)
  
  var <- form # sub(' ~ pH', '', form)
  
  lm_fit <- linear_reg() %>%
    set_engine("lm") %>%
    fit(formula, data = data)
  
  
  mean_pred <- predict(lm_fit, new_data = new_data)
  
  conf_int_pred <- predict(lm_fit, 
    new_data = new_data, 
    type = "conf_int", level = lev)
  
  # Now combine: 
  plot_data <- 
    new_data %>% 
    bind_cols(mean_pred) %>% 
    bind_cols(conf_int_pred) %>%
    arrange(name) %>%
    mutate(vars = var)
  
  out <- list('fit_data' = lm_fit$fit, 'data' = plot_data)
  
  return(out)
  
}

# predict_vars('DIC', df, new_points) 

fit_data <- df %>% filter(name == 8)

new_points %>% mutate(name = recode_factor(name, !!!level_key)) -> new_points

new_data <- new_points %>% filter(name == 8)

form <- 'DIC ~ pH'

predict_vars(form, fit_data, new_data) -> plot_data

plot_data$data %>%
  mutate(ymin = .pred_lower, ymax = .pred_upper) %>%
  ggplot(aes(y = .pred, x = 1:nrow(.), color = name)) + 
  # facet_grid(vars ~ ., scales = 'free_y') +
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, alpha = 0.3) +
  geom_path(aes(group = name), linejoin = "mitre")

  
# formula <- formula(paste0('DIC', ' ~ pH * name'))

summary(plot_data$fit_data)

check_model(plot_data$fit_data, check = "pp_check") # check = "pp_check"

# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)
# *Intercept)             1799.40   17430.37   0.103    0.920
# pH                        30.21    2163.43   0.014    0.989
# nameExperimental-I     -6550.22   17931.32  -0.365    0.723
# nameExperimental-II      210.74   17894.99   0.012    0.991
# pH:nameExperimental-I    868.39    2235.37   0.388    0.707
# pH:nameExperimental-II   -18.46    2227.89  -0.008    0.994

# The Pr(>|t|) column represents the p-value associated with the value in the t value column.
# If the p-value is less than a certain significance level (e.g. α = .05) then the predictor variable is said to have a statistically significant relationship with the response variable in the model.

# t value = Estimate / Std. Error
# Pr = 2 * pt(abs(t-value), 5, lower.tail = FALSE)

# Ex. 
tval <- 30.21/2163.43

2 * pt(abs(tval), 5, lower.tail = FALSE) # The Student t Distribution

# The beta estimate (or beta coeff) determines the direction and strength of the relationship between the two variables.
# A beta of zero suggests there is no association between the two variables. However, if the beta value is positive, the relationship is positive. If the value is negative, the relationship is negative. Further, the larger the number, the bigger the effect is. We’ll discuss effect size and how to interpret the value in more detail later in this lesson.

# Specifically, from the beta estimate, which is positive, we confirm that the relationship is positive (which we could also tell from the scatterplot). We can also interpret this beta estimate explicitly.


# 
# lm_fit %>% tidy() %>%
#   dwplot(dot_args = list(size = 2, color = "black"),
#     whisker_args = list(color = "black"),
#     vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

# check_model(lm_fit)

# model <- glm(formula, family = poisson, data = df)
# model <- rstanarm::stan_glm(formula = formula, data = df)

# check_model(model)
# rstanarm::prior_summary(model)
# check_zeroinflation(model)

out <- list()

# FORMULA ----
# interacción entre dos variables se definen usando el simbolo *

for(i in vars) {
  # cat('\nPredictin', i,' var using pH\n')
  form <- paste0(i, " ~ pH * name")
  
  cat('\nPredictin', i,' var using formula ', form, '\n')
  
  data <- predict_vars(form, df, new_points)
  
  data <- data$data
  
  out[[i]] <- data
}

do.call(rbind, out) -> plot_data


plot_data %>% 
  mutate(vars = sub(" ~ pH [*] name", '', vars)) -> plot_data

fileName <- paste0(getwd(), '/predicted_vars.rds')

write_rds(plot_data, file =fileName)

# ESTIMATE PERFORMANCE ----

fit_data <- c()

for(i in vars) {
  # cat('\nPredictin', i,' var using pH\n')
  form <- paste0(i, " ~ pH * name")
  
  cat('\nPredictin', i,' var using formula ', form, '\n')
  
  data <- predict_vars(form, df, new_points)
  
  fit_data[[i]] <- data$fit_data
}

fileName <- paste0(getwd(), '/fit_data_vars.rds')

write_rds(fit_data, fileName)

# Estimate these metrics:
# Root Mean Squared Error
lapply(fit_data, performance_rmse)
performance_rmse(fit_data$DIC) 
performance_mse(fit_data$a)
performance(fit_data$DIC)

do.call(rbind, lapply(fit_data, performance))
 
set.seed(1234)
size <- 100
times <- 10

obs <- df %>% group_by(name) %>% 
  sample_n(size, replace = TRUE) 

pred <-  sample_n(out$DIC, size, replace = TRUE) 

rsq_vec(obs$DIC, pred$.pred)


# 

plot_data %>% 
  # mutate(name = factor(name, levels = nameLevs)) %>%
  group_by(name, vars) %>%
  rstatix::get_summary_stats(.pred) %>%
  filter(vars %in% c('DIC', 'TA', 'CO3', 'Aragonite')) %>%
  select(name , vars, n, mean, median, sd, se) %>%
  arrange(vars) %>% view()


new_points %>% 
  group_by(name) %>%
  rstatix::get_summary_stats(pH) %>%
  select(name , n, mean, median, sd, se) %>%
  view()

# check_model(fit_data$CO2, check = "pp_check", panel = F)

# lapply(fit_data, function(x) check_model(x, check = "pp_check",  panel = F))

# and plot

plot_data %>% ungroup() %>% distinct(date, id) %>% 
  mutate(month = lubridate::month(date, label = T), 
    day = lubridate::day(date)) %>%
  group_by(date) %>% sample_n(1) -> date_reorder
  # pull(date) %>% as.character()

date_reorder %>% pull(id) -> breaks

date_reorder %>% mutate(date = paste0(month, '-', day)) %>% pull(date) -> date_labels

max <- length(breaks)

limits <- c(seq(1, 32, by = 7), max)

scale_x_date(labels = date_format("%b"))

plot_data %>% 
  ggplot(aes(y = pH, x = date, color = name)) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  # scale_x_reverse(breaks = breaks,labels = date_labels) +
  # scale_x_continuous('', breaks = breaks[limits], labels = date_labels[limits]) +
  # ylim(0,NA) +
  scale_x_date(labels = date_format("%d"),
    name = "time in days") +
  # geom_point()
  geom_line()
  # geom_path(aes(group = name), linejoin = "mitre")

plot_data %>%
  # filter(vars %in% c(varsf, 'CO2')) %>%
  # filter(vars %in% c("HCO3","CO3","Aragonite",'Calcite')) %>%
  filter(vars %in% c("Aragonite")) %>% # ,'Calcite'
  filter(.pred > 0 ) %>%
  drop_na(vars) %>%
  mutate(vars = factor(vars, levels = c(varsf, 'CO2'))) %>%
  mutate(ymin = .pred_lower, ymax = .pred_upper) %>%
  ggplot(aes(y = .pred, x = id, color = name)) + 
  # facet_grid(vars ~ ., scales = 'free_y') +
  theme_bw(base_family = "GillSans", base_size = 14) +
  # scale_x_reverse(breaks = breaks,labels = date_labels) +
  scale_x_continuous('', breaks = breaks[limits], labels = date_labels[limits]) +
  # ylim(0,NA) +
  geom_hline(yintercept = 1, color = 'grey', linetype = 'dashed') +
  geom_path(aes(group = name), linejoin = "mitre", size =1) +
  theme_classic(base_family = "GillSans", base_size = 16) +
  theme(legend.position = 'top', panel.border = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 45, 
      hjust = 1, vjust = 1, size = 10)) +
  labs(y = expression(Omega["ara"] ~ (Predicted))) +
  scale_color_manual("", values = pHpalette) -> psave

ggsave(psave, filename = 'time_serie_vars.png', 
  path = ggsavepath, width = 10, height = 5)

# ggsave(psave, filename = 'time_serie_vars_filtered.png', path = ggsavepath, width = 12, height = 7)

psave + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, alpha = 0.1) -> psave

ggsave(psave, filename = 'time_serie_vars_sd.png', 
  path = ggsavepath, width = 10, height = 5)
# ggsave(psave, filename = 'time_serie_vars_sd_filtered.png', path = ggsavepath, width = 12, height = 7)


plot_data %>%
  # filter(vars %in% c(varsf, 'CO2')) %>%
  filter(vars %in% c("DIC","TA", 'CO2')) %>%
  filter(.pred > 0 ) %>%
  mutate(vars = factor(vars, levels = c(varsf, 'CO2'))) %>%
  ggplot(aes(y = .pred, x = pH, color = name)) +
  geom_smooth(se = F, method = lm) +
  geom_point() +
  facet_wrap(vars ~ ., scales = 'free_y', nrow = 1) +
  # ggpubr::stat_cor(method = "pearson", 
  #   cor.coef.name = "R", alternative = 'greater',
  #   p.accuracy = 0.001) +
  scale_color_viridis_d(option = "plasma", end = .7)
  
# now pH

new_points %>%
  mutate(ymin = pH-se, ymax = pH+se) %>%
  ggplot(aes(y = pH, x = id, color = name)) + 
  theme_bw(base_family = "GillSans", base_size = 14) +
  geom_hline(yintercept = 8, color = 'grey', linetype = 'dashed') +
  geom_path(aes(group = name), linejoin = "mitre", size =1) +
  theme_classic(base_family = "GillSans", base_size = 16) +
  theme(legend.position = 'top', panel.border = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 45, 
      hjust = 1, vjust = 1, size = 10)) +
  labs(y = expression('pH')) +
  scale_color_manual("", values = pHpalette) -> psave

ggsave(psave, filename = 'time_serie_pH.png', 
  path = ggsavepath, width = 10, height = 5)


psave + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, alpha = 0.1) -> psave

library(ggdensity)

# https://github.com/jamesotto852/ggdensity

ggplot(plot_data, aes(pH, .pred, fill = name)) +
  geom_hdr() +  #
  geom_point(shape = 21) +
  facet_wrap(vars ~ ., scales = 'free_y')
# 
# library(patchwork)
# 
# subtitle <- 'Measured vss Predicted DIC using lm'
# 
# ps + ppredic + 
#   patchwork::plot_annotation(subtitle = subtitle)


# 

# Beta prediction (Inferences) -----
# 
# summary(fit_data$DIC)
# 
# fit_data$DIC$coefficients

# t value = Estimate / Std. Error
# Pr = 2 * pt(abs(t-value), 5, lower.tail = FALSE)


# lapply(fit_data, function(x) check_model(x, check = "pp_check",  panel = F))
# 

# Priori ----

# plot_data %>% 
#   group_by(vars)%>%
#   rstatix::t_test(.pred ~ name) -> t.stats 

# plot_data %>%
#   group_by(vars)%>%
#   rstatix::kruskal_test(.pred ~ name) %>%
#   adjust_pvalue(method = "none") %>%
#   add_significance("p")

# he Welch one-way test is an alternative to the standard one-way ANOVA in the situation where the homogeneity of variance can’t be assumed (i.e., Levene test is significant).


plot_data %>%
  group_by(name, vars) %>% 
  filter(vars %in% c('TA', 'DIC')) %>%
  shapiro_test(.pred) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") %>%
  mutate(gauss = ifelse(p > 0.05, TRUE, FALSE))

plot_data %>%
  filter(vars %in% varsf) %>%
  group_by(vars)%>%
  rstatix::welch_anova_test(.pred ~ name) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> anova.test

plot_data %>%
  filter(vars %in% varsf) %>%
  group_by(vars)%>%
  rstatix::kruskal_test(.pred ~ name) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p") -> prior.test



# Posteriori ----

# 

plot_data %>%
  filter(vars %in% varsf) %>%
  group_by(vars) %>%
  pairwise_wilcox_test(.pred ~ name, ref.group = '8', conf.level = 0.95) %>%
  # tukey_hsd(.pred ~ name) %>% # If ANOVA used as priori
  adjust_pvalue() %>%
  add_significance() -> stats.test

stats.test %>% add_xy_position(x = "name", scales = "free_y") %>% 
  mutate(vars = factor(vars, levels = varsf)) -> stats

# stats$y.position <- max(out_stats$a)+out_stats$sd

title <- get_pwc_label(stats)

subtitle <- get_test_label(prior.test, detailed = TRUE)

# and plot

# value ~ name

plot_data %>%
  mutate(name = factor(name, levels = pHLevel)) %>%
  filter(vars %in% varsf) %>%
  mutate(vars = factor(vars, levels = varsf)) %>%
  ggplot(aes(y = .pred, x = name)) +
  facet_wrap(~ vars, scales = 'free_y', nrow = 1) +
  theme_bw(base_family = "GillSans", base_size = 14) +
  stat_boxplot(aes(color = name), geom ='errorbar', width = 0.07) +
  geom_boxplot(aes(color = name, fill = name), width = 0.3, outlier.alpha = 0) +
  # geom_point(alpha = 0.5) +
  scale_color_manual("", values = pHpalette) +
  scale_fill_manual("", values = pHpalette) +
  stat_summary(fun=median, geom="point", shape=23, size=1, color = 'red') +
  theme_bw(base_family = "GillSans", base_size = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(y = '', x = '') +
  stat_summary(fun=median, geom ="line", aes(group = 2), size= 0.5, color = 'blue')  -> psave

psave + theme(strip.background = element_rect(fill = 'grey', color = 'white'),
  panel.border = element_blank()) -> psave

psave + 
  ggpubr::stat_pvalue_manual(stats, label = "p.adj.signif", 
    remove.bracket = F, tip.length = 0.01, linetype = 'dashed', hide.ns = T) +
  labs(subtitle = subtitle[[1]], title = title) -> psave


ggsave(psave, filename = 'stasts_vars_from_ml.png', path = ggsavepath, 
  width = 10, height = 5)

# continue w/ bayesian modeling (to noisy!!) ----

# In Bayesian modeling analysis, a prior distribution needs to be declared for each model parameter that represents the possible values of the parameters (before being exposed to the observed data).

# set the prior distribution
# 1) Configuring-C---Toolchain-for-Mac
# 2)

# install.packages("rstanarm")

library(rstanarm)

prior_dist <- rstanarm::student_t(df = 1)

set.seed(20220502)

# make the parsnip model
bayes_mod <-   
  linear_reg() %>% 
  set_engine("stan", 
    prior_intercept = prior_dist, 
    prior = prior_dist) 

# train the model
# take a while!!

bayes_fit <- 
  bayes_mod %>% 
  fit(DIC ~ pH * name, data = df)
  # fit(width ~ initial_volume * food_regime, data = urchins)

# 8: Markov chains did not converge! Do not analyze results! 

print(bayes_fit, digits = 5)

broom.mixed::tidy(bayes_fit, conf.int = TRUE)

bayes_plot_data <- 
  new_points %>% 
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = pH)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "urchin size") + 
  ggtitle("Bayesian model with t(1) prior distribution")

