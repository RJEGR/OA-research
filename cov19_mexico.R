library(data.table)
library(tidyverse)

x <- read.table('~/Documents/Mexico_COVID19.csv', sep = ",", header = TRUE)
x <- as_tibble(x)

# Source from: https://github.com/carranco-sga/Mexico-COVID-19#notification-settings
# I: representa el número de casos confirmados(°) importados del extranjero al estado.
# L: representa el número de casos confirmados locales dados en el estado.
# ninguno: representa el número total de casos confirmados en el estado.
# R: representa el número total de casos confirmados recuperados(°°) en el estado.
# D: representa el número total de casos confirmados fallecidos en el estado.

totals <- c("Pos_I","Pos_L","Pos","Pos_rep",
            "Susp_rep","Neg_rep","IRAG_Test",
            "Tested_tot","Recovered","Deceased")

x[!names(x) %in% totals] %>%
  reshape2::melt() %>%
  as_tibble() -> y

y$Status <- NA

y[grep("D", y$variable), 'Status'] <- "Muertes"
y[grep("R", y$variable), 'Status'] <- "Recuperados"
y[grep("L", y$variable), 'Status'] <- "Local"
y[grep("I", y$variable), 'Status'] <- "Internacional"

#
s <- as.character(y$variable)

estados <- sapply(strsplit(s, "_"), `[`, 1)

dv <- data.frame(y, Estado = estados) %>% as_tibble()




# dv[Status %in% "Muertes", "Local", wrap:= 'Origen')

agg <- aggregate(dv$value, by = list(dv$Estado), sum)
agg <- data.frame(Estado = agg[,1], casos = agg[,2], stringsAsFactors = F)

lev <- agg[order(agg$casos, decreasing = TRUE), 'Estado']                  
dv$Estado <- factor(dv$Estado, levels = lev)

dv <- filter(dv, !is.na(Status) & value > 0)

bardat <- aggregate(dv$value, by = list(dv$Estado, 
                                        dv$Status), sum)

bardat <- data.frame(Estado = bardat[,1], 
                     Status = bardat[,2], 
                     Casos = bardat[,3])
require(ggplot2)

bar <- bardat %>%
  ggplot(aes(x = Estado, y = Casos)) +
  geom_bar(aes(fill = Status), stat = "Identity", color = "black") +
  coord_flip() +
  facet_grid(~Status) +
  scale_fill_brewer() +
  theme_bw() +
  labs(y = 'Casos\nAcumulados', title = "COVID-19 en México", 
       fill = "Origen",
       subtitle = "Número de casos confirmados hasta la fecha 2020-03-19 T 21:00:00-06:00")

dv %>% 
  group_by(Fecha) %>% 
  summarise(n = n()) %>%
  mutate(n_acumulada = cumsum(n)) -> dv_groups

linep <- dv_groups %>%
  ggplot(aes(x = Fecha, y = n_acumulada, group = Fecha)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust=1, size = 7), 
        axis.text.y = element_text(size = 7)) +
  labs(y = "Casos Acumulados", caption = "Autor grafica: Ricardo Gomez Reyes,\nBase de datos de: https://github.com/carranco-sga/Mexico-COVID-19")

library(patchwork)
save <- bar / linep

ggsave(save, filename = "~/Documents/Mexico_COVID19.png", width = 10, height = 8)

# modeling exponential growth
# https://towardsdatascience.com/modeling-exponential-growth-49a2b6f22e1f

# http://gabgoh.github.io/COVID/index.html

# https://annals.org/aim/fullarticle/2760912/reporting-epidemic-growth-reproduction-numbers-2019-novel-coronavirus-2019-ncov

# epidemic calculator:
# http://gabgoh.github.io/COVID/index.html

# ex curve in r
# https://stackoverflow.com/questions/31851936/exponential-curve-fitting-in-r

# america
# https://elpais.com/sociedad/2020/03/18/actualidad/1584535031_223995.html?ssm=FB_CC
