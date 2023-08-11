# Paquetes
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(viridis)
library(scales)
library(lubridate)
library(cowplot)
library(ggtext)
library(xfun)
library(ggrepel)

##Carga de bases

base <- read.csv("w_mean_depto_total_letra.csv")
head(base)
class(base$fecha)

###Consgina 2
base_filtrada_fecha <- base %>%
  filter(fecha =="2023-04-01")

tabla1 <- base_filtrada_fecha %>% #Entiendo que esta tabla es tabla_media_sector
  select(letra, w_mean) %>% 
  group_by(letra) %>% 
  summarize(mean(w_mean))

grafo_media_sector <- ggplot(tabla_media_sector, aes(x = letra, y = w_mean)) + #Error: object 'tabla_media_sector' not found
  geom_col() +
  scale_y_log10(labels=scales::comma) +
  theme_minimal() +
  labs(title = "Media salarial por sector de actividad", 
       subtitle = "según datos de abril 2023",
       x = "Sector de actividad", 
       y = "Salario promedio")

print(grafo_media_sector)
            