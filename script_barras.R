# Paquetes
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(viridis)
library(scales)
library(lubridate)
library(ggtext)

##Carga de bases

w_mean_depto_total_letra <- read_csv("w_mean_depto_total_letra.csv")
diccionario_clase2 <- read_csv("diccionario_clae2.csv")

##Agrego la descripcion de las letras####

clase2_letra <- diccionario_clase2 %>% #Problemas para quedarme con una tabla de descripción de clae con Pipe.
  select(letra,letra_desc)

dicc_clase2_letra <- clase2_letra[!duplicated(clase2_letra$letra), ]

w_mean_total_descletra <- left_join(dicc_clase2_letra, w_mean_depto_total_letra,c("letra")) #tiene menos obs. Revisar.


###Consgina 2
base_filtrada_fecha <- w_mean_total_descletra %>%
  mutate(ano =  format(fecha, '%Y'))%>%
  filter(ano == '2023')


top_salarios_sector <- base_filtrada_fecha %>%
  group_by(ano, letra_desc)%>%
  summarise(salario = mean(w_mean)) %>%
  head(5) %>% 
  arrange(desc(salario))

print(top_salarios_sector)

tail_salarios_sector <- base_filtrada_fecha %>%
  group_by(ano, letra_desc)%>%
  summarise(salario = mean(w_mean)) %>%
  tail(5) %>% 
  arrange(salario)

muestra_sec <- c("O", "F", "G", "A", "N")

muestra_sectores <- base_filtrada_fecha %>%
  subset((letra%in%muestra_sec)) %>%
  group_by(letra) %>%
  summarise(salario = mean(w_mean)) %>%
  arrange(salario)


  
##Gráfico de barras
grafico_barras <- ggplot(muestra_sectores, aes(x=salario, y= letra)) +
  geom_col() +
  scale_x_log10(labels=scales::comma)+
  scale_color_viridis_d() +
  labs(title = "Salario promedio por sector de actividad", 
       subtitle = "datos 2023",
       x = "Salario promedio", 
       y = "Sector de actividad")

ggsave('barras_salario_sector.png', plot = cuadro, dpi = 300)

