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
library(jsonlite)
library(curl)


##Carga de bases

w_mean_depto_total_letra <- read_csv("./bases/w_mean_depto_total_letra.csv")
diccionario_clase2 <- read_csv("./bases/diccionario_clae2.csv")

##Agrego la descripcion de las letras####

clase2_letra <- diccionario_clase2 %>% #Problemas para quedarme con una tabla de descripción de clae con Pipe.
  select(letra,letra_desc)

dicc_clase2_letra <- clase2_letra[!duplicated(clase2_letra$letra), ]

w_mean_total_descletra <- left_join(dicc_clase2_letra, w_mean_depto_total_letra,c("letra")) #tiene menos obs. Revisar.


###Consgina 2
base_filtrada_fecha <- w_mean_total_descletra %>%
  filter(fecha =="2023-04-01") %>%
arrange(w_mean)


cuadro <- ggplot(base_filtrada_fecha, aes(x=letra_desc, y= mean(w_mean))) +
  geom_col() +
  scale_color_viridis_d() +
  scale_y_log10(labels=scales::comma) +
  theme_minimal() +
  labs(title = "Media salarial por sector de actividad", 
       subtitle = "según datos de abril 2023",
       x = "Sector de actividad", 
       y = "Salario promedio")
  
print(cuadro)


##Consigna evolución salarial###

diccionario_cod_depto <- read_csv("./bases/diccionario_cod_depto.csv")

salario_total_promedio_descrip <- left_join(diccionario_cod_depto,w_mean_total_descletra,
                                            c("id_provincia_indec","codigo_departamento_indec"))
head(salario_total_promedio_descrip)


##Consigna mapa###

archivo_web <- "https://infra.datos.gob.ar/catalog/modernizacion/dataset/7/distribution/7.3/download/departamentos.json"

fromJSON(archivo_web)


mapa_joint <- left_join(archivo_web, salario_total_promedio_descrip,
                        c("id","codigo_departamento_indec"))


            