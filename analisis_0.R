#libreria ####
library(readr)
library(tidyverse)

#bases ####
w_mean_depto_total_letra <- read_csv("w_mean_depto_total_letra.csv")
diccionario_clase2 <- read_csv('diccionario_clae2.csv')
diccionario_cod_depto <- read_csv('diccionario_cod_depto.csv')

##Agrego la descripcion de las letras####

clase2_letra <- diccionario_clase2 %>% #Problemas para quedarme con una tabla de descripción de clae.
 select(letra,letra_desc)

dicc_clase2_letra <- clase2_letra[!duplicated(clase2_letra$letra), ]
  
w_mean_total_descletra <- left_join(dicc_clase2_letra, w_mean_depto_total_letra,c("letra")) #tiene menos obs. Revisar.


##Agrego descripcion depto y provincia.####

salario_total_promedio_descrip <- left_join(diccionario_cod_depto,w_mean_total_descletra,
                                            c("id_provincia_indec","codigo_departamento_indec")) #Revisar. Sigue achicando.

#analisis ####


