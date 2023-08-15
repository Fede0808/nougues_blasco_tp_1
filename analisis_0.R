#libreria ####
library(readr)
library(tidyverse)
library(ggplot2)

#bases ####
w_mean_depto_total_letra <- read_csv("w_mean_depto_total_letra.csv")
diccionario_clase2 <- read_csv('diccionario_clae2.csv')
diccionario_cod_depto <- read_csv('diccionario_cod_depto.csv')

##Agrego la descripcion de las letras####

clase2_letra <- diccionario_clase2 %>% #Problemas para quedarme con una tabla de descripción de clae con Pipe.
 select(letra,letra_desc)

dicc_clase2_letra <- clase2_letra[!duplicated(clase2_letra$letra), ]
  
w_mean_total_descletra <- left_join(dicc_clase2_letra, w_mean_depto_total_letra,c("letra")) #tiene menos obs. Revisar.

#rm(w_mean_depto_total_letra)

#rm(clase2_letra)

#rm(diccionario_clase2)
##Agrego descripcion depto y provincia.####

salario_total_promedio_descrip <- left_join(diccionario_cod_depto,w_mean_total_descletra,
                                            c("id_provincia_indec","codigo_departamento_indec")) #Revisar. Sigue achicando.


salario_total_promedio_descrip <- salario_total_promedio_descrip %>% 
  mutate(ano =  format(fecha, '%Y'))%>%
  filter(ano != '2023', w_mean > '0')  
  


#rm(w_mean_total_descletra)
#analisis ####

##Los 5 sectores de actividad con salarios más bajos, expresados en un gráfico de barras.####

##Elija 4 sectores de actividad (los cuales se distinguen por la letra) o grupos de sectores ####
#y visualice la evolución de los salarios a lo largo de los años disponibles. 

#Elijo los 2 sectores con salario más alto y los 2 con salario más bajo.

top5_salario_prom <- salario_total_promedio_descrip %>%
  group_by(ano,letra_desc) %>%
  summarise(salario = mean(w_mean)) %>%
  arrange(desc(salario)) %>%
  head(5)


ult5_salario_prom <- salario_total_promedio_descrip %>%
  filter( ano == '2022') %>% 
  group_by(ano,letra_desc) %>%
  summarise(salario = mean(w_mean)) %>%
  tail(5) %>% 
  arrange(salario)

muestra <- c('B','D','L','S')


dispersion_wmean <- ggplot(salario_total_promedio_descrip,
                           aes(x=as.Date(fecha,
                                             format= '%Y'),y= w_mean))+
  geom_point()+
  scale_y_log10(labels=scales::comma)

ggsave('dispersion_wmean.png',plot = dispersion_wmean,dpi = 300)

#agrupo por actividad a nivel nacional. 

sal_prom_nacional <- salario_total_promedio_descrip %>% 
  subset(letra %in% muestra) %>%  
  group_by(fecha,letra_desc) %>% 
  summarise(salario = mean(w_mean))

  dispersion_wmean_4 <- ggplot(sal_prom_nacional,
                             aes(x=fecha,y= salario, color = letra_desc))+
    geom_point()+
    scale_color_discrete(guide = guide_legend(nrow = 2))+
    scale_y_log10(labels=scales::comma)+
    geom_smooth(method = 'loess')+
    theme(legend.position = "bottom")+
    labs(title = "Evolucion de salarios", 
         subtitle = "Reune las 2 actividades con mayor y con menor promedio anual del salario promedio.",
         color = "Actividades", 
         x = "Ano", 
         y = "Salario")
  
  

ggsave('dispecion_4a.png', plot = dispersion_wmean_4, dpi = 300) #¿habria que agrupar a nivel pais?

print(dispersion_wmean_4)
