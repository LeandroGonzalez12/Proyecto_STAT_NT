library(lubridate)
library(shiny)
library(tidyverse)
library(ggspectra)
library(maps)
library(dplyr)
library(maptools)
library(rgdal)
library(here)
require(viridis)
library(sf)
library(plotly)
library(shinyalert)
library(shinydashboard)


uruguay <- read_csv(here("data","Uruguay.csv")) #Cargamos datos de vacunacion.

estadisticasUY <- read_csv("data/estadisticasUY.csv", 
                           col_types = cols(fecha = col_date(format = "%d/%m/%Y")))
estadisticasUY<-estadisticasUY %>% filter(between(fecha, as.Date('2021-02-27', format='%Y-%m-%d'),
                                                  as.Date('2021-06-23', format='%Y-%m-%d')))



# Se renombran para facilitar la interpretacion.
uruguay <- uruguay %>% rename(diario_coronavac = daily_coronavac,
                              acum_coronavac = total_coronavac,primDiario_coronavac = people_coronavac,
                              segDiario_coronavac  = fully_coronavac,diario_pfizer = daily_pfizer,
                              acum_pfizer = total_pfizer,primDiario_pfizer = people_pfizer,
                              segDiario_pfizer = fully_pfizer,diario_astrazeneca = daily_astrazeneca,
                              acum_astrazeneca = total_astrazeneca,primDiario_astrazeneca = people_astrazeneca,
                              segDiario_astrazeneca = fully_astrazeneca) %>%
  filter(date >= "2021-02-27" & date <="2021-06-23")


##GRAFICOS 1 Y 2 del Rmd
## A partir de Uruguay se crea la base "labs" para hacer los dos primeros gráficos
total_lab <- select(uruguay,date,diario_coronavac,acum_coronavac,primDiario_coronavac,
                    segDiario_coronavac,diario_pfizer,acum_pfizer,primDiario_pfizer,segDiario_pfizer,
                    diario_astrazeneca,acum_astrazeneca,primDiario_astrazeneca,segDiario_astrazeneca,
                    people_vaccinated,people_fully_vaccinated,
                    daily_vaccinated,total_vaccinations,daily_agenda,daily_pogress)

labs <- total_lab %>% select(date,c(contains("pfizer"),contains("coronavac"),
                                    contains("astrazeneca")))

labs <-labs %>%  pivot_longer(cols = c(ends_with("astrazeneca"),ends_with("pfizer"),
                                       ends_with("coronavac")),names_to = "laboratorio",values_to = "numeros")
labs <- labs %>% separate(col = laboratorio, into = c("arranca","lab"))
labs <-labs %>% pivot_wider(names_from = arranca,values_from = numeros) 

#borro lo auxiliar
rm(total_lab)

## GRAFICOS 3 Y 4 del Rmd

completas_pais_hasta_mayo_R<- uruguay %>% 
  select(date, total_vaccinations, people_vaccinated) %>%
  filter(date == "2021-05-26") 

completas_pais_hasta_junio_R<- uruguay %>% 
  select(date, total_vaccinations, people_fully_vaccinated) %>%
  filter(date == "2021-06-23") 


completas_pais<-merge(x=completas_pais_hasta_mayo_R, y=completas_pais_hasta_junio_R, all=TRUE) %>% 
  pivot_longer(cols = c("people_vaccinated","people_fully_vaccinated"),
               names_to = "Numero_dosis", 
               values_to ="Cant_dosis") %>% 
  group_by(Numero_dosis) %>% 
  summarise(Cant_dosis = sum(Cant_dosis, na.rm=TRUE)) %>% 
  group_by(Numero_dosis, Cant_dosis) %>% 
  summarise(porcentaje=round(Cant_dosis/1694513*100, 2))

##en la app se usa lo siguiente

completas_pais_hasta_mayo<- uruguay %>% 
  select(date, total_vaccinations, people_vaccinated) %>%
  filter(date >= "2021-02-27" & date <="2021-05-26")

completas_pais_hasta_junio<- uruguay %>% 
  select(date, total_vaccinations, people_fully_vaccinated) %>%
  filter(date >= "2021-02-27" & date <="2021-06-23")

completitas<-
  merge(x=completas_pais_hasta_mayo, y=completas_pais_hasta_junio, all=TRUE) %>% 
  rename(fecha = date,
         vacunaciones_totales = total_vaccinations,
         primera = people_vaccinated,
         segunda = people_fully_vaccinated) %>%  
  pivot_longer(cols = c("primera","segunda"),names_to = "dosis",values_to = "cantidad")



#borro lo auxiliar
rm(completas_pais_hasta_junio,completas_pais_hasta_junio_R,
   completas_pais_hasta_mayo,completas_pais_hasta_mayo_R)


#GRAFICO 5 DEL Rmd
departamentos <- read_csv("data/Regions.csv")
departamentos <- departamentos %>%
                 rename(Fully_Vaccinated='Fully Vaccinated',Departamento=Region)


#GRAFICOS 6 y 7 DEL Rmd
#Se cargan los datos del GUIAD
por_depto <- read_csv("data/estadisticasUY_porDepto_detalle.csv")
por_depto$fecha = as.Date(por_depto$fecha, format = "%d/%m/%Y")
por_depto <- por_depto %>% mutate(departamento = recode(departamento, 
                                                        'Artigas(UY-AR)'='Artigas', 'Canelones(UY-CA)'='Canelones',
                                                        'Cerro Largo(UY-CL)'='Cerro Largo', 'Colonia(UY-CO)'='Colonia',
                                                        'Durazno(UY-DU)'='Durazno','Flores(UY-FS)'='Flores','Florida(UY-FD)'='Florida',
                                                        'Lavalleja(UY-LA)'='Lavalleja', 'Maldonado(UY-MA)'='Maldonado',
                                                        'Montevideo(UY-MO)'='Montevideo','Paysandú(UY-PA)'='Paysandú', 
                                                        'Río Negro(UY-RN)'='Río Negro','Rivera(UY-RV)'='Rivera','Rocha(UY-RO)'='Rocha',
                                                        'Salto(UY-SA)'='Salto','San José(UY-SJ)'='San José','Soriano(UY-SO)'='Soriano',
                                                        'Tacuarembó(UY-TA)'='Tacuarembó', 'Treinta y Tres(UY-TT)'='Treinta y Tres'))



#GRAFICOS 8, 9 y 10 del Rmd
muertes_edad <- read_csv(here("data",'Deaths.csv'))

#De dichos datos, se eligen las primeras 17 columnas, 
#ya que las otras son tramos distintos de edad.
#Estos tramos son excluyentes y cubren de 0 a 115

muertes_edad <-muertes_edad %>% select(1:17) %>% 
  pivot_longer(cols = c(starts_with("total"),starts_with("daily")),
               names_to = "tramo_etario",values_to = "numeros")
muertes_edad <-muertes_edad %>% separate(col = tramo_etario, 
                                         into = c("arranca","tramo"),sep = 5)
muertes_edad <-muertes_edad %>% pivot_wider(names_from = arranca,values_from = numeros)
muertes_edad <-muertes_edad %>% mutate(tramo = recode(tramo, '_0_17'  = '[0,17]' , 
                                                      '_18_24'  = '[18,24]' ,'_25_34' = '[25,34]', '_35_44'  = '[35,44]' ,
                                                      '_45_54' = '[45,54]','_55_64'  = '[55,64]' ,'_65_74' = '[65,74]', '_75_115' = '[75,115]')) %>% 
  mutate(tramo_etario = as.factor(tramo)) %>% select(-tramo)

muertes_diarias <- muertes_edad %>% group_by(date) %>% 
  summarise(muertes_totales_diarias=sum(daily))

personas_totalmente_vacunadas <- uruguay %>% select(date, people_fully_vaccinated)

muertes_diarias <- muertes_edad %>% group_by(date) %>% 
  summarise(muertes_totales_diarias=sum(daily))

personas_totalmente_vacunadas <- uruguay %>% select(date, people_fully_vaccinated)


muertes_vacunas<-left_join(personas_totalmente_vacunadas, estadisticasUY, by=c('date'='fecha'))


## borramos lo auxiliar
rm(muertes_edad,muertes_diarias,personas_totalmente_vacunadas)

## PARA HACER MAPA EN LA APP
#datos necesarios para el mapita

sp_depto <- readOGR(here("data","ine_depto.shp"))
dframe_depto <- ggplot2::fortify(sp_depto) #conviere a data.frame

dframe_depto <- dframe_depto %>% filter(id!=15) %>% rename( Departamento = id) %>% 
  mutate(Departamento= recode(Departamento, '0'='Montevideo', '1'='Artigas',
                              '2'='Canelones','3'='Colonia','4'='Durazno', 
                              '5'='Florida','6'='Lavalleja','7'='Paysandú','8'='Río Negro',
                              '9'='Rivera','10'='Rocha','11'='Salto','12'='San José',
                              '13'='Soriano','14'='Treinta y Tres',
                              '16'='Tacuarembó','17'='Flores',
                              '18'='Maldonado','19'='Cerro Largo'))

datos_mapa <- left_join(dframe_depto , departamentos, by = "Departamento")
lab.data <- datos_mapa %>%
  group_by(depa=Departamento) %>%
  summarise(long = mean(long), lat = mean(lat))

# borro lo auxiliar
rm(sp_depto,dframe_depto)

