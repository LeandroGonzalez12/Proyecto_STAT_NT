rm(list = ls())
library(tidyverse)

###cargo los datos

##datos sobre vacunacion por departamentos, tipo de vacuna, etc; actualizado
#hasta el 23/6
uruguay      <- readr::read_csv("data/Uruguay.csv")


#investigando las variables
# uruguay %>% 
#   select(5,8,22:27) %>% 
#   head(20) %>% 
#   View()
# 
# uruguay %>% 
#   select(5,8,22:27) %>% 
#   tail(20) %>% 
#   View()

### daily_agend_diff = [(daily_agenda_ini - daily_agenda) / daily_agenda_ini] * 100

### dailypogress     = (daily_vaccinated / daily_agenda) * 100

### dailyagenda      = daily_agenda_first + daily_agenda_second 
## al principio no, serán solo first y no especifican?
###
# uruguay %>% select(people_coronavac,total_coronavac,daily_coronavac,fully_coronavac,
#                   people_mo,total_mo,daily_mo,fully_mo) %>% 
#   head(20) %>% 
#   View()

##people_coronavac + fully_coronavac = daily_coronavac
## people_mo + fully_mo = total_mo
# uruguay %>% select(people_coronavac,total_coronavac,daily_coronavac,fully_coronavac,
#                   people_mo,total_mo,daily_mo,fully_mo) %>% 
#   tail(20) %>% 
#   View()

## se observa que lo anterior se cumple para las demás vacunas y departamentos
# uruguay %>% select(people_pfizer,total_pfizer,daily_pfizer,fully_pfizer,
#                   people_ma,total_ma,daily_ma,fully_ma) %>% 
#   tail(20) %>% 
#   View()

## interpreto que cuando refiere a laboratorio de vacunas: 
### fully= cantidad de segundas dosis en el día con esa vacuna
### people= cantidad de primeras dosis en el día con esa vacuna

##POR OTRA PARTE
## interpreto que cuando refiere a departamentos: 
### fully= cantidad de segundas dosis acumuladas (totales) en el departamento
### people= cantidad de primeras dosis acumuladas (totales) en el departamento
##

## de acuerdo a lo anterior renombro en la base uruguay las variables referidas a laboratorios
uruguay <-
  uruguay %>% 
  rename(diario_coronavac      = daily_coronavac,
         acum_coronavac        = total_coronavac,
         primDiario_coronavac = people_coronavac,
         segDiario_coronavac  = fully_coronavac,
         diario_pfizer         = daily_pfizer,
         acum_pfizer           = total_pfizer,
         primDiario_pfizer    = people_pfizer,
         segDiario_pfizer     = fully_pfizer,
         diario_astrazeneca       = daily_astrazeneca,
         acum_astrazeneca         = total_astrazeneca,
         primDiario_astrazeneca  = people_astrazeneca,
         segDiario_astrazeneca   = fully_astrazeneca)

prueba <-
  uruguay %>% select(-c(1,3:29),-contains("res")) %>% 
  pivot_longer(cols = c(starts_with("total"),starts_with("fully"),
                        starts_with("daily"),starts_with("people")),
               names_to = "departamento",
               values_to = c("total"))


prueba <-
  prueba %>% 
  separate(col = departamento,into = c("arranca","depar"))

prueba <-
  prueba %>% 
  pivot_wider(names_from = arranca,
              values_from = total)

prueba <-
  prueba %>% mutate(depar = 
                      recode(depar, 'ar'='Artigas', 'ca'='Canelones', 
                             'cl'='Cerro Largo', 'co'='Colonia', 
                             'du'='Durazno','fd'='Florida', 'fs'='Flores', 
                             'la'='Lavalleja', 'ma'='Maldonado','mo'='Montevideo',
                             'pa'='Paysandú', 'rn'='Río Negro','ro'='Rocha','rv'='Rivera',
                             'sa'='Salto','sj'='San José','so'='Soriano',
                             'ta'='Tacuarembó', 'tt'='Treinta y Tres')) %>% 
  mutate(depar = as.factor(depar))

class(prueba$depar)

##creo el siguiente data.frame con los tipo de vacuna 
total_lab <- 
  select(uruguay,date,diario_coronavac,      
         acum_coronavac,        
         primDiario_coronavac, 
         segDiario_coronavac,  
         diario_pfizer,         
         acum_pfizer,           
         primDiario_pfizer,    
         segDiario_pfizer,     
         diario_astrazeneca,       
         acum_astrazeneca,         
         primDiario_astrazeneca,  
         segDiario_astrazeneca,
         people_vaccinated,
         people_fully_vaccinated,
         daily_vaccinated,
         total_vaccinations,
         daily_agenda,
         daily_pogress)


##actualizado, muestra datos de muerte segun edad, pero no por departamento
#hasta el 22/6
muertes_edad <- read_csv("data/Deaths.csv")

##elijo las primeras 17 porque las otras son tramos distintos y es para entreverar,
# estos tramos son excluyentes y cubren de 0 a 115

muertes_edad <-
  muertes_edad %>% select(1:17) %>%  
  pivot_longer(cols = c(starts_with("total"),starts_with("daily")),
               names_to = "tramo_etario",
               values_to = "numeros")


muertes_edad <-
  muertes_edad %>% 
  separate(col = tramo_etario, into = c("arranca","tramo"),sep = 5)

muertes_edad <-
  muertes_edad %>% 
  pivot_wider(names_from = arranca,
              values_from = numeros)


muertes_edad <-
  muertes_edad %>% mutate(tramo = 
                            recode(tramo, '_0_17'  = '[0,17]' , '_18_24'  = '[18,24]' , 
                                   '_25_34' = '[25,34]', '_35_44'  = '[35,44]' ,
                                   '_45_54' = '[45,54]', '_55_64'  = '[55,64]' ,
                                   '_65_74' = '[65,74]', '_75_115' = '[75,115]')) %>% 
  mutate(tramo_etario = as.factor(tramo)) %>% 
  select(-tramo)





##porcentaje de vacunados con al menos una dosis actualmente
(uruguay$people_vaccinated[117]       / 3543025) * 100

##porcentaje de vacunados con ambas dosis
(uruguay$people_fully_vaccinated[117] / 3543025) * 100


###cargo datos del guiad
por_depto <- read_csv("data/estadisticasUY_porDepto_detalle.csv")

nacional   <- read_csv("data/estadisticasUY.csv")



#####

prueba %>% 
  filter(date == as.Date("2021-06-23")) %>% 
  select(depar,total,fully,people,daily)

prueba %>% filter(depar == "Montevideo") %>% 
  ggplot(aes(x = date, y = daily)) +
  geom_line() +
  theme(aspect.ratio = 1) +
  scale_x_date(date_breaks = "1 day",labels = NULL)

### separo la base en 2

labs <- total_lab %>% 
  select(date,c(contains("pfizer"),contains("coronavac"),
                contains("astrazeneca")))

total <- total_lab %>% 
  select(-c(contains("pfizer"),contains("coronavac"),
            contains("astrazeneca")))

rm(total_lab)
### le hago pivot_longer a las vacunas
labs <-
  labs %>%  
  pivot_longer(cols = c(ends_with("astrazeneca"),ends_with("pfizer"),
                        ends_with("coronavac")),
               names_to = "laboratorio",
               values_to = "numeros")


labs <-
  labs %>% 
  separate(col = laboratorio, into = c("arranca","lab"))

labs <-
  labs %>% 
  pivot_wider(names_from = arranca,
              values_from = numeros)

### Pregunta 5)
##Evolución de las vacunaciones diarias segun laboratorio
labs %>% 
  ggplot(aes(x = date, y = diario, colour = lab)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "10 days") +
  theme(aspect.ratio = 1,
        axis.text.x = element_text(size = 7, angle = 300, hjust = 0)) +
  labs(x = "Fecha",
       y = "Cantidad de dosis en el día",
       colour = "Laboratorio")

###Evolución de las segundas dosis diarias segun laboratorio
labs %>% 
  ggplot(aes(x = date, y = segDiario, colour = lab)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "10 days") +
  theme(aspect.ratio = 1,
        axis.text.x = element_text(size = 7, angle = 300, hjust = 0)) +
  labs(x = "Fecha",
       y = "Cantidad de segundas dosis en el día",
       colour = "Laboratorio")

##Porcentaje de vacunados con 1era o 2da dosis
tabg <-
  labs %>%
  group_by(lab) %>% 
  summarise(max_acum  = max(acum)) 

tabg %>% 
  mutate(porc_acum = round(max_acum / sum(max_acum,na.rm = TRUE),2),
         uno = as.factor(rep(1,3))) %>% 
  ggplot(aes(y = porc_acum, fill = uno, x = lab)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = "cadetblue") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Porcentaje de dosis suministradas",
       x = "Laboratorio")


###Porcentaje de vacunados con segunda dosis
tabg2 <-
  labs %>%
  group_by(lab) %>% 
  summarise(acum_seg  = sum(segDiario,na.rm = TRUE)) 



tabg2 %>% 
  mutate(porc_acum = round(acum_seg / sum(acum_seg,na.rm = TRUE),4),
         uno = as.factor(rep(1,3))) %>% 
  ggplot(aes(y = porc_acum, fill = uno, x = lab)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = "darkgreen") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  labs(y = "Proporción de segundas dosis suministradas",
       x = "Laboratorio")









