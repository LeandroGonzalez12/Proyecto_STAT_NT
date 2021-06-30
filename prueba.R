rm(list = ls())
library(tidyverse)
library(ggpmisc)

###cargo los datos

##datos sobre vacunacion por departamentos, tipo de vacuna, etc; actualizado hasta el 23/6
uruguay <- readr::read_csv("data/Uruguay.csv")

#Investigando las variables
# uruguay %>% select(5,8,22:27) %>% head(20) %>% View()
# uruguay %>% select(5,8,22:27) %>% tail(20) %>% View()

### daily_agend_diff = [(daily_agenda_ini - daily_agenda) / daily_agenda_ini] * 100
### dailypogress = (daily_vaccinated / daily_agenda) * 100
### dailyagenda = daily_agenda_first + daily_agenda_second 


## al principio no, serán solo first y no especifican?
###
# uruguay %>% select(people_coronavac,total_coronavac,daily_coronavac,fully_coronavac,
#                   people_mo,total_mo,daily_mo,fully_mo) %>% head(20) %>% View()

##people_coronavac + fully_coronavac = daily_coronavac
## people_mo + fully_mo = total_mo
# uruguay %>% select(people_coronavac,total_coronavac,daily_coronavac,fully_coronavac,
#                   people_mo,total_mo,daily_mo,fully_mo) %>% 
#   tail(20) %>% 
#   View()

## se observa que lo anterior se cumple para las demás vacunas y departamentos
# uruguay %>% select(people_pfizer,total_pfizer,daily_pfizer,fully_pfizer,
#                   people_ma,total_ma,daily_ma,fully_ma) %>% 
# tail(20) %>% View()

## interpreto que cuando refiere a laboratorio de vacunas: 
### fully= cantidad de segundas dosis en el día con esa vacuna
### people= cantidad de primeras dosis en el día con esa vacuna

##POR OTRA PARTE
## interpreto que cuando refiere a departamentos: 
### fully= cantidad de segundas dosis acumuladas (totales) en el departamento
### people= cantidad de primeras dosis acumuladas (totales) en el departamento
##

## de acuerdo a lo anterior renombro en la base uruguay las variables referidas a laboratorios
uruguay <- uruguay %>% rename(diario_coronavac = daily_coronavac,acum_coronavac = total_coronavac,primDiario_coronavac = people_coronavac,
                              segDiario_coronavac  = fully_coronavac,diario_pfizer = daily_pfizer,acum_pfizer = total_pfizer,primDiario_pfizer = people_pfizer,
                              segDiario_pfizer = fully_pfizer,diario_astrazeneca = daily_astrazeneca,acum_astrazeneca = total_astrazeneca,
                              primDiario_astrazeneca = people_astrazeneca,segDiario_astrazeneca = fully_astrazeneca)

prueba <- uruguay %>% select(-c(1,3:29),-contains("res")) %>% pivot_longer(cols = c(starts_with("total"),starts_with("fully"),
                                                                                    starts_with("daily"),starts_with("people")),names_to = "departamento",values_to = c("total")) %>%  
  separate(col = departamento,into = c("arranca","depar"))%>% pivot_wider(names_from = arranca,values_from = total)%>% 
  mutate(depar = recode(depar, 'ar'='Artigas', 'ca'='Canelones', 'cl'='Cerro Largo', 'co'='Colonia', 
                        'du'='Durazno','fd'='Florida', 'fs'='Flores', 'la'='Lavalleja', 'ma'='Maldonado','mo'='Montevideo',
                        'pa'='Paysandú', 'rn'='Rio Negro','ro'='Rocha','rv'='Rivera','sa'='Salto','sj'='San José','so'='Soriano',
                        'ta'='Tacuarembó', 'tt'='Treinta y Tres')) %>% mutate(depar = as.factor(depar))
class(prueba$depar)

##Crear data.frame con los tipo de vacuna 
total_lab <- select(uruguay,date,diario_coronavac,acum_coronavac,primDiario_coronavac,segDiario_coronavac,diario_pfizer,         
                    acum_pfizer,primDiario_pfizer,segDiario_pfizer,diario_astrazeneca,acum_astrazeneca,primDiario_astrazeneca,segDiario_astrazeneca,
                    people_vaccinated,people_fully_vaccinated,daily_vaccinated,total_vaccinations,daily_agenda,daily_pogress)

##Actualizado, muestra datos de muerte segun edad, pero no por departamento hasta el 22/6
muertes_edad <- read_csv("data/Deaths.csv")

##elijo las primeras 17 porque las otras son tramos distintos y es para entreverar,
# estos tramos son excluyentes y cubren de 0 a 115

muertes_edad <-muertes_edad %>% select(1:17) %>% pivot_longer(cols = c(starts_with("total"),starts_with("daily")),
                                                              names_to = "tramo_etario",values_to = "numeros")
muertes_edad <-muertes_edad %>% separate(col = tramo_etario, into = c("arranca","tramo"),sep = 5)
muertes_edad <-muertes_edad %>% pivot_wider(names_from = arranca,values_from = numeros)
muertes_edad <-muertes_edad %>% mutate(tramo = 
                                         recode(tramo, '_0_17'  = '[0,17]' , '_18_24'  = '[18,24]' ,'_25_34' = '[25,34]', '_35_44'  = '[35,44]' ,
                                                '_45_54' = '[45,54]', '_55_64'  = '[55,64]' ,'_65_74' = '[65,74]', '_75_115' = '[75,115]')) %>% 
  mutate(tramo_etario = as.factor(tramo)) %>% select(-tramo)

##Porcentaje de vacunados con al menos una dosis actualmente
(uruguay$people_vaccinated[117] / 3543025) * 100

##Porcentaje de vacunados con ambas dosis
(uruguay$people_fully_vaccinated[117] / 3543025) * 100

###cargo datos del GUIAD
por_depto <- read_csv("data/estadisticasUY_porDepto_detalle.csv")
nacional  <- read_csv("data/estadisticasUY.csv")

prueba %>% filter(date == as.Date("2021-06-23")) %>% select(depar,total,fully,people,daily)
prueba %>% filter(depar == "Montevideo") %>% ggplot(aes(x = date, y = daily)) +geom_line() +theme(aspect.ratio = 1) +
  scale_x_date(date_breaks = "1 day",labels = NULL)

### separo la base en 2

labs <- total_lab %>% select(date,c(contains("pfizer"),contains("coronavac"),contains("astrazeneca")))
total <- total_lab %>% select(-c(contains("pfizer"),contains("coronavac"),contains("astrazeneca")))

rm(total_lab)
### le hago pivot_longer a las vacunas
labs <-labs %>%  pivot_longer(cols = c(ends_with("astrazeneca"),ends_with("pfizer"),ends_with("coronavac")),names_to = "laboratorio",
                              values_to = "numeros")
labs <- labs %>% separate(col = laboratorio, into = c("arranca","lab"))
labs <-labs %>% pivot_wider(names_from = arranca,values_from = numeros)


#--------------------------------------------------------PREGUNTA 1--------------------------------------------------------------

# ¿Como ha evolucionado la cantidad de dosis suministradas diariamente, comparando por laboratorio?

labs %>% ggplot(aes(x = date, y = diario, colour = lab)) +geom_line() +scale_colour_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "10 days") + theme(aspect.ratio = 1, axis.text.x = element_text(size = 7, angle = 300, hjust = 0)) +
  labs(x = "Fecha",y = "Cantidad de dosis en el dia",colour = "Laboratorio")

# ¿Y si solo nos centramos en la segunda dosis?
labs %>% ggplot(aes(x = date, y = segDiario, colour = lab)) + geom_line() + scale_colour_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "10 days") + theme(aspect.ratio = 1, axis.text.x = element_text(size = 7, angle = 300, hjust = 0)) +
  labs(x = "Fecha",y = "Cantidad de segundas dosis en el dia",colour = "Laboratorio")

# ¿Cual es la proporcion por laboratorio del total de dosis suministradas hasta el 23/6
tabg <- labs %>% group_by(lab) %>% summarise(max_acum  = max(acum)) 


tabg %>% mutate(porc_acum = round(max_acum / sum(max_acum,na.rm = TRUE),2),uno = as.factor(rep(1,3))) %>% 
  ggplot(aes(y = porc_acum, fill = uno, x = lab)) + geom_bar(stat = "identity") + scale_fill_manual(values = "cadetblue") +
  theme(legend.position = "none") + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Porcentaje de dosis suministradas",x = "Laboratorio")

tabg2 <- labs %>% group_by(lab) %>% summarise(acum_seg  = sum(segDiario,na.rm = TRUE)) 

tabg2 %>% mutate(porc_acum = round(acum_seg / sum(acum_seg,na.rm = TRUE),4),uno = as.factor(rep(1,3))) %>% 
  ggplot(aes(y = porc_acum, fill = uno, x = lab)) + geom_bar(stat = "identity") + scale_fill_manual(values = "darkgreen") +
  theme(legend.position = "none") + scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  labs(y = "Proporción de segundas dosis suministradas",x = "Laboratorio")

#--------------------------------------------------------PREGUNTA 2---------------------------------------------------------------

# ¿Como fue la evolucion de casos positivos dependiendo el departamento?

por_depto$fecha = as.Date(por_depto$fecha, format = "%d/%m/%Y")
por_depto <- por_depto %>% mutate(departamento = recode(departamento, 'Artigas(UY-AR)'='Artigas', 'Canelones(UY-CA)'='Canelones', 
             'Cerro Largo(UY-CL)'='Cerro Largo', 'Colonia(UY-CO)'='Colonia', 'Durazno(UY-DU)'='Durazno',
             'Flores(UY-FS)'='Flores','Florida(UY-FD)'='Florida','Lavalleja(UY-LA)'='Lavalleja', 'Maldonado(UY-MA)'='Maldonado',
             'Montevideo(UY-MO)'='Montevideo','Paysand?(UY-PA)'='Paysandu', 'R?o Negro(UY-RN)'='Rio Negro',
             'Rivera(UY-RV)'='Rivera','Rocha(UY-RO)'='Rocha','Salto(UY-SA)'='Salto','San Jos?(UY-SJ)'='San Jose',
             'Soriano(UY-SO)'='Soriano','Tacuaremb?(UY-TA)'='Tacuarembo', 'Treinta y Tres(UY-TT)'='Treinta y Tres'))
por_depto %>% ggplot(aes(x = fecha, y = cantCasosNuevosCALC, colour = departamento)) + geom_line(size = 0.05) + scale_x_date(date_breaks = "90 days") + 
  theme(aspect.ratio = 1, axis.text.x = element_text(size = 7, angle = 300, hjust = 0)) +
  labs(x = "Fecha",y = "Cantidad de casos positivos",colour = "Departamento") + facet_wrap(~departamento) + scale_y_log10()

# ¿Y si vemos cuantas personas han fallecido por departamento?
por_depto %>% group_by(departamento) %>% summarise(cantidad_fallecidos = sum(cantFallecidos, na.rm = TRUE)) %>%
  ggplot(aes(y=reorder(departamento,-cantidad_fallecidos, na.rm=TRUE), x = cantidad_fallecidos, fill = departamento)) + 
  geom_col() + theme(aspect.ratio = 1) 



#### Pregunta 7)
## Hasta el 23 de junio, ¿todas las personas con primera dosis antes del 25 mayo
## recibieron la segunda dosis? Si no es así, ¿Cuántas personas no se dieron 
## la segunda dosis?

uruguay %>% 
  select(date, vaccine, total_vaccinations, people_vaccinated,
         people_fully_vaccinated, daily_vaccinated) %>% 
  filter(date <= "2021-05-26") %>% 
  ggplot(aes(x = date, y = daily_vaccinated)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1/3,
        axis.text.x = element_text(size = 7, hjust = 0)) +
  labs(x="Fecha", y="Cantidad de dosis en el día")



completas_hasta_mayo <- 
  prueba %>% 
  select(date, depar, total, people) %>%
  filter(date == "2021-05-26") %>% 
  group_by(depar) 


completas_hasta_junio <-
  prueba %>% 
  select(date, depar, total, fully) %>% 
  filter(date == "2021-06-23") %>% 
  group_by(depar) 

completas <- merge(x=completas_hasta_mayo, y=completas_hasta_junio, all=TRUE) %>% 
  pivot_longer(cols = c("people","fully"), names_to = "Numero_dosis", 
               values_to ="Cant_dosis") %>% 
  group_by(depar, Numero_dosis) %>% 
  summarise(Cant_dosis = sum(Cant_dosis, na.rm=TRUE))

completas%>% 
  ggplot(aes(x=Numero_dosis, y=Cant_dosis, fill=Numero_dosis))+
  geom_col(width=1)+ theme(aspect.ratio = 1)+
  scale_fill_brewer(palette = "Dark2", labels=c('fully'='2 dosis',
                                                'people'='1 dosis'))+
  scale_x_discrete(labels=c('fully'='2 dosis',
                            'people'='1 dosis'))+
  labs(x="Dosis por persona", y="Cantidad de dosis", fill="Dosis por persona")+
  facet_wrap(~depar)

#Arreglar que quede en porcentajes y que se vea mas claro. 


#Hago a nivel pais:
completas_pais_hasta_mayo<-
  uruguay %>% 
  select(date, total_vaccinations, people_vaccinated) %>%
  filter(date == "2021-05-26") 

completas_pais_hasta_junio<-
  uruguay %>% 
  select(date, total_vaccinations, people_fully_vaccinated) %>%
  filter(date == "2021-06-23") 

completas_pais<-merge(x=completas_pais_hasta_mayo, y=completas_pais_hasta_junio, all=TRUE) %>% 
  pivot_longer(cols = c("people_vaccinated","people_fully_vaccinated"),
               names_to = "Numero_dosis", 
               values_to ="Cant_dosis") %>% 
  group_by(Numero_dosis) %>% 
  summarise(Cant_dosis = sum(Cant_dosis, na.rm=TRUE))

completas_pais %>% 
  ggplot(aes(x=Numero_dosis, y=Cant_dosis, fill=Numero_dosis))+
  geom_col()+ theme(aspect.ratio = 2)+
  scale_fill_brewer(palette = "Dark2", labels=c('people_fully_vaccinated'='2 dosis',
                                                'people_vaccinated'='1 dosis'))+
  scale_x_discrete(labels=c('people_fully_vaccinated'='2 dosis',
                            'people_vaccinated'='1 dosis'))+
  labs(x="Dosis por persona", y="Cantidad de dosis", fill="Dosis por persona")

#Personas con una dosis pero que no se dieron la segunda a nivel país:
# 1688019-1458403=229616 personas.
#Hacer el nivel pais con porcentajes.

# probar con: (..count..)/sum(..count..) y https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/



completas_pais %>% 
  ggplot(aes(x=Numero_dosis,y=Cant_dosis, fill=Numero_dosis))+
  geom_bar(position = "dodge", stat = "identity")+ 
  theme(aspect.ratio = 2)+
  scale_fill_brewer(palette = "Dark2", labels=c('people_fully_vaccinated'='2 dosis',
                                                'people_vaccinated'='1 dosis'))+
  scale_x_discrete(labels=c('people_fully_vaccinated'='2 dosis',
                            'people_vaccinated'='1 dosis'))+
  labs(x="Dosis por persona", y="Cantidad de dosis", fill="Dosis por persona")



aindamais<-merge(x=completas_pais_hasta_mayo, y=completas_pais_hasta_junio, all=TRUE) %>% 
  mutate(suma=people_vaccinated+people_fully_vaccinated, na.rm=TRUE)
  select(people_vaccinated, people_fully_vaccinated)
