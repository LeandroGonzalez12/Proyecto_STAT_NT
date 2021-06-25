rm(list = ls())
library(tidyverse)

###cargo los datos

##datos sobre vacunacion por departamentos, tipo de vacuna, etc; actualizado
#hasta el 23/6
uruguay      <- readr::read_csv("data/Uruguay.csv")

##actualizado, muestra deptos en filas, no desagrega segun vacuna
#hasta el 23/6
vacuna_dptos <- read_csv("data/Subnational.csv")

##actualizado, muestra datos de muerte segun edad, pero no por departamento
#hasta el 22/6
muertes_edad <- read_csv("data/Deaths.csv")

prueba <-
  uruguay %>% select(-c(3:29),-contains("res")) %>% 
  pivot_longer(cols = c(starts_with("total"),starts_with("fully"),starts_with("daily"),starts_with("people")),
               names_to = "departamento",
               values_to = c("total"))

prueba <-
  prueba %>% 
  separate(col = departamento,into = c("arranca","depar"))

prueba <-
  prueba %>% 
  pivot_wider(names_from = arranca,
              values_from = total)

str(prueba)
# prueba <-
#   uruguay %>% select(-c(1:29),-contains("res")) %>% 
#   pivot_longer(cols = c(starts_with("total")),
#                names_to = "departamento",
#                values_to = c("total"))

uruguay %>% 
  select(total_res_ar,total_ar) %>% 
  head(10)


#hasta el 28/5
edad         <- read_csv("data/Age.csv") 

#datos agregados por departamentos sobre vacunados, poblacion, cobertura
#dosis, proyecciones, etc;no dice fecha
region       <- read_csv("data/Regions.csv")

##hasta el 25/5, datos sobre vacunacion segun ocupacion y estado de salud
segmento     <- read_csv("data/Segments.csv")

##datos actualizados,agenda para vacunarse
programacion <- read_csv("data/Schedule.csv")



###
control <- left_join(programacion[,c("date","scheduled","pending")],
                     uruguay     [,c("date","people_vaccinated",
                                     "people_fully_vaccinated")]) %>% 
  mutate(aVacunar=people_vaccinated+scheduled+pending)

##porcentaje de vacunados con al menos una dosis actualmente
(control$people_vaccinated[54]       / 3543025) * 100

##porcentaje de vacunados con ambas dosis
(control$people_fully_vaccinated[54] / 3543025) * 100