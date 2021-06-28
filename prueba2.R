#¿SE PODRÁ DECIR QUE LA CANTIDAD DE MUERTES DIARIAS A DISMINUIDO
# A MEDIDA QUE EL PLAN DE VACUNACIÓN AUMENTA?


muertes_diarias<-muertes_edad %>% group_by(date) %>% summarise(muertes_totales_diarias=sum(daily))
personas_totalmente_vacunadas<-uruguay %>% select(date, people_fully_vaccinated)

comparacion<-merge(muertes_diarias, personas_totalmente_vacunadas)

ggplot(data=comparacion)+geom_line(aes(x=people_fully_vaccinated, y=muertes_totales_diarias))

#SI COMPARAMOS LOS PICOS DE MUERTES CON LOS PICOS DE AGENDADOS, 
#¿PODRIAMOS VER ALGUNA RELACIÒN ENTRE AMBOS?

ggplot(comparacion, aes(x=date,y=muertes_totales_diarias))+geom_line()
ggplot(Schedule, aes(date, scheduled))+geom_line()
#sonaba interesante pero no se ve nada interesante, a demas, no se como juntar ambos gráficos para compararlos

#Se podrìa decir que en los departamentos más poblados, como Montevideo y Canelones 
#el porcentaje de vacunados es mayor?

Regions %>% select(Region, Population, Vaccinated) %>% mutate(proporcion=Vaccinated/sum(Population)) %>% 
  arrange(desc(proporcion)) %>% 
  ggplot(aes(reorder(Region, proporcion), proporcion))+geom_bar(stat='identity')+coord_flip()

#podemos ver que los 3 departamantetos mas poblados, son también, los 3 departamentos con mayor proporcion de vacunados

#¿En los departamentos con mayor porcentaje de vacunados 
#se cumple también que tiene el menor porcentaje de muertes?
#no hay datos de muertes sesgados por departamento asi que descartada

