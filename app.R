library(lubridate)
library(shiny)
library(tidyverse)
uruguay <- read_csv("data/Uruguay.csv") #Cargamos datos de vacunacion.

# 1 - Dado que los nombres de las variables se encuentran en ingles.
# Se renombran para facilitar la interpretacion.
uruguay <- uruguay %>% rename(diario_coronavac = daily_coronavac,
                              acum_coronavac = total_coronavac,primDiario_coronavac = people_coronavac,
                              segDiario_coronavac  = fully_coronavac,diario_pfizer = daily_pfizer,
                              acum_pfizer = total_pfizer,primDiario_pfizer = people_pfizer,
                              segDiario_pfizer = fully_pfizer,diario_astrazeneca = daily_astrazeneca,
                              acum_astrazeneca = total_astrazeneca,primDiario_astrazeneca = people_astrazeneca,
                              segDiario_astrazeneca = fully_astrazeneca)

# 2 - De "uruguay" anterior, nos interesan en particular ciertas columnas. 
# Tambien se reestructuran datos y cambian nombre de los departamentos.
vacunacion_uru <- uruguay %>% select(-c(1,3:29),-contains("res")) %>% 
  pivot_longer(cols = c(starts_with("total"),starts_with("fully"),starts_with("daily"),
                        starts_with("people")),names_to = "departamento",values_to = c("total")) %>% 
  separate(col = departamento,into = c("arranca","depar")) %>% 
  pivot_wider(names_from = arranca,values_from = total) %>% 
  mutate(depar = recode(depar, 'ar'='Artigas', 
                        'ca'='Canelones', 'cl'='Cerro Largo', 'co'='Colonia','du'='Durazno',
                        'fd'='Florida', 'fs'='Flores', 'la'='Lavalleja', 'ma'='Maldonado',
                        'mo'='Montevideo','pa'='Paysandú', 'rn'='Rio Negro','ro'='Rocha',
                        'rv'='Rivera','sa'='Salto','sj'='San José','so'='Soriano',
                        'ta'='Tacuarembó', 'tt'='Treinta y Tres')) %>% 
  mutate(depar = as.factor(depar))

# 3 - Se crea otro data.frame (total_lab) con los tipos de vacuna 
total_lab <- select(uruguay,date,diario_coronavac,acum_coronavac,primDiario_coronavac,
                    segDiario_coronavac,diario_pfizer,acum_pfizer,primDiario_pfizer,segDiario_pfizer,
                    diario_astrazeneca,acum_astrazeneca,primDiario_astrazeneca,segDiario_astrazeneca,
                    people_vaccinated,people_fully_vaccinated,
                    daily_vaccinated,total_vaccinations,daily_agenda,daily_pogress)

# 
# 4 - El conjunto de datos Deaths muestra los datos de fallecimiento segun edad,
#pero no por departamento, actualizado hasta el dia 22/6
muertes_edad <- read_csv("data/Deaths.csv")

# 5 - De dichos datos, se eligen las primeras 17 columnas,
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

# 6 - Se cargan datos de vacunacion por departamento
departamentos <- read_csv("data/Regions.csv")

# 7 - Se cargan los datos del GUIAD
por_depto <- read_csv("data/estadisticasUY_porDepto_detalle.csv")
nacional  <- read_csv("data/estadisticasUY.csv")

# 8 - Separamos en 2 bases de datos

labs <- total_lab %>% select(date,c(contains("pfizer"),contains("coronavac"),
                                    contains("astrazeneca")))
total <- total_lab %>% select(-c(contains("pfizer"),contains("coronavac"),
                                 contains("astrazeneca")))

# 9 - Reestructuramos los datos de laboratorio.
labs <-labs %>%  pivot_longer(cols = c(ends_with("astrazeneca"),ends_with("pfizer"),
                                       ends_with("coronavac")),names_to = "laboratorio",values_to = "numeros")
labs <- labs %>% separate(col = laboratorio, into = c("arranca","lab"))
labs <-labs %>% pivot_wider(names_from = arranca,values_from = numeros)

por_depto$fecha = as.Date(por_depto$fecha, format = "%d/%m/%Y")
por_depto <- por_depto %>% mutate(departamento = recode(departamento, 
                                                        'Artigas(UY-AR)'='Artigas', 'Canelones(UY-CA)'='Canelones',
                                                        'Cerro Largo(UY-CL)'='Cerro Largo', 'Colonia(UY-CO)'='Colonia',
                                                        'Durazno(UY-DU)'='Durazno','Flores(UY-FS)'='Flores','Florida(UY-FD)'='Florida',
                                                        'Lavalleja(UY-LA)'='Lavalleja', 'Maldonado(UY-MA)'='Maldonado',
                                                        'Montevideo(UY-MO)'='Montevideo','Paysandú(UY-PA)'='Paysandu', 
                                                        'Río Negro(UY-RN)'='Rio Negro','Rivera(UY-RV)'='Rivera','Rocha(UY-RO)'='Rocha',
                                                        'Salto(UY-SA)'='Salto','San José(UY-SJ)'='San José','Soriano(UY-SO)'='Soriano',
                                                        'Tacuarembó(UY-TA)'='Tacuarembo', 'Treinta y Tres(UY-TT)'='Treinta y Tres'))


completas_pais_hasta_mayo<- uruguay %>% 
  select(date, total_vaccinations, people_vaccinated) %>%
  filter(date == "2021-05-26") 

completas_pais_hasta_junio<- uruguay %>% 
  select(date, total_vaccinations, people_fully_vaccinated) %>%
  filter(date == "2021-06-23") 

completas_pais<-merge(x=completas_pais_hasta_mayo, y=completas_pais_hasta_junio, all=TRUE) %>% 
  pivot_longer(cols = c("people_vaccinated","people_fully_vaccinated"),
               names_to = "Numero_dosis", 
               values_to ="Cant_dosis") %>% 
  group_by(Numero_dosis) %>% 
  summarise(Cant_dosis = sum(Cant_dosis, na.rm=TRUE)) %>% 
  group_by(Numero_dosis, Cant_dosis) %>% 
  summarise(porcentaje=round(Cant_dosis/1688019*100, 2))

muertes_diarias <- muertes_edad %>% group_by(date) %>% 
  summarise(muertes_totales_diarias=sum(daily))

personas_totalmente_vacunadas <- uruguay %>% select(date, people_fully_vaccinated)
muertes_vacunados <- merge(muertes_diarias, personas_totalmente_vacunadas)

departamentos<-departamentos %>% rename(Fully_Vaccinated='Fully Vaccinated')


ui <- fluidPage(
    navlistPanel("Menu",
            tabPanel("Pregunta 1",
                          h3("¿Cómo ha evolucionado la cantidad de dosis suministradas diariamente, comparando por laboratorio?", align="center", 
                             plotOutput("grafico1"),
                             dateInput(inputId = "fecha1", label = "Fecha inicial",
                                       value = "2021-02-27",min = "2021-02-27",
                                       max = "2021-06-23"
                             ),
                             dateInput(inputId = "fecha2", label = "Fecha final",
                                       value = "2021-06-23",min = "2021-02-27",
                                       max = "2021-06-23"
                             ),
                             checkboxGroupInput(inputId = "labs",
                                                label   = "Laboratorio",
                                                choices = c("astrazeneca","pfizer",
                                                            "coronavac"),
                                                selected = "coronavac")
                          ),
                          h4("¿Cuál es el total de dosis suministradas por laboratorio en el rango seleccionado?",
                             align="center", plotOutput("grafico2"))
                 ),
      tabPanel("Pregunta 2",
              selectInput("depto", "Elegir departamento",
                           c("Artigas", "Canelones", "Cerro Largo", "Colonia",
                             "Durazno", "Florida", "Flores", "Lavalleja",
                             "Maldonado", "Montevideo", "Paysandú", "Rio Negro",
                             "Rocha", "Rivera", "Salto", "San José", "Soriano",
                             "Tacuarembó", "Treinta y Tres")),
              sliderInput("fechitas",
                           "Fecha:",
                           min = as.Date("2020-04-29","%Y-%m-%d"),
                           max = as.Date("2021-06-25","%Y-%m-%d"),
                           value=as.Date("2021-06-25"),
                           timeFormat="%Y-%m-%d"),
               h3("¿Cómo fue la evolución de casos positivos dependiendo del departamento?", align="center", 
                  plotOutput("grafico3")),
               h4("¿Y si vemos cuántas personas han fallecido por departamento?", align="center", plotOutput("scat4"))
      ),
      tabPanel("Pregunta 3",
               h3("¿Todas las personas con primera dosis antes del 26/05 recibieron la segunda dosis?", align="center", plotOutput("scat")),
               h4("¿Cuántas personas no se dieron la segunda dosis?", align="center", plotOutput("scat5"))
      ),
      tabPanel("Pregunta 4",
               dateRangeInput("dates",
                              "Select Dates",
                              start = min(muertes_vacunados$date), 
                              end = max(muertes_vacunados$date)),
               h3("This is the third panel", plotOutput("scat6"))
      ),
      tabPanel("Pregunta 5",
               h3("¿Se podría decir que en los departamentos más poblados, como Montevideo y Canelones el porcentaje de vacunados es mayor?",
                   align="center")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    gra3<-reactive(
      por_depto %>% ggplot(aes(x = input$fechitas, y = cantCasosNuevosCALC,
                               colour = input$depto)) + 
        geom_line(size = 0.05) + scale_x_date(date_breaks = "90 days") + 
        theme(aspect.ratio = 1, axis.text.x = element_text(size = 7, angle = 300, hjust = 0),legend.position = 'none') +
        labs(x = "Fecha",y = "Cantidad de casos positivos",colour = "Departamento") + 
        facet_wrap(~ input$depto) + scale_y_log10()
    )
    output$grafico3<-renderPlot({ gra3() })
    
    gra6<-reactive(
      ggplot(data=muertes_vacunados,aes(x=.data[[input$dates]], y=muertes_totales_diarias))+
        geom_line() +
        labs(x='Fecha', y='Cantidad de fallecidos por día')+ 
        stat_peaks(span = NULL, color = "red") +
        stat_peaks(span = NULL, geom = "text", vjust = 0.6,hjust=0.8, color = "red", 
                   aes(label = paste(stat(y.label), "fallecidos,\n el día", stat(x.label))))+
        scale_x_date(date_breaks = "15 days") + 
        theme(axis.text.x = element_text(size = 9, angle = 300, hjust = 0))
    )
    output$scat6<-renderPlot({ gra6() })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    grafico1 <- reactive({
      labs %>% filter(date %in% seq.Date(from = input$fecha1,
                                         to = input$fecha2,
                                         by = "day") &
                        lab %in% input$labs) %>%  
        ggplot(aes(x = date, y = diario, colour = lab)) +geom_line() + 
        scale_colour_brewer(palette = "Dark2") +scale_x_date(date_breaks = "10 days") + 
        theme(aspect.ratio = 1, axis.text.x = element_text(size = 7, angle = 300, hjust = 0)) +
        labs(x = "Fecha",y = "Cantidad de dosis en el dia",colour = "Laboratorio")
    })
    output$grafico1 <- renderPlot(grafico1())
    grafico2 <- reactive({
      labs %>% filter(lab %in% input$labs & date %in% seq.Date(from = input$fecha1,
                                                               to = input$fecha2,
                                                               by = "day")) %>% 
        group_by(lab) %>% summarise(max_acum  = max(acum)) %>% 
        mutate(uno = as.factor(rep(1)), max_acum = max_acum/1000) %>%
        ggplot(aes(y = max_acum, fill = uno, x = lab)) + 
        geom_bar(stat = "identity") + scale_fill_manual(values = "cadetblue") +
        theme(legend.position = "none") + 
        labs(y = "Cantidad de dosis suministradas (en miles de dosis)",
             x = "Laboratorio")
    })
    output$grafico2 <- renderPlot(grafico2())
}

# Run the application 
shinyApp(ui = ui, server = server)
