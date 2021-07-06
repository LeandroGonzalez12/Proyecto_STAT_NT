library(shiny)
library(tidyverse)
library(lubridate)

muertes_diarias <- muertes_edad %>% group_by(date) %>% 
  summarise(muertes_totales_diarias=sum(daily))

personas_totalmente_vacunadas <- uruguay %>% select(date, people_fully_vaccinated)
muertes_vacunados <- merge(muertes_diarias, personas_totalmente_vacunadas)

ui <- fluidPage(
    navlistPanel("Menu",
      tabPanel("Pregunta 1",
               h3("¿Cómo ha evolucionado la cantidad de dosis suministradas diariamente, comparando por laboratorio?", align="center", 
                    plotOutput("grafico1")),
               h4("¿Cuál es la proporción por laboratorio del total de dosis suministradas hasta el 23/06?", align="center", plotOutput("grafico2"))
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
}

# Run the application 
shinyApp(ui = ui, server = server)
