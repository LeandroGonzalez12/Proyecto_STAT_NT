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

#cargamos los datos desde script auxiliar
source("datos.R", encoding = 'UTF-8')


ui <- dashboardPage(skin = "green",
                    dashboardHeader(titleWidth  = 200, title = "Menú"),
                    dashboardSidebar( width = 250,
                                      sidebarMenu(
                                        menuItem("Vacunación", tabName = "mapa"),
                                        menuItem("Plan de vacunación", tabName = "p1", icon = icon("bar-chart-o")),
                                        menuItem("Primera y segunda dosis", tabName = "p2", icon = icon("bar-chart-o")),
                                        menuItem("Evolución de la pandemia", tabName = "p3", icon = icon("bar-chart-o")),
                                        menuItem("Fallecimientos", tabName = "p4", icon = icon("bar-chart-o"))
                                      )
                    ),
                    dashboardBody(
                      tags$head(tags$style(HTML(".main-sidebar { font-size: 17px; }"))),
                      tabItems(
                        tabItem(tabName = "mapa",
                                h2('Vacunación en Uruguay', align='center'), plotlyOutput('mapa1', width = "100%",height='1000px')),
                        tabItem(tabName = "p1",
                                h4("¿Cómo ha evolucionado la cantidad de dosis suministradas diariamente, comparando por laboratorio?", align="center",
                                   plotOutput("grafico1"), 
                                   dateInput(inputId = "fecha1", label = "Fecha inicial",value = "2021-02-27",min = "2021-02-27",
                                             max = "2021-06-23"),
                                   dateInput(inputId = "fecha2", label = "Fecha final",value = "2021-06-23",min = "2021-02-27",
                                             max = "2021-06-23"),
                                   checkboxGroupInput(inputId = "labs",
                                                      label = "Laboratorio",choices = c("astrazeneca","pfizer","coronavac"),selected = "coronavac")
                                ),
                                h4("¿Cuál es el total de dosis suministradas por laboratorio en el rango seleccionado?",align="center", plotOutput("grafico1_1")),
                        ),
                        tabItem(tabName = "p2",
                                h4("¿Todas las personas con primera dosis antes del 26/05 recibieron la segunda dosis?",
                                   align="center", plotOutput("grafico3")),
                                dateInput(inputId = "fechaza1", label = "Fecha inicial",value = "2021-02-27",min = "2021-02-27",
                                          max = "2021-05-26"),
                                dateInput(inputId = "fechaza2", label = "Fecha final",value = "2021-06-23",min = "2021-02-27",
                                          max = "2021-06-23"),
                                checkboxGroupInput(inputId = "dosis",
                                                   label = "Número de dosis",choices = c("primera","segunda"),selected = "segunda")
                        ),
                        tabItem(tabName = "p3",
                                h4("¿Cómo fue la evolución de casos positivos dependiendo del departamento?", align="center", plotOutput("grafico2")),
                                selectInput(inputId =  "depto", label = "Elegir departamento",choices = c("Artigas", "Canelones", "Cerro Largo", 
                                                                                                          "Colonia","Durazno", "Florida", "Flores",
                                                                                                          "Lavalleja","Maldonado", "Montevideo", "Paysandú", "Río Negro",
                                                                                                          "Rocha", "Rivera", "Salto", "San José",
                                                                                                          "Soriano","Tacuarembó", "Treinta y Tres"),selected = "Montevideo"),
                                dateInput(inputId = "fechita1", label = "Fecha inicial",value = "2020-04-29",min = "2020-04-29",max = "2021-06-25"),
                                dateInput(inputId = "fechita2", label = "Fecha final",value = "2021-06-25",min = "2020-04-29",max = "2021-06-25")
                        ),
                        tabItem(tabName = "p4",
                                h4("¿Se podría decir que la cantidad de fallecidos diarios ha disminuido a medida que avanza el plan de vacunación?", 
                                   plotOutput("grafico4")),
                                sliderInput("vacunitas","Personas con dos dosis:",min = 0,max = 1451040,value = c(0,1451040))
                        )
                      )
                    )
)


server<-function(input,output){
  
  output$mapa1<-renderPlotly({
    mapita <- (ggplot(data=datos_mapa,aes(x=long, y= lat)) + geom_polygon(aes(group=group, 
                                                                              fill=Departamento,text = paste('Personas vacunadas:',Vaccinated,'\n','Personas vacunadas con ambas dosis:', Fully_Vaccinated))) +
                 scale_fill_viridis_d()+theme_minimal() + geom_text(data = lab.data,aes(label = depa),
                                                                    size = 3, hjust = 0.5, color='black',fontface = "bold") + 
                 coord_equal() + theme( legend.position='none',
                                        axis.text = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank()))
    ggplotly(mapita) %>% 
      style(hoverinfo = "none", traces = 20)
    
  })
  
  grafico1 <- reactive(
    labs %>% filter(date %in% seq.Date(from = input$fecha1,to = input$fecha2,by = "day") & lab %in% input$labs) %>%  
      ggplot(aes(x = date, y = diario, colour = lab)) +geom_line() + scale_colour_brewer(palette = "Dark2") +scale_x_date(date_breaks = "10 days")+ 
      theme(axis.text.x = element_text(size = 7, angle = 300, hjust = 0)) + labs(x = "Fecha",y = "Cantidad de dosis en el dia",colour = "Laboratorio")
  )
  output$grafico1 <- renderPlot(grafico1())
  
  grafico1_1 <- reactive(
    labs %>% filter(lab %in% input$labs & date %in% seq.Date(from = input$fecha1,to = input$fecha2,by = "day")) %>% 
      group_by(lab) %>% summarise(max_acum  = max(acum)) %>% mutate(uno = as.factor(rep(1)), max_acum = max_acum/1000) %>%
      ggplot(aes(y = max_acum , x = lab)) + geom_bar(stat = "identity")+labs(y = "Cantidad de dosis suministradas (en miles de dosis)",
                                                                             x = "Laboratorio")
  )
  output$grafico1_1 <- renderPlot(grafico1_1())
  
  grafico2<-reactive(
    por_depto %>% filter(fecha %in% seq.Date(from = input$fechita1,to = input$fechita2, by = "day") & departamento %in% input$depto) %>%
      ggplot(aes(x =fecha, y = cantCasosNuevosCALC)) + geom_line(size = 0.05) + scale_x_date(date_breaks = "90 days") + 
      theme(axis.text.x = element_text(size = 8,hjust = 0)) + labs(x = "Fecha",y = "Cantidad de casos positivos") + 
      facet_wrap(~ input$depto)
  )
  output$grafico2<-renderPlot({ grafico2() })
  
  grafico3 <- reactive(
    completitas %>% filter(fecha %in% seq.Date(from = input$fechaza1,to = input$fechaza2, by="day") & dosis %in% input$dosis) %>%
      ggplot(aes(x = fecha, y = cantidad, colour = dosis)) + 
      geom_line(size=1) + 
      scale_colour_brewer(palette = "Dark2") + scale_x_date(date_breaks = "20 days") + 
      theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0))+
      labs(x="Fecha", y="Cantidad de vacunas", colour = "Dosis")+
      geom_text(data=. %>% 
                  arrange(desc(cantidad)) %>% 
                  group_by(dosis) %>% 
                  slice(1), 
                aes(label=cantidad), 
                position=position_nudge(0.1), hjust=1.2, show.legend=FALSE)
  )
  output$grafico3 <- renderPlot({ grafico3() })

  grafico4 <- reactive(
    muertes_vacunas %>% filter(between(people_fully_vaccinated, input$vacunitas[1], input$vacunitas[2])) %>% 
      ggplot(aes(x=people_fully_vaccinated, y=cantFallecidos)) +geom_line()+ labs(x='Personas vacunadas con ambas dosis', 
                                                                                           y='Cantidad de fallecidos por día') +
      stat_wb_hbar(w.band = c(input$vacunitas[1],input$vacunitas[2]), size = 1.2,color='red') + stat_wb_mean(color='red',
                                                                                                             w.band =  c(input$vacunitas[1],input$vacunitas[2]),vjust = 7,  label.fmt  = "Promedio de muertes diarias = %.3g") +
      scale_x_continuous(n.breaks =8)
  )
  output$grafico4<-renderPlot({ grafico4() })
  
  
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html")
    ))
  })
}
shinyApp(ui, server)

