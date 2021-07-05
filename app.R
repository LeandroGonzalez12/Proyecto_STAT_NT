library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    navlistPanel("Menu",
      tabPanel("Pregunta 1",
               h3("¿Cómo ha evolucionado la cantidad de dosis suministradas diariamente, comparando por laboratorio?", align="center", 
                    plotOutput("grafico1")),
               h4("¿Cuál es la proporción por laboratorio del total de dosis suministradas hasta el 23/06?", align="center", plotOutput("grafico2"))
      ),
      tabPanel("Pregunta 2",
               h3("¿Cómo fue la evolución de casos positivos dependiendo del departamento?", align="center", 
                  plotOutput("grafico3")),
               h4("¿Y si vemos cuántas personas han fallecido por departamento?", align="center", plotOutput("scat4"))
      ),
      tabPanel("Pregunta 3",
               h3("pregunta", align="center", plotOutput("scat")),
      ),
      tabPanel("Pregunta 4",
               h3("This is the third panel")
      ),
      tabPanel("Pregunta 5",
               h3("¿Se podría decir que en los departamentos más poblados, como Montevideo y Canelones el porcentaje de vacunados es mayor?",
                   align="center")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
