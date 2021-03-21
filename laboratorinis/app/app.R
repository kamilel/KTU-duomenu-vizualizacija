library(shiny)
library(tidyverse)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("flatly"),
    titlePanel("Krovininis keliu transportas (ecoActCode: 494100)"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "imones_pav", label = "Imones pavadinimas", choices = NULL, selected = NULL)
        ),
        mainPanel(tabsetPanel(
            tabPanel("Grafikai", 
                     plotOutput("plot1"), 
                     plotOutput("plot2")),
            tabPanel("Duomenys", tableOutput("table"))
        )
        )
    )
)
server <- function(input, output, session) {
    data <- read_csv("https://raw.githubusercontent.com/kamilel/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv") %>% filter(ecoActCode == 494100)
    updateSelectizeInput(session, "imones_pav", choices = data$name, server = TRUE)
    
    output$table <- renderTable(
        data %>%
            filter(name == input$imones_pav) , digits = 0
    )
    output$plot1 <- renderPlot(
        data %>%
            filter(name == input$imones_pav) %>%
            mutate(month = as.numeric(substr(month, 5, 7))) %>%
            filter(!is.na(avgWage)) %>%
            ggplot(aes(x = month, y = avgWage), legend = FALSE) +
            geom_line(size = 1, col = "coral") +
            geom_point() +
            scale_x_continuous(breaks = 1:12) +
            theme_light() +
            labs(x = "Menuo", y = "Vidutinis atlyginimas", color = "Name", 
                 title = "Vidutinis atlyginimas 2020 m.")      
    )
    output$plot2 <- renderPlot(
        data %>%
            filter(name == input$imones_pav) %>%
            mutate(month = as.numeric(substr(month, 5, 7))) %>%
            filter(!is.na(numInsured)) %>%
            ggplot(aes(x = month, y = numInsured)) +
            geom_line(size = 1, col = "coral") +
            geom_point() +
            scale_x_continuous(breaks = 1:12) +
            theme_light() +
            labs(x = "Menuo", y = "Apdraustieji", title = "Apdraustu darbuotoju skaicius 2020 m.")          
    )
}
shinyApp(ui, server)