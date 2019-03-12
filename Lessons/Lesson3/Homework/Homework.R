library(shiny)
library(dplyr)
library(ggplot2)
dt_KPI_raw <- read.csv("./Data/lesson2_KPI.csv")
data <- dt_KPI_raw %>% filter_all(all_vars(!is.na(.)))

ui <- fluidPage(
  selectInput("select", label=h1("Scatter plot with colour"),
              choices = list("Region", "Unit", "Segment", "Business", "Year")),
  plotOutput(outputId = "graph"))

server <- function(input, output) 
{
  output$graph <- renderPlot({
    ggplot(data, aes_string(x = "Premium", y = "Expenses")) +
      geom_point(aes_string(x = data$Premium, y = data$Expenses, colour = input$select)) +
      geom_smooth(aes_string(x = data$Premium, y = data$Expenses, colour = input$select), 
                  method = loess, se = FALSE)})
}

shinyApp(ui, server)