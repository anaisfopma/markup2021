library(shiny)
library(gapminder)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggthemes)

ui <- fluidPage(
  titlePanel("Gapminder Data"),
  tags$p("This Shiny app plots the average life expectancy in countries against their GDP per capita, using the Gapminder dataset available in the",
         tags$a(href = "https://cran.r-project.org/web/packages/gapminder/README.html", "gapminder"), "package. It generates simplified versions of the",
         tags$a(href = "https://www.gapminder.org/tools/#$chart-type=bubbles&url=v1", "graphs developed by Gapminder"), "by sizing the countries according to their populations. The user can select a year from 1952 to 2007 and see the corresponding information for each country by hovering over the graph that is displayed."),
  sidebarPanel( 
    sliderInput("year", "Select year",min = 1952, max = 2007,step=5, value = 1952, ticks = F, sep = "")),
  mainPanel(plotlyOutput("plot")))


server <- function(input, output) {
  output$plot <- renderPlotly({
    
    gapminder <- gapminder %>% 
      filter(!row_number() %in% c(853:864)) #remove Kuwait from the dataset, as it seems to have unreliable values
    dat <- gapminder %>% filter(year == input$year)
    p <- ggplot(data = dat, aes(x = gdpPercap, y = lifeExp, text = c(paste("country:",country)), size = pop, colour = gdpPercap)) + 
      geom_point() +
      scale_colour_gradient(low = "lightblue", high = "darkblue") +
      labs(x = "GDP per capita", y = "Life expectancy in years") +
      xlim(0, 60000) + 
      ylim(25, 90) +
      theme_classic()
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
}

shinyApp(ui = ui, server = server)
