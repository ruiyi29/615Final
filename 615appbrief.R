library(shiny)
library(shinythemes)
library(geojsonio)
library(ggplot2)

mapbos <- geojson_read( "https://raw.githubusercontent.com/ruiyi29/615Final/main/ZIP_Codes.geojson",what = "sp")
stops <- read.csv("https://raw.githubusercontent.com/ruiyi29/615Final/main/stop_id.csv")
traveltime <- read.csv("https://raw.githubusercontent.com/ruiyi29/615Final/main/average%20travel%20time.csv")


ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel(title = div("Ananlysis on MBTA Transit Services")),
    navbarPage("",
               tabPanel("T",
                        tabsetPanel(
                          tabPanel("Explanation",textOutput("text1")),
                          tabPanel("70002",plotOutput("plot1"),textOutput("text2")),
                          tabPanel("70004",plotOutput("plot2"),textOutput("text3")),
                          tabPanel("70006",plotOutput("plot3"),textOutput("text4")),
                        )),
               tabPanel("Bus"),
               tabPanel("Ferry"),
               tabPanel("Commuter")
                        ))

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot() +
      geom_polygon(data = mapbos,
                   aes(x = long,
                       y = lat,
                       group = group),
                   alpha = 0,
                   color = "black",
                   size = .5) +
      geom_point(data = stops,
                 aes(x = -71.10806,
                     y = 42.30983),
                 fill = "red",
                 alpha = 1,
                 size = 3,
                 shape = 22) +
      geom_point(data = stops,
                 aes(x = -71.113686,
                     y = 42.300523),
                 fill = "red",
                 alpha = 1,
                 size = 3,
                 shape = 22) +
      theme_void() +
      coord_map() +
      labs(title = "70002 to 70001")
  })
  output$plot2 <- renderPlot({
    ggplot() +
      geom_polygon(data = mapbos,
                   aes(x = long,
                       y = lat,
                       group = group),
                   alpha = 0,
                   color = "black",
                   size = .5) +
      geom_point(data = stops,
                 aes(x = -71.10425,
                     y = 42.31706),
                 fill = "red",
                 alpha = 1,
                 size = 3,
                 shape = 22) +
      geom_point(data = stops,
                 aes(x = -71.113686,
                     y = 42.300523),
                 fill = "red",
                 alpha = 1,
                 size = 3,
                 shape = 22) +
      theme_void() +
      coord_map() +
      labs(title = "70004 to 70001")
  })
  output$plot3 <- renderPlot({
    ggplot() +
      geom_polygon(data = mapbos,
                   aes(x = long,
                       y = lat,
                       group = group),
                   alpha = 0,
                   color = "black",
                   size = .5) +
      geom_point(data = stops,
                 aes(x = -71.09959,
                     y = 42.32313),
                 fill = "red",
                 alpha = 1,
                 size = 3,
                 shape = 22) +
      geom_point(data = stops,
                 aes(x = -71.113686,
                     y = 42.300523),
                 fill = "red",
                 alpha = 1,
                 size = 3,
                 shape = 22) +
      theme_void() +
      coord_map() +
      labs(title = "70006 to 70001")
  })
  output$text1 <- renderText({
    "Orange line is selected for this analysis. Destination and Direction are settled to be 70001 and 0."
  })
  output$text2 <- renderText({
    "The average travel time between 70001 and 70002 is 131.7790."
  })
  output$text3 <- renderText({
    "The average travel time between 70001 and 70004 is 237.5932."
  })
  output$text4 <- renderText({
    "The average travel time between 70001 and 70006 is 365.2530."
  })
}

shinyApp(ui = ui, server = server)

    






