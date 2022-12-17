library(shiny)
library(shinythemes)
library(geojsonio)
library(ggplot2)


mapbos <- geojson_read( "https://raw.githubusercontent.com/ruiyi29/615Final/main/ZIP_Codes.geojson",what = "sp")
stops <- read.csv("https://raw.githubusercontent.com/ruiyi29/615Final/main/stop_id.csv")
stopid <- stops$stop_id
traveltime <- read.csv("https://raw.githubusercontent.com/ruiyi29/615Final/main/average%20travel%20time.csv")
stop <- traveltime$stop_id
time <- traveltime$average.travel.time
averagetime <- function(x){
  for (i in 1:19){
    if (x == stop[i]){
      y <- time[i]
      print(y)
    }
  }
}

ui <- 
  navbarPage(
    "Ananlysis on MBTA Transit Services", collapsible = TRUE, inverse = TRUE, 
    theme = shinytheme("flatly"),
    tabPanel("T",
             
             fluidPage(
               selectInput("stop_id", "Choose Start Stop",stopid),
               textOutput("text1"),
               plotOutput("plot1"),
               textOutput("text2"),
               textOutput("text3"),
               textOutput("text4"),
               verbatimTextOutput("text5")
             )
    ),
    tabPanel("Bus",plotOutput("plot2")),
    tabPanel("Ferry",plotOutput("plot3")),
    tabPanel("Commuter",plotOutput("plot4"))
  )

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
                 aes(x = stop_lon,
                     y = stop_lat),
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
      labs(title = "Stops on T Line (Orange)")
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
      theme_void() +
      coord_map() +
      labs(title = "Stops on Bus Line")
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
      theme_void() +
      coord_map() +
      labs(title = "Stops on Ferry Line")
  })
  output$plot4 <- renderPlot({
    ggplot() +
      geom_polygon(data = mapbos,
                   aes(x = long,
                       y = lat,
                       group = group),
                   alpha = 0,
                   color = "black",
                   size = .5) +
      theme_void() +
      coord_map() +
      labs(title = "Stops on Commuters")
  })
  output$text1 <- renderText({
    "Orange line is selected for this analysis. Destination and Direction are settled to be 70001 and 0. The point which already appears before you make the selection is the 
     position of the stop 70001. In order to make sense, please do not select 70001 as the start stop."
  })
  output$text2 <- renderText({
    paste("The stop you selected is",input$stop_id)
  })
  output$text3 <- renderText({
    paste("The average travel time between 70001 and",input$stop_id)
  })
  output$text4 <- renderText({
    paste("is")
  })
  output$text5 <- renderPrint({
    average_time <- averagetime(input$stop_id)
  })
}

shinyApp(ui = ui, server = server)
