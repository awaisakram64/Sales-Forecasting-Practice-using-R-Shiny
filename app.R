#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(ggplot2)
require(gridExtra)
library(fpp2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$div(class="Header",align = "center" ,style="font-family: Helvetica;",
           tags$h1("Shiny Text")),
  
  sidebarLayout(
    sidebarPanel(
      textInput("caption","Caption:","Data Summary"),
      selectInput(
      "item",
      "Chose a item:",
      "choices"),
      
      selectInput(
        "plot",
        "Chose a plot:",
        c("autoplot","Seasonal Plot","Polar","Lag Plot")),
      
      selectInput(
        "fplot",
        "Chose a Forecast plot:",
        c("seasonal naive","snaive","Simple Expotential Smoothing"))
      
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
          tags$div(class="header",align="center"),
          h3(textOutput("caption")),
          verbatimTextOutput("summary")
          
        ),
        tabPanel("Graphs",plotOutput("view")
        ),
        tabPanel("Forecast",plotOutput("forecast_view"))
      )
    )))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
  dataset <- read.csv(file = '/SOFTWARE ENGENRING/1. FYP/1st Iteration/Web App on R/sales.csv')
  dataset$Date <- as.Date(paste(dataset$Date, sep = ""), format = "%d-%b-%y")
  
  observe({
    updateSelectInput(session, "item", choices = unique(dataset$Model))
    
  })
  
  data <- reactive({
    itemdata <- subset(dataset,Model==input$item)
    return(itemdata)
  })
  
  tsdata <- reactive({
    itemdata <- ts(data()[,c("Count")],start=c(2016,8),frequency=365)
    # Fri Jan 25 12:45:21 2019 ------------------------------
    ts
    return(itemdata)
  })
  
  output$caption <- renderText({
    input$item
  })
  
  output$summary <- renderPrint({
    itemdata <- data()
   summary(itemdata)
  })
  
  output$view <- renderPlot({
    
    myts <- tsdata()
    print(myts)
    if (input$plot == 'autoplot') {
      autoplot(myts)
    }
    else if(input$plot == 'Seasonal Plot'){
      ggseasonplot(myts)
    }
    else if(input$plot == 'Polar'){
      ggseasonplot(myts,polar = T)
    }
    else if(input$plot == 'Lag Plot'){
      gglagplot(myts)
    }
    
  })
  output$forecast_view <- renderPlot({
    myts <- tsdata()
    if (input$fplot == 'seasonal naive'){
      fcnv <- naive(myts, h = 90)
      autoplot(fcnv)
    }
    else if(input$fplot=='snaive'){
      fcsnv <- snaive(myts, h = 90)
      autoplot(fcsnv)
    }
    else if(input$fplot == "Simple Expotential Smoothing"){
      fcses <- ses(myts, h = 90)
    autoplot(fcses)}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

