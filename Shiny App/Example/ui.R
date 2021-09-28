setwd("C:/Users/Shari/OneDrive/R-Scripts/Shiny App/Example")
#https://www.youtube.com/watch?v=Gyrfsrd4zK0

library(shiny)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("My Shiny app!"),
    
    sidebarPanel(
      selectInput("Distribution", "Please Select Distribution Type",
                  choices = c("Normal", "Exponential")),
      sliderInput("sampleSize", "Please Select Sample Size",
                  min = 100, max = 5000, value = 1000, step = 100),
      conditionalPanel(condition = "input.Distribution == 'Normal'",
                       textInput("Mean", "Please Select the mean", 10),
                       textInput("SD", "Please Select Standard Deviation", 3)),
      conditionalPanel(condition = "input.Distribution == 'Exponential'",
                       textInput("Lambda", "Please Select Lambda:",1))
    ),
    
    mainPanel(
      plotOutput("myPlot")
    )
  )
  
)