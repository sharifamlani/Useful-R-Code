rm(list=ls(all=TRUE))


server <- function(input, output, session) {
  
  output$myPlot <- renderPlot({
    distType <- input$Distribution
    size <- input$sampleSize
    
    if(distType == "Normal"){
      randomVec <- rnorm(size, mean = as.numeric(input$Mean), sd = as.numeric(input$SD))
    }
    else{
      randomVec <- rexp(size, rate = 1/as.numeric(input$Lambda))
    }
    
    hist(randomVec, col = "light blue")
  })
  
}