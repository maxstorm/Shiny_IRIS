library(shiny)
library(datasets)
library(dplyr)


function(input, output, session) {

  selectedData <- reactive({ iris[, c(input$xcol, input$ycol)] })
  clusters <- reactive({ kmeans(selectedData(), input$clusters) })
  
  output$plot1 <- renderPlot({
    palette(c("black", "blue", "red", "green",
              "yellow", "orange", "pink", "purple", "grey"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
}
  
  
  