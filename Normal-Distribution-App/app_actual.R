#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Normal Distribution"),

    # Sidebar with a slider input for number of bins 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot ({
    
    x <- seq(-4,4, length.out = 1e4)
    y <- dnorm(x)
    
    dat <- data.frame( x = x, y = y) #Normal Dist. Data
    
    x <- seq(2,4, length.out = 1e4)
    y <- dnorm(x)
    x <- c(x,rev(x))
    y <- c(y,rep(0, 1e4))
    
    right.tail <- data.frame(x = x, y = y)
    
    dat %>% ggplot(aes(x = x, y = y)) +
      geom_line(color = "blue", linewidth = 1) +
      ylab("Frequency") + 
      geom_polygon(data = right.tail,
                   fill = "blue", alpha = 0.4)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
