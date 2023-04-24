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
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "direction", 
                  label = "Direction:",
                  choices = list("Right Tail", "Left Tail", "Two-Tailed")),
      
      # selectInput(inputID = "alpha",
      #             label = "Alpha Level:",
      #             choices = list("0.05", "0.025", "0.001")),
      
      sliderInput(inputId = "z",   #Sliding Bar
                  label = "Z Value",
                  min = -4,
                  max = 4,
                  value = 0,
                  step = 0.01),
    ),
    
    # Sidebar with a slider input for number of bins 
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      h4(textOutput("text1")),
      h4(textOutput("text2"))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  x <- seq(-4,4, length.out = 1e4)
  y <- dnorm(x)
  
  dat <- data.frame( x = x, y = y) #Normal Dist. Data
  
  graph<- dat %>% ggplot(aes(x = x, y = y)) +
    geom_line(color = "blue", linewidth = 1) +
    ylab("Frequency") +
    xlab("Z") + 
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 15))
  
  
  shade = 0.5
  observe({
    if(input$direction == "Right Tail"){
      
      x <- seq(input$z,4, length.out = 1e4)
      y <- dnorm(x)
      x <- c(x,rev(x))
      y <- c(y,rep(0, 1e4))
      
      tail <- data.frame(x = x, y = y)
      
      alpha <- data.frame(x = c(1.96,1.96), y = c(0,dnorm(1.96))) 
      
      graph <- graph + geom_polygon(data = tail,
                                    fill = "#36b8f5", alpha = shade) 
        
      output$distPlot <- renderPlot({graph})
      
      output$text2 <- renderText(
        paste("P-value = ",
              integrate(dnorm, input$z, Inf)$value %>% round(digits = 3) %>% as.character())
      )
    }
    if(input$direction == "Left Tail"){
      
      x <- seq(input$z, -4, length.out = 1e4)
      y <- dnorm(x)
      x <- c(x,rev(x))
      y <- c(y,rep(0, 1e4))
      
      tail <- data.frame(x = x, y = y)
      
      graph <- graph + geom_polygon(data = tail,
                                    fill = "#36b8f5", alpha = shade)
      
      output$distPlot <- renderPlot({graph})
      
      output$text2 <- renderText(
        paste("P-value = ",
              integrate(dnorm, -Inf, input$z)$value %>% round(digits = 3) %>% as.character())
      )
    }

    if(input$direction == "Two-Tailed"){
      if (input$z >= 0){
        
        x <- seq(-input$z, -4, length.out = 1e4)
        y <- dnorm(x)
        x <- c(x,rev(x))
        y <- c(y,rep(0, 1e4))
        
        tailL <- data.frame(x = x, y = y)
        
        
        x <- seq(input$z, 4, length.out = 1e4)
        y <- dnorm(x)
        x <- c(x,rev(x))
        y <- c(y,rep(0, 1e4))
        
        tailR <- data.frame(x = x, y = y)
        
        graph <- graph + 
          geom_polygon(data = tailR,
                       fill = "#36b8f5", alpha = shade) + 
          geom_polygon(data = tailL,
                       fill = "#36b8f5", alpha = shade)
        
        output$distPlot <- renderPlot({graph})
        
        output$text2 <- renderText(
          paste("P-value = ",
                (integrate(dnorm, input$z, Inf)$value*2) %>% round(digits = 3) %>% as.character())
        )
        
      }
      
      if (input$z < 0 ){
        
        x <- seq(input$z, -4, length.out = 1e4)
        y <- dnorm(x)
        x <- c(x,rev(x))
        y <- c(y,rep(0, 1e4))
        
        tailL <- data.frame(x = x, y = y)
        
        
        x <- seq(-input$z, 4, length.out = 1e4)
        y <- dnorm(x)
        x <- c(x,rev(x))
        y <- c(y,rep(0, 1e4))
        
        tailR <- data.frame(x = x, y = y)
        
        graph <- graph + 
          geom_polygon(data = tailR,
                       fill = "#36b8f5", alpha = shade) + 
          geom_polygon(data = tailL,
                       fill = "#36b8f5", alpha = shade)
        
        output$distPlot <- renderPlot({graph})
        
        output$text2 <- renderText(
          paste("P-value = ",
                (integrate(dnorm, -Inf, input$z)$value*2) %>% round(digits = 3) %>% as.character())
        )
      }
    }
  })
  output$text1 <- renderText(
    paste("Z-value = ", {input$z}))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
