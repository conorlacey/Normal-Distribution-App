library(shiny)
library(tidyverse)
library(rsconnect)

fluidPage(
  
  # Application title
  titlePanel("Z-Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "direction", 
                  label = "Direction:",
                  choices = list("Right Tail", "Left Tail", "Two-Tailed")),
      
      selectInput(inputId = "alpha",
                  label = "Alpha:",
                  choices = list("0.05", "0.025", "0.001")),
      
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
