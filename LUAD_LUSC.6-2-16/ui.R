library(shiny)
library(shinyRGL)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

# Application title
    titlePanel("Expression Profiles for LUSC(600) and LUAD(551) Samples from TCGA"),

    # Sidebar with a slider input for the number of bins
    sidebarLayout(position="right",
                  sidebarPanel(
                      selectInput("colorby", "Color By:",
                                  choices=c("Cancer_Type" = "Cancer_Type"
                                            )),
                      plotOutput("distPlot")
                      
                  ),

                  # Show a plot of the generated distribution
                  mainPanel(
                  # webGLOutput("myWebGL", width=800, height=800)
                      webGLOutput("myWebGL", width=1200, height=1200)
                  )
                  )
))
