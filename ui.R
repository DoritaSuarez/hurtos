#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hurtos en Colombia"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("inp", "Estadísticas por:",
                  c("País",
                    "Sexo",
                    "Edad",
                    "Estado Civil",
                    "Arma Empleada"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      div(plotlyOutput("salida"))
    )
  )
))