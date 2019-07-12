#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "www/estilo.css")
  # ),
  includeCSS("styles.css"),
  
  # Application title
  titlePanel("Hurtos en Colombia"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("inp", "Estadísticas por:",
                  c("Edad",
                    "Sexo",
                    "País",
                    "Estado Civil",
                    "Arma Empleada")),
      div( "Este tablero interactivo te permiritá explorar un conjunto de estadísticas relacionadas con los delitos de hurtos ocurridos durante 2018. Las estadísticas mostradas corresponden al número de hurtos ocurridos en todo el país y fueron tomados del portal de datos abiertos Colombia."),
      
    h4("¿Quieres saber a cuántas personas con determinadas características hurtaron en 2018?"),
    h4("Ingresa los datos y calcula!"),
    
    selectInput("dpto_user", "Departamento", c("Todos", dptos_list)),
    selectInput("mpio_user", "Municipios", "Todos"),
    selectInput("zona_user", "Zona", c("Todas", zona_list)),
    selectInput("sexo_user", "Sexo", c("Todos", sexo_list)),
    selectInput("estado_user", "Estado Civil", c("Todos", estado_civil_list)),
    selectInput("escolaridad_user", "Escolaridad", c("Todas", escolaridad_list)),
    # actionButton("do", "Click Me"),
    h2(textOutput("probabilidad"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(width = 7,
      div(plotlyOutput("salida", height = "600px"))))
    )
  )
))
