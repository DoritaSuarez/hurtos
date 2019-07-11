#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  filtro <- reactive({
    input$inp
  })
  
  
  hurtos <- read.csv("www/Hurto_a_personas_2018.csv", encoding = "UTF-8")
  names(hurtos)[19] <- "cod_dane"
  shape_dpto <- read.delim("www/shape_dptos.txt", sep = " ")
  divipola <- read.delim("www/divipola.txt", encoding = "UTF-8")
  poblacion <- read.delim("www/poblacion.txt", encoding = "UTF-8")
  
  dptos <- divipola %>% group_by(cod_dpto, dpto) %>% count()
  dptos <- left_join(dptos, poblacion, by = c("dpto" = "Departamento"))
  
  shape_cruce <- left_join(shape_dpto, dptos, by = c("id" = "cod_dpto"))
  # tail(shape_cruce)
  hurtos$cod_dpto <- substr(hurtos$cod_dane, 1, nchar(hurtos$cod_dane)-6)
  
  hurtos_dpto <- hurtos %>% group_by(cod_dpto) %>% count()
  names(hurtos_dpto)[2] <- "Numero_hurtos"
  
  
  shape_cruce$id <- shape_cruce$id %>% as.character()
  hurtos_dpto$cod_dpto <- hurtos_dpto$cod_dpto %>% as.character()
  
  hurtos_mapa <- left_join(shape_cruce, hurtos_dpto, by = c("id" = "cod_dpto"))
  hurtos_mapa[, "tasa"] <- ceiling(hurtos_mapa$Numero_hurtos/hurtos_mapa$Población.Total * 10000)  
  
  tema_mapas <- function(){
    theme(
      plot.background = element_blank(), 
      panel.background = element_blank(),
      strip.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.text = element_text(colour = "#295EAA", family = "Lato", size = rel(0.8)), # Color de las fuentes de los ejes
      legend.title = element_text(colour = "#295EAA", family = "Lato", size = rel(1)),
      # legend.position = "bottom",
      legend.background = element_blank(),
      legend.position = "none",
      # legend.key.width = unit,
      plot.margin = margin(0, 0, 0, 0, "cm")
    )
  }
  
  
  graficos <- function(input){
    if(input == "País"){
      p <- ggplot(hurtos_mapa, aes(long, lat, group = dpto, fill = tasa, label = Numero_hurtos)) +
        geom_polygon(colour = alpha("#CAD3DD", 1/2)) + tema_mapas()
      return(p)
    }
    
    if(input == "Sexo"){
      hurtos_sexo <- hurtos %>% group_by(Sexo) %>% count() %>% .[-c(1,2),]
      names(hurtos_sexo) <- c("Sexo", "Numero de hurtos")
      p_sex <- ggplot(hurtos_sexo, aes(x= Sexo, y = `Numero de hurtos`, fill = Sexo)) + geom_col()
      return(p_sex)
    }
    
    if(input == "Edad"){
      hurtos <- hurtos[-which(hurtos$Edad> 100), ]
      p_edad <- ggplot(hurtos, aes(x = Edad, fill = Sexo)) + geom_histogram(alpha = 0.8, position = "dodge")
      return(p_edad)
    }
    if(input == "Estado Civil"){
      hurtos$Estado.civil <- hurtos$Estado.civil %>% as.character()
      hurtos_estado <- hurtos %>% group_by(Estado.civil) %>% count() %>% .[-c(1,2),] %>% arrange(n)
      names(hurtos_estado) <- c("Estado.civil", "Numero.Hurtos")
      
      p_estado <- ggplot(hurtos_estado, aes(x= reorder(Estado.civil, -Numero.Hurtos), y = Numero.Hurtos, fill = Estado.civil)) + geom_col() + coord_flip() + xlab("Estado Civil") + ylab("Número de hurtos")
      return(p_estado)
    }
    
    if(input == "Arma Empleada"){
      hurtos$Arma.empleada <- hurtos$Arma.empleada %>% as.character()
      hurtos_arma <- hurtos %>% group_by(Arma.empleada) %>% count() %>% .[-c(1,2),] %>% arrange(n)
      names(hurtos_arma) <- c("Arma empleada", "Numero.Hurtos")
      
      p_arma <- ggplot(hurtos_arma, aes(x= reorder(`Arma empleada`, -Numero.Hurtos), y = Numero.Hurtos, fill = `Arma empleada`)) + geom_col() + coord_flip() + xlab("Arma Empleada") + ylab("Número de hurtos")
      return(p_arma)
    }
    
  }
  
  
  output$salida <- renderPlotly({
    graficos(input$inp)
  })
  
  
  
})
