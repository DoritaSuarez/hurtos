library(dplyr)
hurtos <- read.csv("www/Hurto_a_personas_2018.csv", encoding = "UTF-8")
names(hurtos)[19] <- "cod_dane"
shape_dpto <- read.delim("www/shape_dptos.txt", sep = " ")
divipola <- read.delim("www/divipola.txt", encoding = "UTF-8")
poblacion <- read.delim("www/poblacion.txt", encoding = "UTF-8")


dptos_list <- unique(as.character(hurtos$Departamento)) %>% .[which(!(. %in% c("", "-")))] %>% sort()

mpios_list <- unique(as.character(hurtos$Municipio)) %>% .[which(!(. %in% c("", "-")))] %>% sort()

zona_list <- unique(as.character(hurtos$Zona)) %>% .[which(!(. %in% c("", "-")))] 

sexo_list <- unique(as.character(hurtos$Sexo)) %>% .[which(!(. %in% c("", "-")))] %>% sort()

estado_civil_list <- unique(as.character(hurtos$Estado.civil)) %>% .[which(!(. %in% c("", "-")))] %>% sort()

clase_empleado_list <- unique(as.character(hurtos$Clase.de.empleado)) %>% .[which(!(. %in% c("", "-")))] %>% sort()

escolaridad_list <- unique(as.character(hurtos$Escolaridad)) %>% .[which(!(. %in% c("", "-")))] %>% sort()


