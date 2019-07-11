shape_dpto <- read.delim("input/shape_dptos.txt", sep = " ")
divipola <- read.delim("input/divipola.txt", encoding = "UTF-8")
poblacion <- read.delim("input/poblacion.txt", encoding = "UTF-8")

dptos <- divipola %>% group_by(cod_dpto, dpto) %>% count()
dptos <- left_join(dptos, poblacion, by = c("dpto" = "Departamento"))

shape_cruce <- left_join(shape_dpto, dptos, by = c("id" = "cod_dpto"))
tail(shape_cruce)
hurtos$cod_dpto <- substr(hurtos$Código.DANE, 1, nchar(hurtos$Código.DANE)-6)

hurtos_dpto <- hurtos %>% group_by(cod_dpto) %>% count()
names(hurtos_dpto)[2] <- "Numero_hurtos"


shape_cruce$id <- shape_cruce$id %>% as.character()
hurtos_dpto$cod_dpto <- hurtos_dpto$cod_dpto %>% as.character()

hurtos_mapa <- left_join(shape_cruce, hurtos_dpto, by = c("id" = "cod_dpto"))
hurtos_mapa[, "tasa"] <- ceiling(hurtos_mapa$Numero_hurtos/hurtos_mapa$Población.Total * 10000)

# Primera estadística: Número de hurtos por cada 10000 habitantes ---------

p <- ggplot(hurtos_mapa, aes(long, lat, group = dpto, fill = tasa, label = Numero_hurtos)) +
  geom_polygon(colour = alpha("#CAD3DD", 1/2))

ggplotly(p)

# Segunda estadística: Número de hurtos por cada 1000 habitantes por sexo

hurtos_sexo <- hurtos %>% group_by(Sexo) %>% count() %>% .[-c(1,2),]
names(hurtos_sexo) <- c("Sexo", "Numero de hurtos")

p_sex <- ggplot(hurtos_sexo, aes(x= Sexo, y = `Numero de hurtos`, fill = Sexo)) + geom_col()
ggplotly(p_sex)

# Por tipo de arma
hurtos$Arma.empleada <- hurtos$Arma.empleada %>% as.character()
hurtos_arma <- hurtos %>% group_by(Arma.empleada) %>% count() %>% .[-c(1,2),] %>% arrange(n)
names(hurtos_arma) <- c("Arma empleada", "Numero.Hurtos")

p_arma <- ggplot(hurtos_arma, aes(x= reorder(`Arma empleada`, -Numero.Hurtos), y = Numero.Hurtos, fill = `Arma empleada`)) + geom_col() + coord_flip() + xlab("Arma Empleada") + ylab("Número de hurtos")
ggplotly(p_arma)


# Por estado civil

hurtos$Estado.civil <- hurtos$Estado.civil %>% as.character()
hurtos_estado <- hurtos %>% group_by(Estado.civil) %>% count() %>% .[-c(1,2),] %>% arrange(n)
names(hurtos_estado) <- c("Estado.civil", "Numero.Hurtos")

p_estado <- ggplot(hurtos_estado, aes(x= reorder(Estado.civil, -Numero.Hurtos), y = Numero.Hurtos, fill = Estado.civil)) + geom_col() + coord_flip() + xlab("Estado Civil") + ylab("Número de hurtos")
ggplotly(p_estado)

# Por estado civil y sexo

hurtos_estado <- hurtos %>% group_by(Estado.civil, Sexo) %>% count() %>% .[-c(1:4),] %>% arrange(n)
names(hurtos_estado) <- c("Estado.civil", "Sexo", "Numero.Hurtos")

p_estado_sex <- ggplot(hurtos_estado, aes(x= reorder(Estado.civil, -Numero.Hurtos), y = Numero.Hurtos, fill = Sexo)) + geom_col(position = "dodge") + coord_flip() + xlab("Estado Civil") + ylab("Número de hurtos")
ggplotly(p_estado_sex)


# Por edad
hurtos <- hurtos[-which(hurtos$Edad> 100), ]
p_edad <- ggplot(hurtos, aes(x = Edad, fill = Sexo)) + geom_histogram(alpha = 0.8, position = "dodge")
ggplotly(p_edad)
