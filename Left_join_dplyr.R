# PRUEBAS 

rm(list=ls()) # Elimino todo lo que haya en memoria
# Defino el directoria de trabajo
#setwd("C:/Users/ruben_nuc/Desktop/TPDM2017") 
setwd("C:/Users/e_flecha/Documents/GitHub/TPDM2017")

#Confirmo mi directorio de trabajo
getwd()

# Importo librerías
# sqldf: Libreria me permite trabajar con SQL
if(!require("sqldf")) install.packages("sqldf")
library(sqldf)

# Libreria ggplot2
if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# String
if(!require("string")) install.packages("string")
library(stringr)
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

superheroes <- "
    name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
superheroes
str(superheroes)
library(nycflights13)
airlines
str(airlines)
dim(airlines)

flights
dim(flights)
flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier)
flights2
head(flights2)
dim(flights2)

weather
dim(weather)
head(weather)
join1 <- flights2 %>% left_join(weather)
join1
head(join1)

dim(planes)
head(planes)
planes
str(planes)
tailf <- as.factor(planes$tailnum)
str(tailf)
tailnum
str(tailnum)

join2 <- flights2 %>% left_join(planes, by = "tailnum")
str(join2)
dim(join2)
head(join2)


#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Carga de datos version csv
# La version V5 tiene las columnas duration y durtion-1

message("Cargando datos...")
ged50v5 <- read.csv("ged50V5.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")

str(ged50v5)
head(ged50v5)
#Hago una version chica para probar

ged50v5_short <- select(ged50v5, id, relid)
fil4<- filter(select(fil, id, year, deaths_unknown), deaths_unknown>100)
head(fil4)

# Carga de datos version csv
message("Cargando datos...")
ayuda <- read.csv("extsup_largeV1.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")

dim(ayuda)
str(ayuda)

join3 <-  ged50v5 %>% left_join(ayuda, by = "key1")

head(join3)
dim(join3)
str(join3)

# where con dplyr
fil <- filter(join3, key1 == '1989137Government of Afghanistan - Hizb-i Islami-yi Afghanistan - Khalis faction')
fil
head(fil)
str(fil)

#Select con dplyr
fil2 <- select(fil,id, relid, year, key1)
fil2

# Select + where 'Meteodo 1'
fil3 <-(fil %>% select(id, relid, year, key1, deaths_unknown) %>% filter (deaths_unknown > 100))
head(fil3)

# Select + where 'Metodo 2'
fil4<- filter(select(fil, id, year, deaths_unknown), deaths_unknown>100)
head(fil4)

fil4
write.csv(fil, file = "salida.csv")
