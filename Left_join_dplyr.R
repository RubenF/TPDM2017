# PRUEBAS 

rm(list=ls()) # Elimino todo lo que haya en memoria
# Defino el directoria de trabajo
setwd("C:/Users/ruben_nuc/Desktop/TPDM2017") 

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


#-----------------------------------
# Carga de datos version csv
message("Cargando datos...")
ged50 <- read.csv("ged50V4.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")

dim(ged50)
str(ged50)

# Carga de datos version csv
message("Cargando datos...")
ayuda <- read.csv("extsup_largeV1.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")

dim(ayuda)
str(ayuda)

join3 <-  ged50 %>% left_join(ayuda, by = "key1")
head(join3)
dim(join3)
str(join3)

filter(join3, key1 == '1989137Government of Afghanistan - Hizb-i Islami-yi Afghanistan - Khalis faction')
