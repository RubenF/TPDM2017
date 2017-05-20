# Universidad de Buenos Aires. 
# Maestria en Explotacion de Datos y Descubrimiento de la Información
# Data Mining
# Trabajo Práctivo 2017

rm(list=ls()) # Elimino todo lo que haya en memoria
# Defino el directoria de trabajo
setwd("C:/Users/e_flecha/Documents/GitHub/TPDM2017") 
# C:\Users\e_flecha\Desktop\TPDM2017
# C:\Users\e_flecha\Documents\GitHub\TPDM2017

#Confirmo mi directorio de trabajo
getwd()

# Importo librerías
# sqldf: Libreria me permite trabajar con SQL
if(!require("sqldf")) install.packages("sqldf")
library(sqldf)

# Libreria ggplot2
if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# RgoogleMaps
if(!require("RgoogleMaps")) install.packages("RgoogleMaps")
library(RgoogleMaps)


# Lectura de los datos
# Version RDatafile
#message("Cargando datos...")
#load("ged50.Rdata") 
#message("Carga de datos --> LISTO!!!")


# Carga de datos version csv
message("Cargando datos...")
ged50 <- read.csv("ged50V4.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")

dim(ged50)
str(ged50)


# Análisis Exploratorio de datos
head(ged50)
str(ged50)
summary(ged50)
ged50$year <- as.factor(ged50$year)

str(anio)
hist(anio)

hist(ged50$year)
hist(ged50$active_yea)
boxplot(ged50$year)
barplot(ged50$active_yea)
barplot(ged50$year)

type_of_v <- as.factor(ged50$type_of_vi)
type_of_v
str(type_of_v)

#Atencion Solo hay 3 tipos de v
plot(type_of_v)

# Cantidad de conflictos por año
plot(ged50$year)
head(ged50$conflict_d)
head(ged50$conflict_1)
plot(ged50$conflict_1)
# Where Precition
where_prec <- as.factor(ged50$where_prec)
plot(where_prec)

# Probemos algo---------
ggplot(economics, aes(date, unemploy / pop)) +
  geom_line()
ggplot(economics, aes(date, uempmed)) +
  geom_line()

ggplot(economics, aes(unemploy / pop, uempmed)) +
  geom_path() +
  geom_point()
year <- function(x) as.POSIXlt(x)$year + 1900
ggplot(economics, aes(unemploy / pop, uempmed)) +
  geom_path(colour = "grey50") +
  geom_point(aes(colour = year(date)))
#---------------------------------------------
# Carga de datos de Ayuda de terceros
message("Cargando datos...")
ayuda <- read.csv("extsup_large.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")
dim(ayuda)
str(ayuda)
