# Universidad de Buenos Aires. 
# Maestria en Explotacion de Datos y Descubrimiento de la Información
# Data Mining
# Trabajo Práctivo 2017
# Alumnos:  Ferrari, Mauricio
#           Flecha, Rubén
#           Gil, Myriam


rm(list=ls()) # Elimino todo lo que haya en memoria
# Defino el directoria de trabajo
setwd("C:/Users/e_flecha/Documents/GitHub/TPDM2017") 
# C:\Users\e_flecha\Desktop\TPDM2017
# C:\Users\e_flecha\Documents\GitHub\TPDM2017

#Confirmo mi directorio de trabajo
getwd()

# Importo librerías a utilizar

if(!require("sqldf")) install.packages("sqldf")
library(sqldf)

if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if(!require("RgoogleMaps")) install.packages("RgoogleMaps")
library(RgoogleMaps)

if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

if(!require("data.table")) install.packages("data.table")
library(data.table)

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

#------------------------------------------------------
# 1. Análisis Exploratorio de datos
#------------------------------------------------------

head(ged50)
str(ged50)
summary(ged50)
#ged50$year <- as.factor(ged50$year)

# Histograma Cantidad Conflictos por año
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


#------------------------------------------------------
# 2. Ayuda de Terceros
#------------------------------------------------------

# Carga de datos de Ayuda de terceros

message("Cargando datos...")
ayuda <- read.csv("extsup_large.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")


# Nos quedamos solo con las ayudas de 1989 en adelante y los registros que tuvieron ayuda
ayuda <- filter(ayuda, ywp_year > 1988)
ayuda <- filter(ayuda, external_exists == 1)
head(ayuda)

# Hago una version chica para probar
ayuda_short <- select(ayuda, external_id, ywp_year, ywp_name, conflictID, external_exists, external_name, external_type_code)
head(ayuda_short)
dim(ayuda_short)
str(ayuda_short)
typeof(ayuda_short)
#filter(ayuda_short, conflictID == 137)

# Agrego una variable ayuda_anio, que es la union: Pais q recibe ayuda + pais que ayuda + codigo ayudas

ayuda_short$ayuda_anio = paste(ayuda_short$ywp_name, ayuda_short$external_name, ayuda_short$external_type_code, sep="-")
ayuda_short
head(ayuda_short)

filter(ayuda_short, conflictID == 92)

test <- select(ayuda_short,ywp_year, conflictID,  ayuda_anio)
test <-test[order(test$ywp_year, test$conflictID),]
head(test)
filter(test, conflictID == 92)

DT <- data.table(test)
DT
typeof(DT)

# Cuento cantidad de ayudas por conflicto y por año
DT[,num := seq_len(.N), by=c("ywp_year","conflictID")]
#DT[,id := rowid(ywp_year)]
head(DT)
dim(DT)

DT<-DT[order(DT$ywp_year),]

# Pivoteo tabla. Tengo un registro por cada combinacion año-conflicto y todas las ayudas en columnas
w <- reshape(data = DT,
              idvar = c("ywp_year","conflictID"),
              v.names = c("ayuda_anio"),
              timevar = "num",
              direction = "wide")

fil<- (  w %>%
           group_by(ywp_year, conflictID)           )

dim(w)
head(w)
fix(w)
head(fil)
dim(fil)
write.csv(w, file = "salida.csv")


#------------------------------------------------------
# 3. Tratados de Paz
#------------------------------------------------------

# Carga de datos de tratados de paz

message("Cargando datos...")
paz <- read.csv("ucdp-peace-agreements.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")
head(paz)
dim(paz)
ver <- paz$pa_comment
str(ver)
head(ver)
