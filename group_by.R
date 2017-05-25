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


if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

if(!require("data.table")) install.packages("data.table")
library(data.table)

# Carga de datos version csv
# La version V5 tiene las columnas duration y durtion-1

message("Cargando datos...")
ged50v5 <- read.csv("ged50V5.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")

str(ged50v5)
typeof(ged50v5)

head(ged50v5)

#Hago una version chica para probar

ged50v5_short <- select(ged50v5, id, year,  conflict_dset_id, dyad_name, side_a, side_b, duration, best_est)
head(ged50v5_short)

dim(ged50v5_short)
str(ged50v5_short)
typeof(ged50v5_short)


#Como saco los distintos y los guardo en un vector
distinct_df = ged50v5_short %>% distinct(dyad_name)
distinct_vector <- distinct_df$dyad_name

# El group by!
fil<- (  ged50v5_short %>%
          group_by(year,conflict_dset_id, dyad_name,side_a, side_b) %>%
          summarize(total_dias = sum(duration),number_deaths= sum(best_est)))

fil<-fil[order(fil$conflict_dset_id)]

head(fil)
fil[1:20,]
str(fil)
write.csv(fil, file = "salida.csv")
# --------------------------------------------------------------------------------
head(fil)
str(fil)
typeof(fil)
dim(fil)

fil4<- filter(select(fil, id, year, deaths_unknown), deaths_unknown>100)

fil5 <-filter(select(fil, year, conflict_dset_id, dyad_name, side_a, side_b, total_dias, number_deaths), conflict_dset_id==1-171)
head(fil5)
#---------------------------------------------------------------------------------

#---------------------------------------------
# Carga de datos de Ayuda de terceros
message("Cargando datos...")
ayuda <- read.csv("extsup_large.csv", sep = ";", header = T)
message("Carga de datos --> LISTO!!!")

#Como saco los distintos y los guardo en un vector
distinct_ayuda <- ayuda %>% distinct(external_name)
typeof(distinct_ayuda)
distinct_vector <- distinct_df$dyad_name
dim(distinct_ayuda)


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
filter(ayuda_short, conflictID == 137)

# Agrego una variable ayuda_anio, que es la union del pais que ayuda + tipo de ayuda
ayuda_short$ayuda_anio = paste(ayuda_short$ywp_name, ayuda_short$external_name, ayuda_short$external_type_code, sep="-")
ayuda_short
# Que comiencen los tests
#test <- filter(ayuda_short, conflictID == 103 | conflictID == 90 |conflictID == 92)
test <- filter(ayuda_short)
test <- ayuda_short
head(test)
dim(test)
test2 <- select(test,ywp_year, conflictID, external_id, ywp_name, ayuda_anio)
# Lo ordeno
test2<-test2[order(test2$ywp_year),]
test2[1:20,]
head(test2)

w2 <- reshape(data = test2,
              idvar = c("ywp_year","conflictID","ywp_name"),
              timevar = "external_id",
              direction = "wide")
head(w2)
dim(w2)
typeof(w2)
w2[1:4]
w2[1:1,]
write.csv(w2, file = "salida.csv")

test3 <- select(test,ywp_year, conflictID,  ayuda_anio)
test3<-test3[order(test3$ywp_year),]
test3

# Con este for cuento las ayudas por conflicto por año
for (i in unique(test3$ywp_year)) 
  test3$num[test3$ywp_year == i] <- seq_len(sum(test3$ywp_year == i))

# Esto serviria para el unique compuesto

unique(test3[c("ywp_year", "conflictID")])
for (i in unique(test3[c("ywp_year", "conflictID")])) 
  test3$num[c("ywp_year", "conflictID") == i] <- seq_len(sum(c("ywp_year", "conflictID") == i))

# Con reshape paso las ayudas a columnas
w3 <- reshape(data = test3,
              idvar = c("ywp_year","conflictID"),
              v.names = c("ayuda_anio"),
              timevar = "num",
              direction = "wide")

test3$num <- ave(test3$ywp_year,  FUN = seq_along)
dim(w3)
test3
write.csv(w3, file = "salida.csv")
#----------
head(test)
test4 <- select(test,ywp_year, conflictID,  ayuda_anio)
test4 <- test
test4<-test4[order(test4$ywp_year, test4$conflictID),]
head(test4)

DT <- data.table(test4)
DT
typeof(DT)

# Cuento cantidad de ayudas por conflicto y por año
DT[,num := seq_len(.N), by=c("ywp_year","conflictID")]
#DT[,id := rowid(ywp_year)]

# Pivoteo tabla. Tengo un registro por cada combinacion año-conflicto y todas las ayudas en columnas
w4 <- reshape(data = DT,
              idvar = c("ywp_year","conflictID"),
              v.names = c("ayuda_anio"),
              timevar = "num",
              direction = "wide")
dim(w4)
head(w4)

write.csv(w4, file = "salida.csv")
