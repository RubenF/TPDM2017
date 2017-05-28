# Universidad de Buenos Aires. 
# Maestria en Explotacion de Datos y Descubrimiento de la Información
# Data Mining
# Trabajo Práctivo 2017
# Alumnos:  Ferrari, Mauricio
#           Flecha, Rubén
#           Gil, Myriam

# Elimino todo lo que haya en memoria
rm(list=ls()) 

# Defino el directorio de trabajo
setwd("C:/Users/e_flecha/Documents/GitHub/TPDM2017") 
# C:\Users\e_flecha\Desktop\TPDM2017
# C:\Users\e_flecha\Documents\GitHub\TPDM2017

#Confirmo directorio de trabajo
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

if(!require("arules")) install.packages("arules")
library(arules)


if(!require("XLConnectJars")) install.packages("XLConnectJars")
library(XLConnectJars)

if(!require("XLConnect")) install.packages("XLConnect")
library(XLConnect)

#-----------------------------------------------------------
# Carga de datos version csv
message("Cargando datos...")
ged50V6 <- read.csv("ged50V6.csv", sep = ",", header = T)
message("Carga de datos --> LISTO!!!")

str(ged50V6)
dim(ged50V6)
head(ged50V6)
# dim ged50V4.csv
#[1] 128264     48

# dim(ged50V6) <- Al eliminar duplicados, el número de registros queda en:
#[1] 127789     47

# En la variable GED50 armo el data frame con la estrucutra que quiero
GED50 <- select(ged50V6,  id,
                          relid,
                        # key2,
                        # key1,
                          year,
                          active_year,
                          type_of_violence,
                          conflict_dset_id,
                          conflictID,
                          conflict_name,
                          dyad_name,
                          side_a,
                          side_b,
                          country,
                          region,
                          duration,
                          best_est)
head(GED50)

# Creamos la variable GED50_grouped y la agrupamos como mejor queremos
GED50_grouped <- (  GED50 %>%
                    group_by(conflictID,
                            # year,
                            # conflict_dset_id,
                             # dyad_name,
                               side_a, 
                               side_b
                             ) %>%
                    summarize(total_dias = sum(duration),
                              number_deaths= sum(best_est)))

dim(GED50)
dim(GED50_grouped)
head(GED50_grouped)
typeof(GED50)
typeof(GED50_grouped)
str(GED50_grouped)
write.csv(GED50_grouped, file = "salida.csv")


#------------------------------------------------------
# 1. Análisis Exploratorio de datos
#------------------------------------------------------
# Completar!
#------------------------------------------------------
head(GED50)
str(GED50)
typeof(GED50)
summary(ged50)


# Histograma Cantidad Conflictos por año
hist(ged50$year)


hist(ged50$active_yea)
boxplot(ged50$year)
barplot(ged50$active_yea)
barplot(ged50$year)

type_of_v <- as.factor(ged50$type_of_vi)
type_of_v
str(type_of_v)

#3 tipos de v
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
# 2. Ayuda de Terceros- Dyadic level (Large)
#------------------------------------------------------

# Carga de datos de Ayuda de terceros - Version large
message("Cargando datos...")
ayudaL <- read.csv("extsup_largeV2.csv", sep = ",", header = T)
message("Carga de datos --> LISTO!!!")
dim(ayudaL)
# [1] 6519   30

# Nos quedamos solo con las ayudas de 1989 en adelante y los registros que tuvieron ayuda
# ayuda <- filter(ayuda, ywp_year > 1988)
# ayuda <- filter(ayuda, external_exists == 1)
head(ayudaL)
dim(ayudaL)
# [1] 3759   48
str(ayudaL)
ayudaL$conflictID <- as.factor(ayudaL$conflictID)
# Hago una version chica para probar
EXTSUP <- select(ayudaL, 
                      external_id, 
                      ywp_year, 
                      ywp_name, 
                      conflictID, 
                      external_exists, 
                      external_name, 
                      external_type_code)


head(EXTSUP)
dim(EXTSUP)
str(EXTSUP)
typeof(EXTSUP)



#------------------------------------------------------
# 3. Ayuda de Terceros - Conflict level (Small)
#------------------------------------------------------

# Carga de datos de Ayuda de terceros - Version corta
message("Cargando datos...")
ayuda <- read.csv("extsup_smallV2.csv", sep = ",", header = T)
message("Carga de datos --> LISTO!!!")

dim(ayuda)
head (ayuda)
str(ayuda)
#[1] 3606   62
# Nos quedamos solo con las ayudas de 1989 en adelante y los registros que tuvieron ayuda
# ayuda <- filter(ayuda, ywp_year > 1988)
# ayuda <- filter(ayuda, external_exists == 1)

#------------------------------------------------------
# 4. Tratados de Paz
#------------------------------------------------------

# Carga de datos de tratados de paz
# read.csv no lee los registros bien, tirando 236 (?)
#message("Cargando datos...")
#paz <- read.csv("ucdp-peace-agreements_V2.csv", sep = ",", header = T)
#message("Carga de datos --> LISTO!!!")
#dim(paz)
#[1] 236  69

# Usamos mejor XLConnect y leemos el XLS directo
install.packages("XLConnect")
library(XLConnect)

paz <- readWorksheetFromFile("ucdp-peace-agreements.xls", sheet = "PA dataset")
dim(paz)
# [1] 216  69   <--- correcto!
str(paz)

# Nos quedamos solo con las tratos de 1989 en adelante 
# Revisar. Year no es el año del tratado
#paz <- filter(paz, Year > 1988)
#dim(paz)
# [1] 188  69

# Paz reducido
 AGREEMENT <- select(paz,
                     PAID,
                     Region,
                     GWNO, 
                     CID, 
                     Name, 
                     DyadName,
                     bwdID, 
                     actorId, 
                     Inc, 
                     pa_name, 
                     Year, 
                     pa_date., 
                     pa_3rd,
                     ended, 
                     cease,
                     Intarmy,	
                     DDR,	
                     Withd,	
                     Mil_prov,	
                     pp,	
                     Intgov,	
                     Intciv,	
                     Elections,	
                     Interrim,	
                     Natalks,
                     Shagov,	
                     Pol_prov,	
                     Aut,	
                     Fed,	
                     Ind,	
                     Ref,	
                     Shaloc,	
                     Regdev,	
                     Cul,	
                     Demarcation,	
                     Locgov,	
                     Terr_prov,	
                     Amn,	
                     pris,	
                     Recon,	
                     Return,	
                     Justice_prov,	
                     Reaffirm,	
                     Reaffirm.ID,	
                     Outlin,	
                     PKO,	
                     Co_impl,	
                     DyVi05,	
                     CoVi01,
                     noconf,	
                     termdur,	
                     noconf11)
 
head(AGREEMENT)

#----------------------------------------------------------------------------------------------------
#PRUEBAS
fil<- (  ayuda %>%  select (year, external_name, conflictID) %>% group_by(external_name, conflictID) %>% summarize(cant=n()) )

fil<- data.frame(ayuda%>%group_by(external_name)%>%summarize(cant=n()))

head(fil)
filter(fil, external_name=="United States")

# Hago una version chica para probar
ayuda1 <- select(ayudaL, 
                 external_id, 
                 ywp_id,
                 ywp_year, 
                 ywp_name, 
                 conflictID, 
                 external_exists, 
                 external_name, 
                 external_type_X,
                 external_type_Y,
                 external_type_L #external_type_W, external_type_M, external_type_T, external_type_., external_type_I, external_type_O,external_type_U
                  )
str(ayuda1)
head(ayuda1)
dim(ayuda1)

# Creamos la variable GED50_grouped y la agrupamos como mejor queremos
ayuda1_grouped <- (  ayuda1 %>%
                      group_by( conflictID,
                                external_name
                               #external_id,
                               #ywp_id
                               #ywp_year,
                               #ywp_name,
                      ) %>%
                      summarize(total_X = sum(external_type_X),
                                total_Y = sum(external_type_Y),
                                total_L = sum(external_type_X)))

head(ayuda1_grouped)
fix(ayuda1_grouped)
filter(ayuda1_grouped, ayuda1_grouped$conflictID == "137")
filter(ayuda1_grouped, ayuda1_grouped$external_name =="United States")
str(ayuda1_grouped)
