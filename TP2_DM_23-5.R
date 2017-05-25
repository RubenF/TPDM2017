# Importo librerías a utilizar

if(!require("sqldf")) install.packages("sqldf")
library(sqldf)

if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if(!require("RgoogleMaps")) install.packages("RgoogleMaps")
library(RgoogleMaps)

if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

if(!require("arules")) install.packages("arules")
library(arules)

if(!require("pmml")) install.packages("pmml")
library(pmml)


rm(list=ls()) # Elimino todo lo que haya en memoria
# Defino el directoria de trabajo
setwd("C:/Users/e_flecha/Documents/GitHub/TP2") 
# C:\Users\e_flecha\Desktop\TPDM2017
# C:\Users\e_flecha\Documents\GitHub\TPDM2017
jester <- read.csv ('jester-data-1.csv', header = TRUE, dec = '.')

reglas <- apriori(jester, parameter=list(support=0.11,confidence = 0.75))

jester_d <- jester

rangos <- function (){
  for (x in 1:100 ) {
    J <- paste("J",x,sep="")
    jester_d[[J]] <<- ordered(cut (jester[[J]], c(-10,-5,0,1,5,10),
                                   labels = c("M","R","B","MB","E")))
  }
}
# El siguiente comando ejecuta la función que definimos mas arriba
rangos()



#dmg.org/pmml_examples/archive/TreeClassification.xml

jester_d [["Jokes"]] <- NULL

fix(jester_d)
reglas <- apriori(jester_d, parameter=list(support=0.11,confidence = 0.75))

summary(reglas)

inspect(sort(reglas, by = "lift"))
inspect(head(reglas,n=10))

# Punto 10
reglasJ32consecuente <-subset(reglas, subset = rhs %in% "J32=" )
reglasJ13antecedente <-subset(reglas, subset = lhs %in% "J13=" )
reglasSupport <-subset(reglas, subset = support > 0.2 )
reglasLift <-subset(reglas, lift > 1.5)

it <- items(reglas)
it
typeof(it)
fix(it)

esCerrado <- is.closed(it)
esMaximo <- is.maximal(it)

write.PMML(reglas, file = "rules.xml")

#---------------------------------
import <- read.csv ('import_reducido.csv', header = TRUE, dec = '.')
str(import)
reglas <- apriori(import, parameter=list(support=0.11,confidence = 0.75))
