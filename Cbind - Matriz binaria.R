# Elimino todo lo que haya en memoria
rm(list=ls())

# sqldf: Libreria me permite trabajar con SQL
if(!require("sqldf")) install.packages("sqldf")
library(sqldf)

# Libreria ggplot2
if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Libreria dplyr
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Libreria leaflet
if(!require("leaflet")) install.packages("leaflet")
library(leaflet)

#setear directorio de trabajo y llamar archivo
setwd("C:/Users/AA/Desktop/Mauricio/Maestría Datamining/1.- Materias/2.- Data Mining/TP Materia/Últimas versiones")

# Cargar bases
ged50<-read.csv("ged50CSV2-csv.csv", header=TRUE, sep=',', dec='.')
extsup<-read.csv("extsup_largeCSV-csv.csv", header=TRUE, sep=',', dec='.')

# Adaptar External Support (sin éxito)
es_country<-unique(extsup$external_name)
es_typesup<-unique(extsup$external_type_of_support.1)
extsup2<-extsup[,3:6]
extsup3<-extsup[,9:10]
extsup4<-extsup[,12]
extsup5<-extsup[,17:22]
extsup7<-cbind(extsup2, extsup3, extsup4,extsup5)
glimpse(extsup7)
class(extsup7)
rm(extsup6)
head(extsup7)

# Crear columnas de 0 y 1 desde la columna external_name
extsup2<-cbind(extsup, model.matrix( ~ 0+external_name,extsup))
glimpse(extsup2)
head(extsup2)

# Visualizar bases
head(ged50)
str(ged50)
unique(extsup$external_name)
dim(ged50)
str(ged50)
dim(extsup)
str(extsup)
glimpse(extsup2)
head(extsup)

# Agrupar Ged50 por conflicto y eligiendo las variables que me interesan mostrar
ged501<-data.frame(ged50%>%select(everything())%>%group_by(year, conflict_dset_id)%>%summarize(cant=n()))
glimpse(ged501)
head(ged501)
ged501<-data.frame(ged50%>%group_by(year, type_of_violence, conflict_dset_id)%>%summarize(count=n(),TotMuertes=sum(best_est)))
glimpse(ged501)
head(ged501)

ged501<-data.frame(ged50%>%group_by(region)%>%summarize(count=n(),TotMuertes=sum(best_est)))
head(ged501)
ged50_year<-data.frame(ged50%>%group_by(year)%>%summarize(Conflictos=n(),TotMuertes=sum(best_est)))
head(ged50_year)
ged50_year_conf<-data.frame(ged50%>%group_by(year)%>%summarize(Conflictos=n()))
head(ged50_year_conf)
ged50_reg_conf<-data.frame(ged50%>%group_by(region)%>%summarize(Conflictos=n(),TotMuertes=sum(best_est),AvgMuertes_Conf=sum(best_est)/n()))
head(ged50_reg_conf)

# Filtros
filter(ged501, conflict_dset_id=="1-10", year=="1989")
filter(ged501, grepl("1-10",conflict_dset_id))
filter(extsup2, external_nameAfghanistan==1)

#Gráficos
# Gráfico-ged501-Barras acostadas
barMuertes_region=barplot(ged501$TotMuertes, names.arg=ged501$region)
barMuertes_region=barplot(ged501$TotMuertes, border=F, names.arg=ged501$region)
barMuertes_region=barplot(ged501$TotMuertes, border=F, names.arg=ged501$region,horiz=TRUE)
barMuertes_region=barplot(ged501$TotMuertes, border=F, names.arg=ged501$region,horiz=TRUE, las=2, cex.axis=0.7, cex.names=0.7)
barMuertes_region=barplot(ged501$TotMuertes, border=F, names.arg=ged501$region,horiz=TRUE, las=2, cex.axis=0.7, cex.names=0.7, col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.3,0.5,0.4,0.6), rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6), rgb(0.3,0.5,0.4,0.6)))
barMuertes_region=barplot(ged501$TotMuertes, border=F, names.arg=ged501$region,horiz=TRUE, las=2, cex.axis=0.7, cex.names=0.7, col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.3,0.5,0.4,0.6), rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6), rgb(0.3,0.5,0.4,0.6)), main="Muertes en conflictos según región")
text(barMuertes_region, adj=0,pos=4,paste("n = ",ged501$TotMuertes,sep="") ,cex=0.7)

#Gráfico ged50_year-Barras
barged50_year<-ggplot(ged50_year, aes(x = year, y = TotMuertes,fill = year )) + geom_bar(width = 0.9, stat="identity")
barged50_year

#Gráfico ged50_región-Barras circulares
barreg_conf<-ggplot(ged50_reg_conf, aes(x = region, y = Conflictos ,fill = region)) + 
  geom_bar(width = 0.65, stat="identity") + 
  coord_polar(theta = "y") +  
  xlab("") + ylab("") +
  geom_text(ged50_reg_conf = ged50_reg_conf, hjust = 1, size = 2, aes(x = region, y = 0, label = region)) +
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())
barreg_conf

#Gráficos de mapas
#Descarga de mapas
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
system("unzip world_shape_file.zip")

# library leaflet - Agregar un mapa en blanco
# We always initiate a leaflet map with the leaflet() function
m=leaflet()
# Then we Add default OpenStreetMap map tiles
m=addTiles(m)
m

# Agregar puntos al mapa
ged50_mapa<-data.frame(long=ged50$longitude ,  lat=ged50$latitude)

# Show a circle at each position
m=leaflet(ged50_mapa) %>% addTiles() %>% addCircleMarkers(~long, ~lat , popup = ~as.character(ged50$country))
m
rm(m)
