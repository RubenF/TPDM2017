#lista
lista=seq(1,11,2)
lista

#matriz
matriz=matrix( lista,   nrow=3,    ncol=2) 
class(matriz)
t(matriz)

#data.frame
df<-data.frame(matriz)
colnames(df)
colnames(df)<-c('a', 'b')
df
t(df)
nrow(df); ncol(df)

#######para acceder

df[1,2]
matriz[1,2]

df$a[1]
df$a

df$c=df$b+df$a
df


matriz<-cbind(matriz,matriz[,1]+matriz[,2] )
matriz

matriz<-cbind(matriz,c(2,66,99) )
matriz<-cbind(matriz,c('a',66,99) )



##data.table ###es un data.frame con ventajas
library(data.table)
dt<-data.table(matriz)

colnames(dt)<-letters[2:6]
dt$b+dt$c
dt$b<-as.numeric(dt$b)
dt$c<-as.numeric(dt$c)

dt$w<-dt$b+dt$c
dt

sapply(dt, class) ##apply
dt1<-dt[2:3,]
rm(dt1)


###############################unir data.frames
mtcars
?mtcars ###bases de datos que vienen con R

class(mtcars)
sapply(mtcars, class)
row.names(mtcars)
mtcars["Duster 360",] ##permite acceder

###es igual, es distinto
mtcars[row.names(mtcars)=="Duster 360",]  
mtcars[row.names(mtcars)!="Duster 360",] 

row.names(mtcars)=="Duster 360"
seq(1, length(row.names(mtcars)))[row.names(mtcars)=="Duster 360"]
mtcars[7,]


####parto el data frame

mtcars1<-mtcars[1:2,]
mtcars2<-mtcars[3:nrow(mtcars),]

####uno data frames 
mtcars3<-rbind(mtcars1,mtcars2)
mtcars3==mtcars
sum(mtcars3==mtcars)
nrow(mtcars)*ncol(mtcars)

mtcars3[1,1]
mtcars3[1,1]<-22
mtcars3==mtcars
sum(mtcars3==mtcars)


###cbind
mtcars4<-cbind(mtcars[,1:2], mtcars3[,1:2]) ###mismas filas
head(mtcars4)
colnames(mtcars4)<-c('mpg', 'cyl', 'mpg.1', 'cyl.1')


###dplyr
mtcars5<-mtcars4[1:5,]
mtcars5

library(dplyr)
cyl<-data.frame(mtcars5%>%group_by(cyl)%>%summarize(cant=n()))
cyl
mtcars5[2,1]<-21.3
mtcars5
cyl2<-data.frame(mtcars5%>%group_by(cyl)%>%summarize(cant=n(), suma=sum(mpg), median(mpg), avg=mean(mpg)))
cyl2


#####################
mtcars5$nuevo<-seq(1,5,1)
mtcars5

cyl3<-data.frame(mtcars5%>%mutate(mpg/sum(nuevo)))
cyl3[,'mpg']/sum(cyl3$nuevo)


#########merge


cyl4<-cyl2[-1,]
unido<-merge(mtcars5, cyl4, by.x='cyl', by.y='cyl') ##es un inner

unidoInner<-merge(mtcars5, cyl4, by.x='cyl', by.y='cyl')
unidoInner

unidoLeft<-merge(mtcars5, cyl4, by.x='cyl', by.y='cyl', all.x=T)
unidoLeft

cyl5<-rbind(cyl2, cyl2[1,])
cyl5[4,1]<-5
cyl5
unidoRight<-merge(mtcars5, cyl5, by.x='cyl', by.y='cyl', all.y=T)
unidoRight

unidoAll<-merge(mtcars5, cyl5, by.x='cyl', by.y='cyl', all.x=T, all.y=T)
unidoAll


#############order y sort

l<-c(11,22,33,44)
order(l)
sort(l)

order(-l)
l[order(-l)]

######reshape #pivot tables

library(reshape) ###ver reshape 2 solos
mtcars5
f0<-cast(mtcars5, mpg~cyl, value='nuevo' ,mean)
?cast
f0
f00<-melt(f0, id=c( 'cyl'))
f00
f01<-f00[!is.nan(f00$value),]
########comparar f01 con mtcars5

#############cast, agrupando
mtcars5$nombres<-substr(rownames(mtcars5),0,5)
f<-cast(mtcars5, nombres~cyl, value='nuevo' ,mean)
f
###se puede volver???



##############################plots basicos

plot(seq(2,6,1), runif(5), type='l')

set.seed(1)
plot(seq(2,6,1), runif(5), type='l')

fech<-seq(as.Date('2017/02/01',"%Y/%m/%d"), as.Date('2017/02/05',"%Y/%m/%d"),1)
plot(fech,  runif(5), type='l', main='', xaxt='n')
axis.Date(1, at = fech, format= "%Y/%m/%d", las = 2)


hist(c(1,1,2,2,2,3,3,3,4,5,6), main='hist')
hist(log(c(1,1,2,2,2,3,3,3,4,5,6)), main='hist')
a=c(1,1,2,2,2,3,3,3,4,5,6)

boxplot(a)
median(a)
quantile(a,probs=c(0.25,0.75))



##################################################con datos externos##############
setwd('C:/Users/Gaby/Documents/Discover/ml-1m') 
rm(list=ls())
ratings<-read.table( "ratings.dat", sep=":", header=FALSE)  #####crear un archivo con tabs-- \t en Excel y levantarlo 
head(ratings)
ratings$V2<-NULL
ratings$V4<-NULL
ratings$V6<-NULL
ratings$date<-as.POSIXct(ratings$timestamp, origin="1970-01-01")
ratings$day<-as.Date(ratings$date)

colnames(ratings)<-c('userId', 'movieId', 'rating', 'timestamp')
head(ratings)
saveRDS(ratings, "ratings.RDS" ) ###salvarlo
rm(list=ls())
#evantarlo
ratings<-readRDS("ratings.RDS" )
head(ratings)

###########en clase

###hacer un loop

####para la lista de peliculas 1193, 661 y 914 hacer un loop que haga un plot

for (i in c(1193,661,914)){
  #i=1193
  x<-ratings[ratings$movieId==i,]
  hist(x$rating, main=paste0(i))
  }





###agrupo por peliculas los votos mayor a 3 y no
ratings$binario<-ifelse(ratings$rating>3,1,0)
##sólo para las pelis de antes


library(dplyr)
ratings1<-data.frame(ratings%>%filter(movieId %in% c(1193,661,914))%>%group_by(movieId,rating)%>%summarize(cant=n() ))
head(ratings1)
nrow(ratings[ratings$movieId==661 & ratings$rating==1,])

library(reshape)
colnames(ratings2)
cast(ratings1, rating~movieId)


####hacer una funcion cualquiera en clase






############hacer ejercicios

###ver dimensiones de matriz

###agrupar por rating y contar cant de registros, cant de peliculas y cant de usuarios 

###unir con la tabla agrupada por rating 

####hacer ratios 

###hacer ifelse (si el ranking >3 poner 1 sino 0)

###hacer un histograma de ratings












































