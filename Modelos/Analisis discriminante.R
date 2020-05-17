#SCRIPT ANÁLISIS DISCRIMINANTE

#Librería RMySQL
library("RMySQL")
library("MASS")

set.seed(28626)

#Conexión con la base de datos

con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")


#### PRUEBA SIN AGRUPAR LA VARIABLE POPULARIDAD CON VARIABLES ACOUSTICNESS, DANCEABILITY, INSTRUMENTALNESS Y LOUDNESS

data0<-dbGetQuery(con, "SELECT popularity, acousticness, danceability, instrumentalness, loudness FROM cancion")

attach(data0)
train=sample(seq(length(data0$popularity)), length(data0$popularity)*0.70,replace=FALSE)
lda.tr=lda(popularity[train]~.,data=data0[train,])
probs=predict(lda.tr,newdata=data0[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data0$popularity[-train])

mean(probs$class==data0$popularity[-train]) # 2.66% de bien clasificados


#### PRUEBA CON GRUPOS 10 GRUPOS DE RANGO 10 CON VARIABLES ENERGY Y LOUDNESS

data1<-dbGetQuery(con, "SELECT popularity, energy, loudness FROM cancion")

data1$group <- cut(data1$popularity, c(-1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c('group0','group1','group2','group3','group4','group5','group6','group7','group8','group9'))
data1$group <- as.numeric(data5$group)

data1$popularity<-NULL

attach(data1)
train=sample(seq(length(data1$group)), length(data1$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data1[train,])
probs=predict(lda.tr,newdata=data1[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data1$group[-train])

mean(probs$class==data1$group[-train])  # 26.38% de bien clasificados

###########################################

#### PRUEBA CON GRUPOS 5 GRUPOS DE RANGO 20 CON VARIABLE LOUDNESS

data2<-dbGetQuery(con, "SELECT popularity, loudness FROM cancion")

data2$group <- cut(data2$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data2$group <- as.numeric(data2$group)

data2$popularity <- NULL

attach(data2)
train=sample(seq(length(data2$group)), length(data2$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data2[train,])
probs=predict(lda.tr,newdata=data2[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data2$group[-train])

mean(probs$class==data2$group[-train])  # 44.5% de bien clasificados

###########################################

#### PRUEBA CON GRUPOS 4 GRUPOS DE RANGO 25 CON VARIABLES ACOUSTICNESS, LOUDNESS E INSTRUMENTALNESS

data3<-dbGetQuery(con, "SELECT popularity, acousticness, loudness, instrumentalness FROM cancion")

data3$group <- cut(data3$popularity, c(-1, 25, 50, 75, 100), labels = c('group0','group1','group2','group3'))
data3$group <- as.numeric(data3$group)

data3$popularity <- NULL

attach(data3)
train=sample(seq(length(data3$group)), length(data3$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data3[train,])
probs=predict(lda.tr,newdata=data3[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data3$group[-train])

mean(probs$class==data3$group[-train]) # 57.03% de bien clasificados

###########################################

#####################################################################################
#                                                                                   #
# UNA VEZ REALIZADAS LAS PRUEBAS ANTERIORES DECIDIMOS QUEDARNOS CON LOS MODELOS QUE #
# IMPLEMENTAN 5 GRUPOS DE RANGO 20 YA QUE AÚN TENIENDO MENOR ACIERTO QUE EL MODELO  #
# DE 4 GRUPOS, NOS PARECE MÁS LÓGICO REALIZAR LA DIVISIÓN EN 5 Y ES POR ESO QUE     #
# LOS SIGUIENTES ANÁLISIS SE REALIZARÁN CON 5 GRUPOS.                               #
#                                                                                   #
#####################################################################################

#### MÁS PRUEBAS CON 5 GRUPOS DE RANGO 20 CON VARIABLES ACOUSTICNESS Y DANCEABILITY

data4<-dbGetQuery(con, "SELECT popularity, acousticness, danceability FROM cancion")

data4$group <- cut(data4$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data4$group <- as.numeric(data4$group)

data4$popularity<-NULL

attach(data4)
train=sample(seq(length(data4$group)), length(data4$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data4[train,])
probs=predict(lda.tr,newdata=data4[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data4$group[-train])

mean(probs$class==data4$group[-train]) # 44.53% de bien clasificados

###########################################

#### MÁS PRUEBAS CON 5 GRUPOS DE RANGO 20 CON VARIABLES ACOUSTICNESS, LOUDNESS E INSTRUMENTALNESS

data5<-dbGetQuery(con, "SELECT popularity, acousticness, loudness, instrumentalness FROM cancion")

data5$group <- cut(data5$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data5$group <- as.numeric(data5$group)

data5$popularity <- NULL

attach(data5)
train=sample(seq(length(data5$group)), length(data5$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data5[train,])
probs=predict(lda.tr,newdata=data5[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data5$group[-train])

mean(probs$class==data5$group[-train]) # 44.3% de bien clasificados

###########################################
