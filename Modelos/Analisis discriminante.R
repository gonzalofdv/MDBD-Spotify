#SCRIPT MODELO REGRESION 3

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


#Obtenemos los datos del dw

data<-dbGetQuery(con, "SELECT popularity, acousticness, loudness, instrumentalness FROM cancion")
data2<-dbGetQuery(con, "SELECT popularity, loudness FROM cancion")
data3<-dbGetQuery(con, "SELECT popularity, acousticness, loudness, instrumentalness FROM cancion")
data4<-dbGetQuery(con, "SELECT popularity, acousticness, danceability FROM cancion")
data5<-dbGetQuery(con, "SELECT popularity, energy, loudness FROM cancion")


#Agrupamos en 5 grupos con rango 20

data$group <- cut(data$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data$group <- as.numeric(data$group)

data2$group <- cut(data2$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data2$group <- as.numeric(data2$group)

data3$group <- cut(data3$popularity, c(-1, 25, 50, 75, 100), labels = c('group0','group1','group2','group3'))
data3$group <- as.numeric(data3$group)

data4$group <- cut(data4$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data4$group <- as.numeric(data4$group)

data5$group <- cut(data5$popularity, c(-1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c('group0','group1','group2','group3','group4','group5','group6','group7','group8','group9'))
data5$group <- as.numeric(data5$group)


data$popularity <- NULL
data2$popularity <- NULL
data3$popularity <- NULL
data4$popularity<-NULL
data5$popularity<-NULL
#AQUI LA PRUEBA DE CLASIFICADOR

attach(data)
train=sample(seq(length(data$group)), length(data$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data[train,])
probs=predict(lda.tr,newdata=data[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data$group[-train])

mean(probs$class==data$group[-train])

attach(data2)
train=sample(seq(length(data2$group)), length(data2$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data2[train,])
probs=predict(lda.tr,newdata=data2[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data2$group[-train])

mean(probs$class==data2$group[-train])

attach(data3)
train=sample(seq(length(data3$group)), length(data3$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data3[train,])
probs=predict(lda.tr,newdata=data3[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data3$group[-train])

mean(probs$class==data3$group[-train])


attach(data4)
train=sample(seq(length(data4$group)), length(data4$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data4[train,])
probs=predict(lda.tr,newdata=data4[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data4$group[-train])

mean(probs$class==data4$group[-train])

attach(data5)
train=sample(seq(length(data5$group)), length(data5$group)*0.70,replace=FALSE)
lda.tr=lda(group[train]~.,data=data5[train,])
probs=predict(lda.tr,newdata=data5[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,data5$group[-train])

mean(probs$class==data5$group[-train])