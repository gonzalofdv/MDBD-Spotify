#SCRIPT MODELO REGRESION 3

#Librería RMySQL
library("RMySQL")

set.seed(28626)

#Conexión con la base de datos

con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")

#Obtenemos los datos del dw

data<-dbGetQuery(con, "SELECT popularity, acousticness, loudness, instrumentalness FROM cancion")

#Agrupamos en 5 grupos con rango 20

data$group <- cut(data$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data$group <- as.numeric(data$group)

formula<- data$group ~ data$acousticness + data$loudness + data$instrumentalness

regresion.glm <-glm(formula, data = data)

#Visualización del resultado 

summary(regresion.glm)


#Aplicación del modelo

pred <-predict(regresion.glm, type="response")

#4. Representación
plot(data$group, pred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)

#EVALUACIÓN

#Error medio a priori
ECMaPriori <- sqrt(sum((residuals(regresion.glm)^2))/(length(residuals(regresion.glm))))
ECMaPriori

EMPaPriori <- sqrt(ECMaPriori) / mean(data$group)
EMPaPriori #34,01% de error medio "a priori"


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# PRUEBA CON OTROS PARÁMETROS

data<-dbGetQuery(con, "SELECT popularity, acousticness, energy FROM cancion")

#Agrupamos en 5 grupos con rango 20

data$group <- cut(data$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data$group <- as.numeric(data$group)

formula<- data$group ~ data$acousticness + data$energy

regresion.glm <-glm(formula, data = data)

#Visualización del resultado 

summary(regresion.glm)


#Aplicación del modelo

pred <-predict(regresion.glm, type="response")

#4. Representación
plot(data$group, pred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)

#EVALUACIÓN

#Error medio a priori
ECMaPriori <- sqrt(sum((residuals(regresion.glm)^2))/(length(residuals(regresion.glm))))
ECMaPriori

EMPaPriori <- sqrt(ECMaPriori) / mean(data$group)
EMPaPriori #34,43% de error medio "a priori"
