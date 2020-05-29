#SCRIPT MODELO REGRESION 2 UTILIZANDO ENERGY Y LOUDNESS

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

data<-dbGetQuery(con, "SELECT popularity, energy, loudness FROM cancion")

#Agrupamos la popularidad en grupos de 10
data$group <- cut(data$popularity, c(-1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c('group0','group1','group2','group3','group4','group5','group6','group7','group8','group9'))

#Hacemos los grupos como numérico
data$group <- as.numeric(data$group)

#Eliminamos columna popularidad ya que solo usaremos los grupos
data$popularity<-NULL

formula<- data$group ~ data$energy + data$loudness

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

EMPaPriori <- sqrt(ECMaPriori) / mean(data$group)
EMPaPriori #26,3% de error medio "a priori"
