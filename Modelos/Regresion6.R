#REGRESION CON GRUPOS DE POPULARIDAD DE RANGO 20 Y CON LAS 5 VARIABLES CON MAYOR INDICE DE CORRELACION

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

data<-dbGetQuery(con, "SELECT popularity, acousticness, danceability, energy, instrumentalness, loudness FROM cancion")

data$group <- cut(data$popularity, c(-1, 20, 40, 60, 80, 100), labels = c('group0','group1','group2','group3','group4'))
data$group <- as.numeric(data$group)

formula<- data$group ~ data$acousticness + data$danceability + data$loudness + data$instrumentalness + data$energy

regresion.glm <-glm(formula, data = data)

#Visualización del resultado 

summary(regresion.glm)


#Aplicación del modelo

pred <-predict(regresion.glm, type="response")

#Error medio a priori
ECMaPriori <- sqrt(sum((residuals(regresion.glm)^2))/(length(residuals(regresion.glm))))
ECMaPriori

EMPaPriori <- sqrt(ECMaPriori) / mean(data$group)
EMPaPriori #33,59% de error medio "a priori"
