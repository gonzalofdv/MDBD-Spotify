#SCRIPT MODELO REGRESION



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

data<-dbGetQuery(con, "SELECT popularity, acousticness, danceability, duration, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence FROM cancion")

#Correlaciones de todas las variables numéricas con la popularidad

cor(data, data$popularity)

#Realizamos la regresion lineal con las 4 variables cuyo indice de correlación es mayor

regres<-lm(data$popularity ~ data$loudness + data$acousticness + data$danceability + data$instrumentalness)

#resumen de la regresion

summary(regres)

#Graficas referentes a la regresion

plot(regres)

#Regresión General (popularity, energy, loudness)

#Podemos utilizar los datos extraidos previamente
myFormula <- data$popularity ~ data$energy + data$loudness
poputotal.glm <- glm(myFormula, data = data)

#Regresión general para cantante Ozuna

#Obtenemos los datos del DW

dataOzuna<-dbGetQuery(con, "SELECT popularity, energy, loudness FROM cancion WHERE artista = 'Ozuna'")
myFormula2 <- dataOzuna$popularity ~ dataOzuna$energy + dataOzuna$loudness
ozuna.glm <- glm(myFormula2, data = dataOzuna)


#Resumen regresión general
summary(poputotal.glm)

#Resumen regresión general Ozuna
summary(ozuna.glm)


#Aplicación del modelo y gráficas

#Para todos los valores
pred <- predict(poputotal.glm, type="response")

plot(data$popularity, pred)
abline(a=0, b=1)

#Para el artista Ozuna
pred2 <- predict(ozuna.glm, type="response")

plot(dataOzuna$popularity, pred2)
abline(a=0, b=1)

#Evaluación
ECMaPriOz <- sqrt(sum((residuals(ozuna.glm)^2))/(length(residuals(ozuna.glm))))
ECMaPriOz

EMPaPriOz <- sqrt(ECMaPriOz) / mean(data$group)
EMPaPriOz #Tenemos un error medio de 83,4% "a priori"
