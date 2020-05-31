#SCRIPT MODELO REGRESION 1


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

data<-dbGetQuery(con, "SELECT popularity, acousticness, danceability, instrumentalness, loudness FROM cancion")

#Realizamos la regresion lineal con las 4 variables cuyo indice de correlación es mayor

regres<-lm(data$popularity ~ data$loudness + data$acousticness + data$danceability + data$instrumentalness, data=data)

summary(regres)


ECMaPri <- sqrt(sum((residuals(regres)^2))/(length(residuals(regres))))
EMPaPri <- sqrt(ECMaPri) / mean(data$popularity) #Tenemos un error de 9.15% (0.09153787) a priori

#Graficas referentes a la regresion

plot(regres)

#############################################################################

#Regresión Generalizada (popularity, energy, loudness)

#Obtenemos datos del dw y construimos la formula

data<-dbGetQuery(con, "SELECT popularity, energy, loudness FROM cancion")

myFormula <- data$popularity ~ data$energy + data$loudness

#Realizamos la regresión

poputotal.glm <- glm(myFormula, data = data)

#resumen de la regresion

summary(poputotal.glm)

#Función predict para la gráfica

pred <- predict(poputotal.glm, type="response")

plot(data$popularity, pred)
abline(a=0, b=1)

#Error medio a priori

ECMaPri <- sqrt(sum((residuals(poputotal.glm)^2))/(length(residuals(poputotal.glm))))
EMPaPri <- sqrt(ECMaPri) / mean(data$popularity) #Tenemos un error de 9.2% (0.09209373) a priori

#Regresión general para cantante Ozuna

#Obtenemos los datos del DW y construimos la formula

dataOzuna<-dbGetQuery(con, "SELECT popularity, energy, loudness FROM cancion WHERE artista = 'Ozuna'")

myFormula2 <- dataOzuna$popularity ~ dataOzuna$energy + dataOzuna$loudness

#Realizamos la regresión

ozuna.glm <- glm(myFormula2, data = dataOzuna)


#Resumen regresión general Ozuna
summary(ozuna.glm)


#Predict para la grafica

pred2 <- predict(ozuna.glm, type="response")

plot(dataOzuna$popularity, pred2)
abline(a=0, b=1)


#Evaluación (ERROR MEDIO A PRIORI)
ECMaPriOz <- sqrt(sum((residuals(ozuna.glm)^2))/(length(residuals(ozuna.glm))))

EMPaPriOz <- sqrt(ECMaPriOz) / mean(data$popularity) #Tenemos un error medio de 9,26% (0.09268116) "a priori"
