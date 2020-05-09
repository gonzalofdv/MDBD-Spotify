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

