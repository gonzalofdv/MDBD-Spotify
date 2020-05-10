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


#REGRESIÓN GENERAL (popularity, energy, loudness)

#1. Creación del modelo
#1.1 datasheet entero
myFormula <- data$popularity ~ data$energy +data$loudness
poputotal.glm <-glm(myFormula, data = data)

#2.1 filtrando el datasheet para solo un cantante
dataOzuna<-filter(data, artist_name ="Ozuna")
myFormula <- dataOzuna$popularity ~ dataOzuna$energy +dataOzuna$loudness
ozuna.glm <-glm(myFormula, data = dataOzuna)

#2. Visualización 
#1.2 data
summary(poputotal.glm)

#2.2 Ozuna
summary(ozuna.glm)
#Resultados 2.2
Call:
glm(formula = myFormula, data = dataOzuna)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-34.231   -7.477    1.072    9.599   27.519  

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)         169.087     28.388   5.956 1.45e-07 ***
dataOzuna$energy    -93.249     29.032  -3.212  0.00212 ** 
dataOzuna$loudness    7.692      1.673   4.598 2.25e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 226.7216)

    Null deviance: 18442  on 62  degrees of freedom
Residual deviance: 13603  on 60  degrees of freedom
AIC: 525.41

Number of Fisher Scoring iterations: 2

#3. Aplicación del modelo
#1.3 data
pred <-predict(poputotal.glm, type="response")

#2.3 Ozuna
pred <-predict(ozuna.glm, type="response")

 #4. Visualización gráfica
 #1.4 data
plot(data$popularity, pred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)

 #2.4 Ozuna
 plot(dataOzuna$popularity, pred, xlab="Observed Values", ylab="Predicted Values")
 abline(a=0, b=1)

 
