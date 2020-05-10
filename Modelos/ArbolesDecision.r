#SCRIPT MODELO ARBOLES DE DECISION

#cargamos las librerias que usaremos
library(party)
#No habrá conexión con la base de datos en este modelo porque no soy el localhost

#carga del csv. cambiar ruta por la ruta correspondiente
dataset <- read.csv('C:/Users/Moha/Desktop/Universidad/MDBD/SpotifyPredictore/dataset.csv')
copia <- dataset

#preparación del dataset para trabajar. Usaremos la copia para no modificar el dataset original
#convertir los valores de 'clave', 'genero', 'modo' y 'time_signature' en factors
copia$genero <- as.factor(copia$genero)
copia$clave <- as.factor(copia$clave)
copia$mode <- as.factor(copia$mode)
copia$time_signature <- as.factor(copia$time_signature)

#comprobamos y limpiamos los valores del dataset que no esten completos (tienen algun NA)
copia <- copia[complete.cases(copia),]

#agrupamos los valores de popularidad en 10 intervalos de rango 10
copia$group <- cut(copia$popularity, c(-1, 10, 20, 30, 40, 50 ,60 ,70 ,80 ,90 ,100), labels = 
                     c('group0','group1','group2','group3','group4','group5','group6','group7','group8','group9'))

#creamos la semilla para poder replicar los resultados
set.seed(1234)

#comprobaremos el grupo de la popularidad en funcion del resto de variables para empezar
formula1 <- group ~ genero + acousticness + danceability + duration + energy + 
  instrumentalness + clave + liveness + loudness + mode + speechiness + tempo +time_signature + valence
#ahora, el grupo con solo las variables de acousticness, danceability, loudness, energy e instrumentalness, que son las que mayor correlacion tienen 
formula2 <- group ~ acousticness + danceability + energy + 
  instrumentalness + loudness
#hacemos la division en entrenamiento y prueba, empezaremos con una division del 70%/30% respectivamente
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.7, 0.3))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles
tree1 <- ctree(formula1, train)
tree2 <- ctree(formula2, train)

#una vez tengamos los arboles, comprobemos su precision
table(predict(tree1), train$group)
table(predict(tree2), train$group)

precTrain1 <- mean(predict(tree1) == train$group) #nos da un valor del 45.24% en entrenamiento
precTrain2 <- mean(predict(tree2) == train$group) #nos da un valor del  31.72% en entrenamiento 

precTest1 <- mean(predict(tree1, newdata = test) == test$group) #nos da un valor del 44.33% en prueba
precTest2 <- mean(predict(tree2, newdata = test) == test$group) #nos da un valor del 30.94% en prueba
#Una precisión bastante baja para ambos si consideramos un buen valor de precision aquel por encima del 70%

#probemos ahora con una division entrenamiento/prueba del 80/20
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.8, 0.2))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles
tree1 <- ctree(formula1, train)
tree2 <- ctree(formula2, train)

#una vez tengamos los arboles, comprobemos su precision
table(predict(tree1), train$group)
table(predict(tree2), train$group)

precTrain1 <- mean(predict(tree1) == train$group) #nos da un valor del 45.20% en entrenamiento
precTrain2 <- mean(predict(tree2) == train$group) #nos da un valor del  31.86% en entrenamiento 

precTest1 <- mean(predict(tree1, newdata = test) == test$group) #nos da un valor del 45.16% en prueba
precTest2 <- mean(predict(tree2, newdata = test) == test$group) #nos da un valor del 30.58% en prueba

#----------------------------------------------------------------------
#Ejecutar a partir de aqui que a mi me da un error que no se solucionar|
#----------------------------------------------------------------------
#comprobemos ahora con una menor dispersion de la variable groups. Dividamos la popularidad en 5 tramos en vez de 10 
copia$group <- cut(copia$popularity, c(-1, 20, 40, 60, 80, 100), labels = 
                     c('group0','group1','group2','group3','group4'))
#Mismo procedimeinto que antes
#hacemos la division en entrenamiento y prueba, empezaremos con una division del 70%/30% respectivamente
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.7, 0.3))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles
tree3 <- ctree(formula1, train)
tree2 <- ctree(formula2, train)

#una vez tengamos los arboles, comprobemos su precision
table(predict(tree1), train$group)
table(predict(tree2), train$group)

precTrain1 <- mean(predict(tree1) == train$group) #nos da un valor del 45.24% en entrenamiento
precTrain2 <- mean(predict(tree2) == train$group) #nos da un valor del  31.72% en entrenamiento 

precTest1 <- mean(predict(tree1, newdata = test) == test$group) #nos da un valor del 44.33% en prueba
precTest2 <- mean(predict(tree2, newdata = test) == test$group) #nos da un valor del 30.94% en prueba
#Una precisión bastante baja para ambos si consideramos un buen valor de precision aquel por encima del 70%

#probemos ahora con una division entrenamiento/prueba del 80/20
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.8, 0.2))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles
tree1 <- ctree(formula1, train)
tree2 <- ctree(formula2, train)

#una vez tengamos los arboles, comprobemos su precision
table(predict(tree1), train$group)
table(predict(tree2), train$group)

precTrain1 <- mean(predict(tree1) == train$group) #nos da un valor del 45.20% en entrenamiento
precTrain2 <- mean(predict(tree2) == train$group) #nos da un valor del  31.86% en entrenamiento 

precTest1 <- mean(predict(tree1, newdata = test) == test$group) #nos da un valor del 45.16% en prueba
precTest2 <- mean(predict(tree2, newdata = test) == test$group) #nos da un valor del 30.58% en prueba

#Los resultados siguen siendo bajos. Los arboles de decision serán una solución poco precisa por lo que no seguiremos investigandolos


