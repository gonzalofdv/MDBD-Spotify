#SCRIPT MODELO ARBOLES DE DECISION

#cargamos las librerias que usaremos
library(rpart)
library(caret)
#SIN ACCESO A LA BASE DE DATOS
  #No habrá conexión con la base de datos en este modelo porque no soy el localhost
  
  #carga del csv. cambiar ruta por la ruta correspondiente
  #dataset <- read.csv('C:/Users/Moha/Desktop/Universidad/MDBD/SpotifyPredictore/dataset.csv')
  #copia <- dataset

#CON ACCESO A LA BASE DE DATOS

#Conexión con la base de datos

con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")

#Obtenemos los datos del dw

copia<-dbGetQuery(con, "SELECT * FROM cancion")

#preparación del dataset para trabajar. Usaremos la copia para no modificar el dataset original

#convertir los valores de 'clave', 'genero', 'modo' y 'time_signature' en factors
copia$genero <- as.factor(copia$genero)
copia$clave <- as.factor(copia$clave)
copia$mode <- as.factor(copia$mode)
copia$time_signature <- as.factor(copia$time_signature)

#comprobamos y limpiamos los valores del dataset que no esten completos (tienen algun NA)
copia <- copia[complete.cases(copia),]

#creamos la semilla para poder replicar los resultados
set.seed(1234)

###################################################
###Todas las formulas a considerar
#Con estas formulas podemos probar la influencia de distintos grupos de variables

formula1 <- group ~ genero + acousticness + danceability + duration + energy + 
  instrumentalness + clave + liveness + loudness + mode + speechiness + tempo +time_signature + valence

formula2 <- group ~ acousticness + danceability + duration + energy + 
  instrumentalness  + liveness + loudness  + speechiness + tempo + valence

formula3 <- group ~ acousticness + danceability + energy + 
  instrumentalness  + liveness + loudness  + speechiness + valence

formula4 <- group ~ acousticness + 
  instrumentalness + liveness + speechiness + valence

formula5 <- group ~ genero + clave + mode + time_signature

###################################################


#10 grupos de rango 10
copia$group <- cut(copia$popularity, c(-1,10, 20,30,40, 50, 60, 70, 80 ,90,100), labels = 
        c('group0','group1','group2','group3','group4','group5','group6','group7','group8','group9'))
#hacemos la division en entrenamiento y prueba, empezaremos con una division del 70%/30% respectivamente
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.7, 0.3))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles


#FORMULA 1
tree <- rpart(formula1, train, control = rpart.control(minsplit = 10))

#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest1 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain1 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest1$overall
cmTrain1$overall



#FORMULA 2
tree <- rpart(formula2, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest2 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain2 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest2$overall
cmTrain2$overall



#FORMULA 3
tree <- rpart(formula3, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest3 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain3 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest3$overall
cmTrain3$overall


#FORMULA 4
tree <- rpart(formula4, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest4 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain4 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest4$overall
cmTrain4$overall
#FORMULA 5
tree <- rpart(formula5, train, control = rpart.control(minsplit = 10))


#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest5 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain5 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest5$overall
cmTrain5$overall
#########################################################




#5 grupos de rango 20
copia$group <- cut(copia$popularity, c(-1,20,40,60,80,100), labels = 
        c('group0','group1','group2','group3','group4'))

#hacemos la division en entrenamiento y prueba, empezaremos con una division del 70%/30% respectivamente
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.7, 0.3))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles


#FORMULA 1
tree <- rpart(formula1, train, control = rpart.control(minsplit = 10))

#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest1 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain1 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest1$overall
cmTrain1$overall



#FORMULA 2
tree <- rpart(formula2, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest2 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain2 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest2$overall
cmTrain2$overall



#FORMULA 3
tree <- rpart(formula3, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest3 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain3 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest3$overall
cmTrain3$overall


#FORMULA 4
tree <- rpart(formula4, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest4 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain4 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest4$overall
cmTrain4$overall
#FORMULA 5
tree <- rpart(formula5, train, control = rpart.control(minsplit = 10))


#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest5 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain5 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest5$overall
cmTrain5$overall
#########################################################



#4 grupos de rango 25
copia$group <- cut(copia$popularity, c(-1,25,50,75,100), labels = 
        c('group0','group1','group2','group3'))

#hacemos la division en entrenamiento y prueba, empezaremos con una division del 70%/30% respectivamente
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.7, 0.3))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles


#FORMULA 1
tree <- rpart(formula1, train, control = rpart.control(minsplit = 10))

#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest1 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain1 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest1$overall
cmTrain1$overall



#FORMULA 2
tree <- rpart(formula2, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest2 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain2 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest2$overall
cmTrain2$overall



#FORMULA 3
tree <- rpart(formula3, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest3 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain3 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest3$overall
cmTrain3$overall


#FORMULA 4
tree <- rpart(formula4, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest4 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain4 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest4$overall
cmTrain4$overall
#FORMULA 5
tree <- rpart(formula5, train, control = rpart.control(minsplit = 10))


#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest5 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain5 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest5$overall
cmTrain5$overall
#########################################################



#2 grupos de rango 50
copia$group <- cut(copia$popularity, c(-1, 50,100), labels = 
        c('group0','group1'))

#hacemos la division en entrenamiento y prueba, empezaremos con una division del 70%/30% respectivamente
ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.7, 0.3))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles


#FORMULA 1
tree <- rpart(formula1, train, control = rpart.control(minsplit = 10))

#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest1 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain1 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest1$overall
cmTrain1$overall



#FORMULA 2
tree <- rpart(formula2, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest2 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain2 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest2$overall
cmTrain2$overall



#FORMULA 3
tree <- rpart(formula3, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest3 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain3 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest3$overall
cmTrain3$overall


#FORMULA 4
tree <- rpart(formula4, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest4 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain4 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest4$overall
cmTrain4$overall
#FORMULA 5
tree <- rpart(formula5, train, control = rpart.control(minsplit = 10))


#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest5 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain5 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest5$overall
cmTrain5$overall
#########################################################



# Agrupación de popularidad irregular, niveles:
# desconocido [0,20)
# inpopular [20,50)
# poco conocida [50, 60)
# algo conocida [60, 70)
# conocida [70, 80)
# popular [80, 90)
# viral [90, 100]
copia$group <- cut(copia$popularity, c(-1, 20, 50, 60, 70, 80 ,95,100), labels = 
                     c('desconocido','inpopular','poco conocida','algo conocida','conocida','popular','viral'))




ind <- sample(2, nrow(copia), replace=TRUE, prob=c(0.7, 0.3))
train <- copia[ind == 1, ]
test <- copia[ind == 2,]

#hora de construir los arboles


#FORMULA 1
tree <- rpart(formula1, train, control = rpart.control(minsplit = 10))

#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest1 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain1 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest1$overall
cmTrain1$overall



#FORMULA 2
tree <- rpart(formula2, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest2 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain2 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest2$overall
cmTrain2$overall



#FORMULA 3
tree <- rpart(formula3, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest3 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain3 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest3$overall
cmTrain3$overall


#FORMULA 4
tree <- rpart(formula4, train, control = rpart.control(minsplit = 10))
#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest4 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain4 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest4$overall
cmTrain4$overall
#FORMULA 5
tree <- rpart(formula5, train, control = rpart.control(minsplit = 10))


#Realizamos la poda de los arboles y obtenemos las precisiones 
opt <- which.min(tree$cptable[,"xerror"])
cp <- tree$cptable[opt, "CP"]
tree_prune <- prune(tree, cp= cp)

#Matriz de confusión y precisión del modelo
cmTest5 <- confusionMatrix(table(test$genero, predict(tree_prune, test, type = "class")))
cmTrain5 <- confusionMatrix(table(train$genero, predict(tree_prune, train, type = "class")))

cmTest5$overall
cmTrain5$overall
#########################################################
