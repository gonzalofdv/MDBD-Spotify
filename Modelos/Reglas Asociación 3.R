#PREPROCESAMIENTO DE DATOS

#Librer�a RMySQL
library("RMySQL")

set.seed(28626)

#Conexión con la base de datos

con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")

#Obtenemos los datos del dw

dataRules<-dbGetQuery(con, "SELECT popularity, acousticness, danceability, energy, loudness, genero, clave, mode FROM cancion")


#Creamos una columna nueva en la que vamos a agrupar los datos de popularidad en 
#5 intervalos 

dataRules$popularityGroup = dataRules$popularity


#Reglas de asociacion(Seguimos con la parte de preprocesamiento):

#Primero nos aseguramos de que no hay agregacion de datos:
summary(dataRules)

#   genre               
#   Electronic : 9149   
#   Alternative: 9095   
#   Classical  : 8834    
#   Reggae     : 8687   
#   Reggaeton  : 8549   
#   Blues      : 8496  
#   (Other)    :81139  

#Podemos ver que en la columna de genero los datos no estan agregados

#Ahora eliminamos las columnas que no nos van a aportar informacion(En las primeras iteraciones vimos que tenian
#poca correlacion o nula con la popularidad):
#Eso solo es para procesamiento sin conexión a la BD (con todo el dataset):
	#dataRules$track_id = NULL
	#dataRules$artist_name = NULL
	#dataRules$track_name = NULL
	#dataRules$popularity = NULL
	#dataRules$duration_ms = NULL
	#dataRules$liveness = NULL
	#dataRules$instrumentalness = NULL
	#dataRules$speechiness = NULL
	#dataRules$tempo = NULL
	#dataRules$valence = NULL

#Eliminamos la columna popularity que no nos será util
dataRules$popularity = NULL

#Pasamos a factor los valores numericos

#popularityGRoup
dataRules$popularityGroup <- cut(dataRules$popularityGroup, br=c(1,20,40,60,80,101), labels = c("1","2","3","4","5"))
#Energy
dataRules$energy <- cut(dataRules$energy, br=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), labels = c("1","2","3","4","5","6","7","8","9","10"))
#Loudness
dataRules$loudness <- cut(dataRules$loudness, br=c(-50,-40,-35,-30,-25,-20,-15,-10,-5,0,5), labels = c("1","2","3","4","5","6","7","8","9","10"))
#Acousticness
dataRules$acousticness <- cut(dataRules$acousticness, br=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), labels = c("1","2","3","4","5","6","7","8","9","10"))
#Danceability
dataRules$danceability <- cut(dataRules$danceability, br=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), labels = c("1","2","3","4","5","6","7","8","9","10"))



#PROCESAMIENTO DE DATOS CON EL MODELO DE REGLAS DE ASOCIACION:

#Instalamos arules y lo cargamos en la memoria:

if(!require(arules)){
  install.packages("arules", dependencies = TRUE)
}
require(arules) 


#Metemos en rules.popularity todas las reglas que aparezcan en un 0,5% del total de items(supp=0,005) y que
#al menos tengan una confianza del 40%, es decir, que de las veces que se da esa relacion, al menos en el 40%
#de los casos se obtenga la rhs.
#Ademas, obligamos a que en la rhs aparezca el grupo de popularidad al que pertenece, para poder obtener resultados
#acerca de las reglas que mas influyen en la popularidad.
rules.popularity <- apriori(dataRules,parameter = (list(minlen=2,supp=0.005,conf=0.8)),
                            appearance = list(rhs=c("popularityGroup=5","popularityGroup=4","popularityGroup=3","popularityGroup=2","popularityGroup=1")))



#Si inspeccionamos las reglas obtenidas ordenandolas de mayor a menor confidence obtenemos:
inspect(sort(rules.popularity,by="confidence"))

#lhs                                                                   rhs                 support     confidence lift      count
#[1]   {genre=Indie,mode=Major,acousticness=1}                            => {popularityGroup=3} 0.006122274 0.8872604   1.997140  787 
#[2]   {genre=Opera,energy=1,loudness=4}                                  => {popularityGroup=1} 0.005235439 0.8751625  10.540572  673 
#[3]   {genre=Alternative,key=C#}                                         => {popularityGroup=3} 0.006557913 0.8735751   1.966336  843 
#[4]   {genre=Opera,loudness=4,acousticness=10}                           => {popularityGroup=1} 0.005227660 0.8727273  10.511241  672 
#[5]   {genre=Opera,loudness=4}                                           => {popularityGroup=1} 0.005818883 0.8717949  10.500011  748 
#[6]   {genre=Alternative,energy=6,loudness=8}                            => {popularityGroup=3} 0.005733312 0.8701299   1.958581  737 
#[7]   {genre=Indie,acousticness=1}                                       => {popularityGroup=3} 0.008930586 0.8677249   1.953167 1148 
#[8]   {genre=Indie,loudness=8,acousticness=1}                            => {popularityGroup=3} 0.005406583 0.8665835   1.950598  695 
#[9]   {genre=Opera,mode=Major,danceability=5}                            => {popularityGroup=1} 0.006402328 0.8663158  10.434020  823 
#[10]  {genre=Alternative,mode=Minor,loudness=8}                          => {popularityGroup=3} 0.012462368 0.8640777   1.944958 1602 

#Solo se muestran las 10 primeras reglas pero por lo general vemos que la popularidad esta bastante ligada al
#genero, ademas con respecto a la iteracion previa vemos como categorizar la popularidad en 5 grupos distintos da muchos
#mejores resultados que hacerlo en 10 ya que bajamos un poco la precision



#Clasificador:

install.packages("caret")  
install.packages("arulesCBA")
require("caret")
require("arulesCBA")


#Creamos los datasets que van a ser usados como entrenamiento y como pruebas
indexes <- createDataPartition(dataRules$popularityGroup, p=0.8, list = F)

train <- dataRules[indexes,]
test <- dataRules[-indexes,]

#Creamos el clasificador que tenga en cuenta las reglas con un confidence igual o superior a 0,3
clasificador <- CBA(
       popularityGroup ~ ., data = train, supp = 0.0001, conf=0.3, verbose = FALSE
   )

#Ahora comparamos la eficiencia:
predicted_strong <- predict(clasificador, test)

cross_tab_strong<- table(predicted = predicted_strong, true = test$popularityGroup)

accuracy_strong <- (cross_tab_strong[1,1]+cross_tab_strong[2,2]+cross_tab_strong[3,3]+cross_tab_strong[4,4]+cross_tab_strong[5,5])/sum(cross_tab_strong)

accuracy_strong
