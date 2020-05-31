#PREPROCESAMIENTO DE DATOS

#LibrerÃa RMySQL
library("RMySQL")

set.seed(28626)

#ConexiÃ³n con la base de datos

con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")

#Obtenemos los datos del dw

dataRules<-dbGetQuery(con, "SELECT popularity, acousticness, danceability, energy, loudness, genero, clave, mode FROM cancion")

#Con la siguiente instruccion comentada, podriamos construir el modelo sin conexion con la base de datos
#dataRules <- read.csv('SpotifyFeatures.csv', encoding = 'UTF-8')

#Creamos una columna nueva en la que vamos a agrupar los datos de popularidad en 
#10 intervalos 

dataRules$popularityGroup = dataRules$popularity

#Susituimos los datos de la nueva columna por su grupo de popularidad correspondiente

dataRules$popularityGroup <- cut(dataRules$popularityGroup, br=c(1,10,20,30,40,50,60,70,80,90,101), labels = c("1","2","3","4","5","6","7","8","9","10"))




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
  #Esto solo para procesamiento sin conexiÃ³n a BD (con todo el dataset):
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

  #Para procesamiento con BD, habremos extraido Ãºnicamente la informaciÃ³n necesaria
    dataRules$popularity = NULL

#Pasamos a factor los valores numericos

#popularityGRoup
dataRules$popularityGroup <- cut(dataRules$popularityGroup, br=c(1,2,3,4,5,6,7,8,9,10), labels = c("1","2","3","4","5","6","7","8","9","10"))
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
rules.popularity <- apriori(dataRules,parameter = (list(minlen=2,supp=0.005,conf=0.4)),
                     appearance = list(rhs=c("popularityGroup=10","popularityGroup=9","popularityGroup=8","popularityGroup=7","popularityGroup=6","popularityGroup=5","popularityGroup=4","popularityGroup=3","popularityGroup=2","popularityGroup=1")))

#ESPECIFICACIONES DE NUESTRA LLAMADA A APRIORI:
 # specification:
 #  confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target   ext
 #0.4    0.1    1 none FALSE            TRUE       5   0.005      2     10  rules FALSE

 #Algorithmic control:
 #  filter tree heap memopt load sort verbose
 #0.1 TRUE TRUE  FALSE TRUE    2    TRUE

 #Absolute minimum support count: 642 

 #set item appearances ...[10 item(s)] done [0.00s].
 #set transactions ...[84 item(s), 128547 transaction(s)] done [0.08s].
 #sorting and recoding items ... [79 item(s)] done [0.01s].
 #creating transaction tree ... done [0.09s].
 #checking subsets of size 1 2 3 4 5 6 done [0.16s].
 #writing ... [246 rule(s)] done [0.00s].
 #creating S4 object  ... done [0.02s].



#Si inspeccionamos las reglas obtenidas ordenandolas de mayor a menor confidence obtenemos:
inspect(sort(rules.popularity,by="confidence"))

#lhs                                                         rhs                 support     confidence lift     count
#[1]   {genre=R&B,acousticness=1}                               => {popularityGroup=5} 0.007771477 0.5946429  2.250605  999 
#[2]   {genre=R&B,loudness=8,acousticness=1}                    => {popularityGroup=5} 0.005445479 0.5922166  2.241422  700 
#[3]   {genre=R&B,danceability=7}                               => {popularityGroup=5} 0.006472341 0.5777778  2.186774  832 
#[4]   {genre=R&B,mode=Minor}                                   => {popularityGroup=5} 0.011404389 0.5699844  2.157278 1466 
#[5]   {genre=R&B}                                              => {popularityGroup=5} 0.023718951 0.5695871  2.155774 3049 
#[6]   {genre=R&B,mode=Major}                                   => {popularityGroup=5} 0.012314562 0.5692197  2.154384 1583 
#[7]   {genre=Rock,acousticness=1}                              => {popularityGroup=6} 0.005196543 0.5685106  2.958239  668 
#[8]   {genre=R&B,mode=Minor,loudness=8}                        => {popularityGroup=5} 0.007670346 0.5679724  2.149663  986 
#[9]   {genre=R&B,energy=6}                                     => {popularityGroup=5} 0.005134309 0.5665236  2.144179  660 
#[10]  {genre=R&B,loudness=8}                                   => {popularityGroup=5} 0.015869682 0.5650970  2.138780 2040 

#Solo se muestran las 10 primeras reglas pero por lo general vemos que la popularidad esta bastante ligada al
#genero, ya que en la mayoria de los casos en la lhs aparecen como factor para determinar la popularidad.
