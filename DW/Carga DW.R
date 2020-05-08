#SCRIPT PARA CARGA DEL DATAWAREHOUSE
#BASE DE DATOS CREADA PREVIAMENTE: en este script creación de la tabla y carga de la misma a partir del datasetx

#Paquete para conexión con la base de datos MySQL
  install.packages("RMySQL", type="source")
  library("RMySQL")


#Conexión con la base de datos con la base ya creada previamente

  con <- dbConnect(RMySQL::MySQL(),
                   host="localhost",
                   dbname = "mineriabd",
                   user = "root",
                   password = "")

#CREACIÓN DE LAS TABLAS PARA EL DATAWAREHOUSE

#Tabla canción

  dbGetQuery(con, "CREATE TABLE `cancion` (
    `track_id` varchar(30) NOT NULL,
    `track_name` varchar(40) NOT NULL,
    `artista` varchar(30) NOT NULL,
    `genero` varchar(15) NOT NULL,
    `popularity` int(4) NOT NULL,
    `accousticness` float NOT NULL,
    `danceability` float NOT NULL,
    `duration` float NOT NULL,
    `energy` float NOT NULL,
    `instrumentalness` float NOT NULL,
    `clave` varchar(2) NOT NULL,
    `liveness` float NOT NULL,
    `loudness` float NOT NULL,
    `mode` varchar(6) NOT NULL,
    `speechiness` float NOT NULL,
    `tempo` float NOT NULL,
    `time_signature` varchar(4) NOT NULL,
    `valence` float NOT NULL
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;")

#Clave primaria

  dbGetQuery(con, "ALTER TABLE `cancion` ADD PRIMARY KEY (`track_id`);")

  dbGetQuery(con, "COMMIT;")

#Lectura del CSV a partir del cual llenamos el DW
  
  SpFeatures <- read.csv("SpotifyFeatures.csv", encoding = "UTF-8")
  
#Renombrar columna a genre:
  
  names(SpFeatures)[1]<-"genre"
  
#Eliminamos los paréntesis y el texto entre ellos a los titulos
  
  SpFeatures$track_name <- gsub("\\s*\\([^\\)]+\\)","",as.character(SpFeatures$track_name))
                        
#Eliminamos los generos Movie, Anime, Comedy, Soundtrack y World:
  
  SpFeatures <- SpFeatures[!(SpFeatures$genre == "Movie"),]
  SpFeatures <- SpFeatures[!(SpFeatures$genre == "Anime"),]
  SpFeatures <- SpFeatures[!(SpFeatures$genre == "A Capella"),]
  SpFeatures <- SpFeatures[!(SpFeatures$genre == "Comedy"),]
  SpFeatures <- SpFeatures[!(SpFeatures$genre == "Soundtrack"),]
  SpFeatures <- SpFeatures[!(SpFeatures$genre == "World"),]
  SpFeatures <- SpFeatures[!(SpFeatures$genre == "Children's Music"),]

  
  SpFeatures$artist_name <- gsub("'", "", SpFeatures$artist_name)
  SpFeatures$track_name <- gsub("'", "", SpFeatures$track_name)
  SpFeatures$genre <- gsub("'", "", SpFeatures$genre)
  #Paquete dplyr
  
  install.packages('dplyr')
  library('dplyr')
  
#Eliminamos duplicados
  
  SpFeatures <- SpFeatures[!duplicated(SpFeatures$track_id),]

  
i <- 1 
nrow(SpFeatures)
for(i in 1:nrow(SpFeatures)){
  D<-SpFeatures[i,]
  
  id <- D$track_id
  name <- D$track_name
  artist <- D$artist_name
  genre <- D$genre
  pop <- D$popularity
  ac <- D$acousticness
  dan <- D$danceability
  dur <- D$duration_ms
  en <- D$energy
  ins <- D$instrumentalness
  k <- D$key
  liv <- D$liveness
  loud <- D$loudness
  m <- D$mode
  speech <- D$speechiness
  temp <- D$tempo
  time <- D$time_signature
  val <- D$valence
  
  query <- paste("INSERT INTO cancion (
  track_id,
  track_name,
  artista,
  genero,
  popularity,
  accousticness,
  danceability,
  duration,
  energy,
  instrumentalness,
  clave,
  liveness,
  loudness,
  mode,
  speechiness,
  tempo,
  time_signature,
  valence)
  VALUES ('",id,"','",name,"','",artist,"','",genre,"','",pop,"','",ac,"','",dan,"','",dur,"','",en,"','",ins,"','",k,"','",liv,"','",loud,"','",m,"','",speech,"','",temp,"','",time,"','",val,"')", sep="")
  
  dbGetQuery(con, query)
}

#Para comprobar que se han introducido todas las filas

  dbGetQuery(con, "SELECT count(track_id) FROM cancion")
