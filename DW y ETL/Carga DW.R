#SCRIPT PARA CARGA DEL DATAWAREHOUSE
#BASE DE DATOS CREADA PREVIAMENTE: en este script creación de la tabla y carga de la misma a partir del datasetx

#Paquete para conexión con la base de datos MySQL
  install.packages("RMySQL", type="source")
  library("RMySQL")

#Conexión con el servicio php
  
  con <- dbConnect(RMySQL::MySQL(),
                    host="localhost",
                    user = "root",
                    password = "")
  
#Creación base de datos
  
  dbGetQuery(con, "CREATE DATABASE IF NOT EXISTS `mineriabd` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci;")
  dbGetQuery(con, "USE `mineriabd`")
  
#Renovamos conexión esta vez con la base de datos

  con <- dbConnect(RMySQL::MySQL(),
                   host="localhost",
                   dbname = "mineriabd",
                   user = "root",
                   password = "")
  
  
#CREACIÓN DE LAS TABLAS PARA EL DATAWAREHOUSE

#Tabla artista
  
  dbGetQuery(con, "CREATE TABLE `artista` (
    `artista` varchar(30) NOT NULL,
    `popularity` float NOT NULL
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;")
  
  dbGetQuery(con, "ALTER TABLE `artista` ADD PRIMARY KEY (`artista`);")
  
  dbGetQuery(con, "COMMIT;")
  
#Tabla canción

  dbGetQuery(con, "CREATE TABLE `cancion` (
    `track_id` varchar(30) NOT NULL,
    `track_name` varchar(40) NOT NULL,
    `artista` varchar(30) NOT NULL,
    `genero` varchar(15) NOT NULL,
    `popularity` int(4) NOT NULL,
    `acousticness` float NOT NULL,
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

  dbGetQuery(con, "ALTER TABLE `cancion` ADD PRIMARY KEY (`track_id`);")

  dbGetQuery(con, "COMMIT;")
  
#Relación entre tablas
  
  dbGetQuery(con, "ALTER TABLE `cancion`
  ADD CONSTRAINT `cancion_ibfk_1` FOREIGN KEY (`artista`) REFERENCES `artista` (`artista`) ON UPDATE CASCADE;")
  dbGetQuery(con, "COMMIT;")
  
############################################################################
#  EN ESTE PUNTO LAS TABLAS QUEDAN CREADAS Y CONFIGURADAS Y PASAMOS AL VOLCADO
#  DE INFORMACIÓN
############################################################################

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
  
  SpFeatures$artist_name <- str_to_lower(SpFeatures$artist_name, locale = "es")

#Extraemos información de los artistas en dataframe a parte
  
  artistas <- SpFeatures %>%
    group_by(artist_name) %>%
    summarize(mean(popularity))

#Renombramos
  
  names(artistas)[2]<-"popularity"
  
  library(stringr)
  
#Pasamos a minusculas y eliminamos duplicados
  
  artistas$artist_name <- str_to_lower(artistas$artist_name, locale = "es")
  
  artistas <- artistas[!duplicated(artistas$artist_name),]
  
#Carga de la información en la tabla
  
  j<-1
  nrow(artistas)
  for(j in 1:nrow(artistas)){
    D <- artistas[j,]
    
    artista <- D$artist_name
    popularidad <- D$popularity
    query <- paste("INSERT INTO artista (
  artista,
  popularity)
  VALUES ('",artista,"','",popularidad,"')", sep="")
    
    dbGetQuery(con, query)
  }
  
#Carga de la tabla canción
  
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
  acousticness,
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

#Para comprobar que se han introducido todas las filas viendo cuantas se han introducido y comparamos visualmente con las filas de los dataframe

  dbGetQuery(con, "SELECT count(track_id) FROM cancion")
  
  dbGetQuery(con, "SELECT count(artista) FROM artista")
