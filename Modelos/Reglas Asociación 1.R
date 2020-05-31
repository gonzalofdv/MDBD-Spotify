library("RMySQL")

set.seed(28768)

con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")

dataRules<-dbGetQuery(con, "SELECT * FROM cancion")

#Cargamos el paquete arules para poder utilizar las reglas

if (!require(arules)) {
  install.packages("arules",
                   dependencies = TRUE)}
require(arules)

#Eliminamos la columna track id que no nos va a aportar informacion
dataRules$track_id <- NULL

#metemos nuestra base de datos como argumento de la funcion apriori para sacar las reglas
rules.all <- apriori(dataRules)
rules.all

#comprobamos los resultados obtenidos
inspect(rules.all)

#No son muy satisfactorios, asi que procedemos a hacer un estudio de las correlaciones:
cor(dataNum)
m
round(m, 2)

#comparamos la correlacion con los resultados obtenidos de la funcion apriori para sacar las conclusiones
inspect(rules.all)
summary(data)
rules.all
