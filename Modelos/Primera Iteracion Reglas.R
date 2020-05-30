library("RMySQL")

set.seed(28768)

con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")

dataRules<-dbGetQuery(con, "SELECT * FROM cancion")


if (!require(arules)) {
  install.packages("arules",
                   dependencies = TRUE)}
require(arules)

rules.all <- apriori(dataRules)
dataRules$track_id <- NULL
rules.all <- apriori(dataRules)
rules.all
inspect
inspect(rules.all)
cor(dataNum)
m
round(m, 2)
inspect(rules.all)
summary(data)
rules.all