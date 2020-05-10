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