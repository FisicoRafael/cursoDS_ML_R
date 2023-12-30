library(dplyr)
library(rpart)
library(rpart.plot)

base <- read.csv("data/risco_credito.csv")

classificador <- rpart(risco ~ .,
    data = base, control = rpart.control(minbucket = 1)
)

plot(classificador)
text(classificador)

rpart.plot(classificador)

# historia: boa, divida: alta, garantias: nenhuma, renda :35

historia <- c("boa", "ruim")
divida <- c("alta", "alta")
garantias <- c("nenhuma", "adequada")
renda <- c("acima_35", "0_15")

df <- data.frame(historia, divida, garantias, renda)

previsao <- predict(classificador, df)
previsao
