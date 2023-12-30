library(dplyr)
library(e1071)
library(psych)

base <- read.csv("data/risco_credito.csv")

classificador <- naiveBayes(base$risco ~ .,
    data = base
)

# historia: boa, divida: alta, garantias: nenhuma, renda :35

historia <- c("ruim")
divida <- c("alta")
garantias <- c("nenhuma")
renda <- c("acima_35")

df <- data.frame(historia, divida, garantias, renda)

previsao <- predict(classificador, df)
