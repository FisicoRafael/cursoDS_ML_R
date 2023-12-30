library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)


base <- read.csv("data/credit_data.csv")

# base <- base %>%
#     select(-c("clientid"))

media_idade <- base %>%
    filter(age > 0) %>%
    pull(age) %>%
    mean()

base <- base %>%
    mutate(age = replace(
        age, which(age < 0 | is.na(age)),
        media_idade
    )) %>%
    mutate(default = as.factor(default))

# criar base de teste e de treinamento
porcentagem_treinamento <- 0.75

base_treinamento <- base %>%
    sample_frac(porcentagem_treinamento)

base_teste <- base %>%
    anti_join(base_treinamento, by = "clientid")

base_treinamento$clientid <- NULL
base_teste$clientid <- NULL

classificador <- rpart(default ~ .,
    data = base_treinamento
)

rpart.plot(classificador)

previsao <- predict(classificador, base_teste, type = "class")

matriz_confusao <- table(base_teste$default, previsao)
cf <- confusionMatrix(matriz_confusao)

fourfoldplot(as.table(cf), color = c("green", "red"), main = "Confusion Matrix")
