library(dplyr)
library(e1071)
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
    mutate_at(vars(-matches("default")), scale)

# criar base de teste e de treinamento
porcentagem_treinamento <- 0.75

base_treinamento <- base %>%
    sample_frac(porcentagem_treinamento)

base_teste <- base %>%
    anti_join(base_treinamento, by = "clientid")

base_treinamento$clientid <- NULL
base_teste$clientid <- NULL

classificador <- naiveBayes(base_treinamento$default ~ .,
    data = base_treinamento
)

previsao <- predict(classificador, base_teste)

# matriz de confusao

matriz_confusao <- table(base_teste$default, previsao)
confusionMatrix(matriz_confusao)