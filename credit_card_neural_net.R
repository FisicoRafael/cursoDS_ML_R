library(dplyr)
library(caret)
library(h2o)

h2o.init()

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
    mutate_at(vars(-matches("default")), scale) %>%
    mutate(default = as.factor(default)) %>%
    mutate_at(vars(-matches("default")), as.numeric) 

# criar base de teste e de treinamento
porcentagem_treinamento <- 0.75

base_treinamento <- base %>%
    sample_frac(porcentagem_treinamento)

base_teste <- base %>%
    anti_join(base_treinamento, by = "clientid")

base_treinamento$clientid <- NULL
base_teste$clientid <- NULL

classificador <- h2o.deeplearning(
    y = 'default',
    training_frame = as.h2o(base_treinamento),
    activation = 'Rectifier',
    hidden = c(3),
    epochs = 1000
)

previsao <- h2o.predict(classificador, newdata =  as.h2o(base_teste[-4]))
# previsao <- (previsao > 0.5)
# previsao <- as.vector(previsao)

matriz_confusao <- table(base_teste$default, as.vector(previsao$predict))
cf <- confusionMatrix(matriz_confusao)

fourfoldplot(as.table(cf), color = c("green", "red"), main = "Confusion Matrix")
