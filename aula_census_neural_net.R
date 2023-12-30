library(dplyr)
library(caret)
# library(e1071)
library(h2o)

h2o.init()


base <- read.csv("data/census.csv")

base <- base %>%
    # select(-c("X")) %>%
    mutate_if(is.character, as.factor)

base_levels <- list(
    sex = data.frame(
        l_sex = as.factor(levels(base$sex)),
        n_sex = as.factor(as.integer(as.factor(levels(base$sex))))
    ),
    workclass = data.frame(
        l_workclass = as.factor(levels(base$workclass)),
        n_workclass = as.factor(as.integer(as.factor(levels(base$workclass))))
    )
)


base <- base %>%
    # As variaveis categoricas que estão no formto de string serão categoricas,
    # mas com valores numericos
    mutate(sex = as.numeric(factor(as.integer(sex), labels = c(0, 1)))) %>%
    mutate(workclass = as.numeric(as.factor(as.integer(workclass)))) %>%
    # mutate(income = as.numeric(factor(as.integer(income), labels = c(0, 1)))) %>%
    mutate(education = as.numeric(as.factor(as.integer(education)))) %>%
    mutate(marital.status = as.numeric(as.factor(as.integer(marital.status)))) %>%
    mutate(occupation = as.numeric(as.factor(as.integer(occupation)))) %>%
    mutate(relationship = as.numeric(as.factor(as.integer(relationship)))) %>%
    mutate(race = as.numeric(as.factor(as.integer(race)))) %>%
    mutate(native.country = as.numeric(as.factor(as.integer(native.country))))
# Escalonando as variaveis numericas
# mutate_at(
#     c(
#         "age",
#         "final.weight",
#         "education.num",
#         "capital.gain",
#         "capital.loos",
#         "hour.per.week"
#     ),
#     scale
# )

# criar base de teste e de treinamento
porcentagem_treinamento <- 0.85

base_treinamento <- base %>%
    sample_frac(porcentagem_treinamento)

base_teste <- base %>%
    anti_join(base_treinamento, by = "X")

base_treinamento$X <- NULL
base_teste$X <- NULL


classificador <- h2o.deeplearning(
    y = "income",
    training_frame = as.h2o(base_treinamento),
    activation = "Rectifier",
    hidden = c(100, 100),
    epochs = 1000
)

previsao <- h2o.predict(classificador, newdata = as.h2o(base_teste[-15]))

matriz_confusao <- table(base_teste$income, as.vector(previsao$predict))
cf <- confusionMatrix(matriz_confusao)

fourfoldplot(as.table(cf), color = c("green", "red"), main = "Confusion Matrix")
