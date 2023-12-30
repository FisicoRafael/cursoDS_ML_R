library(dplyr)
library(e1071)
library(caret)

base <- read.csv("data/credit_data.csv")

base <- base %>%
    select(-c("clientid"))

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
    mutate(default = as.factor(default))

controle_treinamento <- trainControl(
    method = "cv",
    number = 10
)

modelo <- train(
    default ~ .,
    data = base,
    trControl = controle_treinamento,
    method = "glm"
)
