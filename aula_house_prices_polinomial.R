library(dplyr)
# library(caret)
library(ggplot2)

base <- read.csv("data/house_prices.csv")

col_tirar <- c(
    "date",
    "sqft_lot15",
    "sqft_living15",
    "sqft_basement"
)

outcome <- "price"

base <- base %>%
    select(-all_of(col_tirar))

vars_predictors <- base %>%
    select(-all_of(c(outcome, "id"))) %>%
    names()

for (pre in vars_predictors) {
    for (i in 2:5) {
        base <- base %>%
            mutate(!!paste0(pre, "_", i) := bedrooms^i)
    }
}

# criar base de teste e de treinamento
porcentagem_treinamento <- 0.70

base_treinamento <- base %>%
    sample_frac(porcentagem_treinamento)

base_teste <- base %>%
    anti_join(base_treinamento, by = "id")

base_treinamento$id <- NULL
base_teste$id <- NULL
base$id <- NULL



regressor <- glm(
    price ~ .,
    data = base_treinamento
)

previsoes_teste <- predict(regressor, base_teste %>% select(-c("price")))
