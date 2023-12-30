library(dplyr)
# library(caret)
library(ggplot2)

base <- read.csv("data/house_prices.csv")

# criar base de teste e de treinamento
porcentagem_treinamento <- 0.70

base_treinamento <- base %>%
    sample_frac(porcentagem_treinamento)

base_teste <- base %>%
    anti_join(base_treinamento, by = "id")

regressor <- lm(
    price ~ sqft_living,
    data = base_treinamento
)

previsoes_treinamento <- predict(regressor, base_treinamento["sqft_living"])

grafico <- base_treinamento %>%
    ggplot() +
    geom_point(
        aes(x = sqft_living, y = price),
        colour = "blue"
    ) +
    geom_line(
        aes(x = sqft_living, y = previsoes_treinamento),
        colour = "red"
    )

plot(grafico)

previsoes_teste <- predict(regressor, base_teste["sqft_living"])
