library(dplyr)
library(arules)

# base <- read.csv("data/mercado.csv", header = FALSE)
base <- read.transactions("data/mercado.csv", sep = ",", rm.duplicates = TRUE)

# itemFrequencyPlot(base, topN = 7)s

regras <- base %>%
    apriori(parameter = list(
        support = 0.3,
        confidence = 0.8
    ))

inspect(sort(regras, by = "lift"))
