library(dplyr)
library(arules)

# base <- read.csv("data/mercado.csv", header = FALSE)
base <- read.transactions(
    "data/mercado_dois.csv",
    sep = ",",
    rm.duplicates = TRUE
)

# itemFrequencyPlot(base, topN = 7)

#numero de vezes na semana
num_sem <- 3 * 7
suport_cal <- num_sem / nrow(base)

regras <- base %>%
    apriori(parameter = list(
        support = suport_cal,
        confidence = 0.3
    ))

inspect(sort(regras, by = "lift")[1:20])
