library(readr)
library(tibble)
library(arules)

dados <- read.csv2("D:/Projetos_em_R/Machine_Learning/Dados/mercado2.csv",
                   header = FALSE, sep = ",")

tibble::glimpse(dados)
transacoes <- arules::read.transactions("D:/Projetos_em_R/Machine_Learning/Dados/mercado2.csv",
                                        sep = ",", rm.duplicates = TRUE)
summary(transacoes)

# Grafico
arules::itemFrequencyPlot(transacoes, topN = 7)

# Regras
regras <- arules::apriori(data = transacoes, 
                          parameter = list(support = 0.03, confidence = 0.2))
# Visualizando as regras geradas
arules::inspect(arules::sort(regras, by = "lift"))
