library(readr)
library(tibble)
library(arules)

# Mineracao dos dados
dados <- read.csv2("D:/Projetos_em_R/Machine_Learning/Dados/mercado1.csv",
                   header = FALSE, sep = ",")

# Pre-visualizacao dos dados
tibble::glimpse(dados)

# Criacao do modelo
transacoes <- arules::read.transactions("D:/Projetos_em_R/Machine_Learning/Dados/mercado1.csv",
                                   sep = ",", rm.duplicates = TRUE)
summary(transacoes)

# Grafico
arules::itemFrequencyPlot(transacoes, topN = 7)

# Regras
regras <- arules::apriori(data = transacoes, 
                          parameter = list(support = 0.3, confidence = 0.8))

# Visualizando as regras geradas
arules::inspect(arules::sort(regras, by = "lift"))

