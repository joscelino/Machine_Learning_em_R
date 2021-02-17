library(readr)
library(tibble)
library(arules)
library(arulesViz)

# Mineracao dos dados
dados <- read.csv2("D:/Projetos_em_R/Machine_Learning/Dados/mercado2.csv",
                   header = FALSE, sep = ",")

# Pre-visualizacao dos dados
tibble::glimpse(dados)

# Criacao do modelo
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

# Visualizacao Scatter
plot(regras, method = "scatter", engine = "htmlwidget", max = 300)

# Visualizacao Grafo
plot(regras, method = "graph", engine = "htmlwidget", max = 300)

# Convertendo as regras para data frame
df <- as(regras, "data.frame")
View(df)
