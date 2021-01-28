library(arules)

####### Exemplo 1
transacoes <- arules::read.transactions("D:/Projetos_em_R/Machine_Learning/Dados/mercado1.csv",
                                        sep = ",", rm.duplicates = TRUE)
summary(transacoes)

# Grafico
arules::itemFrequencyPlot(transacoes, topN = 7)

# Numero minimo de produtos na composicao da transacao
numero_produtos <- 2

# Criando os itemsets
itemsets <- arules::eclat(data = transacoes, parameter = list(support = 0.3,
                                                              minlen = numero_produtos))

# Visualizando as regras geradas
arules::inspect(arules::sort(itemsets, by = "support"))



####### Exemplo 2
transacoes2 <- arules::read.transactions("D:/Projetos_em_R/Machine_Learning/Dados/mercado2.csv",
                                        sep = ",", rm.duplicates = TRUE)
summary(transacoes2)

# Grafico
arules::itemFrequencyPlot(transacoes2, topN = 7)

# Numero minimo de produtos na composicao da transacao
numero_produtos2 <- 3

# Criando os itemsets
itemsets2 <- arules::eclat(data = transacoes2, parameter = list(support = 0.003,
                                                              minlen = numero_produtos2))

# Visualizando as regras geradas
arules::inspect(arules::sort(itemsets2, by = "support")[1:10])
