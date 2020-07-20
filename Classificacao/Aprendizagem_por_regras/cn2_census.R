library(RoughSets)
library(caTools)
library(caret)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/census.csv')
base$X = NULL
summary(base)

set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.05)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

# Convertendo os dados para a estrutura da biblioteca
dt_treinamento <- SF.asDecisionTable(dataset = base_treinamento, decision.attr = 15)
dt_teste <- SF.asDecisionTable(dataset = base_teste, decision.attr = 15)

# Transformando dados numericos em categoricos (necessidade da biblioteca)
intervalos <- D.discretization.RST(dt_treinamento, nOfIntervals = 4)
dt_treinamento <- SF.applyDecTable(dt_treinamento, intervalos)
dt_teste <- SF.applyDecTable(dt_teste, intervalos)

# Treinamento
classificador <- RI.CN2Rules.RST(dt_treinamento, K = 5)
print(classificador)

# Previsoes
previsoes <- predict(classificador, newdata = dt_teste[-15])
View(previsoes)

# Matriz de confusao
matriz_confusao <- table(dt_teste[, 15], unlist(previsoes))
View(matriz_confusao)
print(matriz_confusao)
confusionMatrix(matriz_confusao)
