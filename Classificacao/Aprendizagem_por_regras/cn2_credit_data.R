library(RoughSets)
library(caTools)
library(caret)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv')

base$clientid <- NULL
summary(base)

# Tratando dados inconsistentes
# Verificando idades negativas
idades_invalidas <- base[base$age < 0 & !is.na(base$age), ]

base$age <- ifelse(base$age < 0, mean(base$age[base$age > 0], na.rm = TRUE), base$age)

# Verificando valores faltantes
base[is.na(base$age), ]
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

# Separacao de bases de treinamento e testes
set.seed(1)

divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

# Convertendo os dados para a estrutura da biblioteca
dt_treinamento <- SF.asDecisionTable(dataset = base_treinamento, decision.attr = 4)
dt_teste <- SF.asDecisionTable(dataset = base_teste, decision.attr = 4)

# Transformando dados numericos em categoricos (necessidade da biblioteca)
intervalos <- D.discretization.RST(dt_treinamento, nOfIntervals = 4)
dt_treinamento <- SF.applyDecTable(dt_treinamento, intervalos)
dt_teste <- SF.applyDecTable(dt_teste, intervalos)

# Treinamento
classificador <- RI.CN2Rules.RST(dt_treinamento, K = 5)
print(classificador)

# Previsoes
previsoes <- predict(classificador, newdata = dt_teste[-4])
View(previsoes)

# Matriz de confusao
matriz_confusao <- table(dt_teste[, 4], unlist(previsoes))
View(matriz_confusao)
print(matriz_confusao)
confusionMatrix(matriz_confusao)
