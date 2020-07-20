library(caTools)
library(caret)
library(h2o)


base <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv")
base$clientid <- NULL
summary(base)

# Tratando dados inconsistentes
# Verificando idades negativas
idades_invalidas <- base[base$age < 0 & !is.na(base$age), ]

base$age <- ifelse(base$age < 0, mean(base$age[base$age > 0], na.rm = TRUE), base$age)

# Verificando valores faltantes
base[is.na(base$age), ]
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

# Escalonamento (normalizacao) dos dados
base[, 1:3] <- scale(base[, 1:3])

# Separacao de bases de treinamento e testes
set.seed(1)

divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

h2o.init(nthreads = -1)
# Classificador
classificador <- h2o.deeplearning(y = 'default', 
                                  training_frame = as.h2o(base_treinamento),
                                  activation = 'Rectifier',
                                  hidden = c(100, 80),  epochs = 1000)
# Previsoes
previsoes <- h2o.predict(classificador, newdata = as.h2o(base_teste[-4]))
previsoes <- previsoes >= 0.5
previsoes <- as.vector(previsoes)

# Matriz de confusao
matriz_confusao <- table(base_teste[,4], previsoes)
confusionMatrix(matriz_confusao)
