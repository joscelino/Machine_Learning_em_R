library(caTools)
library(e1071)
library(caret)

base <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/heart.csv")
glimpse(base)
summary(base)

# Tratando dados inconsistentes
# Verificando idades negativas
anyNA(base)
idades_invalidas <- base[base$ï..age < 0 & !is.na(base$base$ï..age), ]

base$base$ï..age <- ifelse(base$age < 0, mean(base$ï..age[base$ï..age > 0], 
                                      na.rm = TRUE), base$base$ï..age)

# Verificando valores faltantes
base[is.na(base$ï..age), ]

# Escalonamento (normalizacao) dos dados
base[, 1:13] <- scale(base[, 1:13])

# Separacao de bases de treinamento e testes
set.seed(1)


divisao <- sample.split(base$target, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_treinamento$base <- NULL
base_teste <- subset(base, divisao == FALSE)
base_teste$base <- NULL

# Classificador
classificador <- svm(formula = target ~., data = base_treinamento, 
                     type = 'C-classification', kernel = 'radial', cost = 6.0)

# Previsoes
previsoes <- predict(classificador, newdata = base_teste[-14])

# Matriz de confusao
matriz_confusao <- table(base_teste[,14], previsoes)
print(matriz_confusao)
confusionMatrix(matriz_confusao)
