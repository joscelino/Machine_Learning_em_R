library(caTools)
library(e1071)
library(caret)

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

# Classificador
classificador <- svm(formula = default ~., data = base_treinamento, 
                     type = 'C-classification', kernel = 'radial', cost = 6.0)

# Previsoes
previsoes <- predict(classificador, newdata = base_teste[-4])

# Matriz de confusao
matriz_confusao <- table(base_teste[,4], previsoes)
print(matriz_confusao)
confusionMatrix(matriz_confusao)
