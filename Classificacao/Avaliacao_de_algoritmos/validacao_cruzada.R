library(caTools)
library(caret)

base <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv")
base$clientid <- NULL
idades_invalidas <- base[base$age < 0 & !is.na(base$age), ]
base$age <- ifelse(base$age < 0, mean(base$age[base$age > 0], na.rm = TRUE), base$age)
base[is.na(base$age), ]
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)
base[, 1:3] <- scale(base[, 1:3])
base$default <- factor(base$default, levels = c(0, 1))


# Validacao cruzada -> retornando apenas 1 resultado
controle_treinamento <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
modelo <- train(default ~.,data = base, trControl = controle_treinamento, 
               method = 'svmRadial')
print(modelo)

precisao <- modelo$results$Accuracy[2]
