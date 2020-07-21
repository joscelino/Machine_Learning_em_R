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


# Validacao cruzada 30 testes

resultados30 <- c()

for (i in 1:30){
  controle_treinamento <- trainControl(method = 'cv', number = 10)
  modelo <- train(default ~.,data = base, trControl = controle_treinamento, 
                  method = 'svmRadial')
  precisao <- max(modelo$results$Accuracy)
  print(precisao)
  resultados30 <- c(resultados30, precisao)
}

for (i in 1:30){
  cat(gsub('[.]', ',', resultados30[i]))
  cat('\n')
}