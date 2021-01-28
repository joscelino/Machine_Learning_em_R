library(e1071)
library(caTools)
library(caret)

# Funcao para identificar NAs em colunas de data frames
funcaoNA <- function(df){
  
  library(pacman)
  pacman::p_load(dplyr)
  
  index_col_na <- NULL
  quantidade_na <- NULL
  
  for (i in 1:ncol(df)) {
    if(sum(is.na(df[,i])) > 0) {
      index_col_na[i] <- i
      quantidade_na[i] <- sum(is.na(df[,i]))
    }
  }
  resultados <- data.frame(index_col_na,quantidade_na)
  resultados <- resultados %>% filter(quantidade_na>0)
  return(resultados)
}

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv')
base$clientid <- NULL
summary(base)

# verificando NAs
funcaoNA(base)
dplyr::glimpse(base)

# Tratando dados inconsistentes
# Verificando idades negativas
idades_invalidas <- base[base$age < 0 & !is.na(base$age), ]

base$age <- ifelse(base$age < 0, mean(base$age[base$age > 0], na.rm = TRUE), base$age)

# Verificando valores faltantes
base[is.na(base$age), ]
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

# Escalonamento (normalizacao) dos dados
base[, 1:3] <- scale(base[, 1:3])

# Encode da classe
base$default <- factor(base$default, levels = c(0, 1))

# Separacao de bases de treinamento e testes
set.seed(1)

divisao <- caTools::sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

# PCA
pca <- caret::preProcess(x = base_treinamento[-4], method = "pca",
                         pcaComp = 2)

# Transformacao das bases de dados
base_treinamento <- stats::predict(pca, base_treinamento)
base_teste <- stats::predict(pca, base_teste)

# Treinamento
classificador <- e1071::naiveBayes(x = base_treinamento[-1], 
                                   base_treinamento$default)
print(classificador)

# Previsoes
previsoes <- predict(classificador, newdata = base_teste[-1])

# matriz de confusao
matriz_confusao <- table(base_teste[ ,1], previsoes)
print(matriz_confusao)

confusionMatrix(matriz_confusao)
