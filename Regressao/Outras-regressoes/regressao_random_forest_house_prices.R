library(readr)
library(randomForest)
library(tibble)
library(dplyr)
library(ggplot2)
library(plotly)
library(patchwork)
library(miscTools)
library(caTools)
library(miscTools)
library(rpart)

# Funcao para identificar NAs em colunas de data frames
funcaoNA <- function(df){
  
  library(pacman)
  pacman::p_load(dplyr, tibble)
  
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

# Mineracao dos dados
base <- read_csv('D:/Projetos_em_R/Machine_Learning/Dados/house_prices.csv')
funcaoNA(base)
base$id <- NULL
base$date <- NULL
base$sqft_living15 <- NULL
base$sqft_lot15 <- NULL
base$sqft_basement <- NULL

# Divisao da base de dados
set.seed(1)
divisao <- caTools::sample.split(base$price, SplitRatio = 0.70)
base_treinamento <- base::subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

# Criacao do modelo
regressor <- randomForest::randomForest(x = base_treinamento[2:16],
                                        y = base_treinamento$price,
                                        ntree = 250)
summary(regressor)

# Previsoes
previsoes_treinamento <- stats::predict(regressor, 
                                        newdata = base_treinamento[-1])
cc_treinamento <- miscTools::rSquared(base_treinamento[['price']],
                                      resid = base_treinamento[['price']] - 
                                        previsoes_treinamento)

previsoes_teste <- stats::predict(regressor, newdata = base_teste[-1])

# Erro medio
base::mean(abs(base_teste[['price']] - previsoes_teste))

cc_teste <- miscTools::rSquared(base_teste[['price']],
                                resid = base_teste[['price']] - previsoes_teste)
