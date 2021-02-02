library(dplyr)
library(cluster)

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

# Base de dados
base <- iris
funcaoNA(base)
baseKmeans <- iris[1:2]

# Gerando o modelo
base::set.seed(1)
kmeans <- stats::kmeans(x = baseKmeans, centers = 3)
previsoes <- kmeans$cluster

# Gerando o grafico
cluster::clusplot(baseKmeans, previsoes, color = TRUE, lines = 0,
                  shade = TRUE)

# Verificando o agrupamento
base::table(base$Species, previsoes)
