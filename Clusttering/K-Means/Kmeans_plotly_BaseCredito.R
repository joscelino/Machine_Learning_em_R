library(readr)
library(dplyr)
library(plotly)

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
dados <- read_csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_card_clients.csv")

funcaoNA(dados)

dados$BILL_TOTAL <- dados$BILL_AMT1 + dados$BILL_AMT2 + dados$BILL_AMT3 +
  dados$BILL_AMT4 + dados$BILL_AMT5 + dados$BILL_AMT6
dados$ID <- base::factor(dados$ID)
dados$SEX <- base::factor(dados$SEX)
dados$EDUCATION <- base::factor(dados$EDUCATION)
dados$MARRIAGE <- base::factor(dados$MARRIAGE)
dados$`default payment next month` <- base::factor(dados$`default payment next month`)
dplyr::glimpse(dados)

# Base para modelo de 2 atributos
baseKmeans <- base::data.frame(limite = dados$LIMIT_BAL , gasto = dados$BILL_TOTAL)
baseKmeans <- base::scale(baseKmeans)

# Elbow method
base::set.seed(1)
wcss <- base::vector()

for (i in 1:10) {
  # Gerando o modelo
  kmeans <- stats::kmeans(x = baseKmeans, centers = i)
  wcss[i] <- base::sum(kmeans$withinss)
}

graphics::plot(1:10, wcss, type = 'b', xlab = 'Clusters', ylab = 'WCSS')

# Gerando o modelo para 2 atributos
set.seed(1)
kmeans <- stats::kmeans(x = baseKmeans, centers = 5)
previsoes <- kmeans$cluster

# Gerando o grafico interativo
df <- data.frame(baseKmeans, previsoes)
glimpse(df)
graf <- plot_ly(data = df, x=~limite, y=~gasto,
                color = ~previsoes)
graf


#Base de dados para mais de 2 atributos
baseKmeans2 <- base::data.frame(limite = dados$LIMIT_BAL,
                                gasto = dados$BILL_TOTAL,
                                educacao = dados$EDUCATION,
                                civil = dados$MARRIAGE,
                                idade = dados$AGE)

# Normalizacao dos dados
baseKmeans2$educacao <- base::as.numeric(baseKmeans2$educacao)
baseKmeans2$civil <- base::as.numeric(baseKmeans2$civil)
baseKmeans2 <- base::scale(baseKmeans2)

# Elbow method
base::set.seed(1)
wcss2 <- base::vector()

for (i in 1:10) {
  # Gerando o modelo
  kmeans2 <- stats::kmeans(x = baseKmeans2, centers = i)
  wcss2[i] <- base::sum(kmeans$withinss)
}

graphics::plot(1:10, wcss, type = 'b', xlab = 'Clusters', ylab = 'WCSS2')

# Gerando o modelo para 2 atributos
set.seed(1)
kmeans2 <- stats::kmeans(x = baseKmeans2, centers = 4)
previsoes2 <- kmeans2$cluster

# Gerando o grafico interativo
df2 <- data.frame(baseKmeans2, previsoes2)
glimpse(df2)
graf1 <- plot_ly(data = df2, x = ~limite, y = ~gasto,
                color = ~previsoes2)
graf1

pairs(baseKmeans2, col = c(1:4)[previsoes])
