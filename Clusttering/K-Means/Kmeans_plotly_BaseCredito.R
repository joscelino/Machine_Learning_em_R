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
dados$ID <- base::factor(dados$ID)

funcaoNA(dados)

dados$BILL_TOTAL <- dados$BILL_AMT1 + dados$BILL_AMT2 + dados$BILL_AMT3 +
  dados$BILL_AMT4 + dados$BILL_AMT5 + dados$BILL_AMT6
dados$SEX <- base::factor(dados$SEX)
dados$EDUCATION <- base::factor(dados$EDUCATION)
dados$MARRIAGE <- base::factor(dados$MARRIAGE)
dados$`default payment next month` <- base::factor(dados$`default payment next month`)
dplyr::glimpse(dados)

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

# Gerando o modelo
set.seed(1)
kmeans <- stats::kmeans(x = baseKmeans, centers = 5)
previsoes <- kmeans$cluster

# Gerando o grafico interativo
df <- data.frame(baseKmeans, previsoes)
glimpse(df)
graf <- plot_ly(data = df, x=~limite, y=~gasto,
                color = ~previsoes)
graf
