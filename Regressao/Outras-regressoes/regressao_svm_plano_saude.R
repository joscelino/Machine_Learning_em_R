library(readr)
library(tibble)
library(dplyr)
library(ggplot2)
library(plotly)
library(patchwork)
library(miscTools)
library(e1071)

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
base <- read_csv('D:/Projetos_em_R/Machine_Learning/Dados/plano_saude2.csv')

# Criacao do modelo
regressor <- e1071::svm(formula = custo ~ ., data = base,
                        type = "eps-regression",
                        kernel = "radial")

# Previsoes
previsoes <- stats::predict(regressor, newdata = base[-2])
cc <- miscTools::rSquared(base[['custo']], resid = base[['custo']] - previsoes)

# Grafico
grafico <- ggplot2::ggplot() + 
  ggplot2::geom_point(aes(x = base$idade, y = base$custo), colour = 'blue') +
  ggplot2::geom_line(aes(x = base$idade, y = previsoes), colour = 'red') +
  xlab("idade") + ylab("custo") +
  ggplot2::theme_classic()

plotly::ggplotly(grafico)

# Previsao a partir de um dado de entrada
idade_a_prever <- 40
df <- base::data.frame(idade = c(idade_a_prever))
previsao <- stats::predict(regressor, newdata = df)
print(previsao)


