library(readr)
library(randomForest)
library(tibble)
library(dplyr)
library(ggplot2)
library(plotly)
library(patchwork)
library(miscTools)

base <- read_csv('D:/Projetos_em_R/Machine_Learning/Dados/plano_saude2.csv')

# Criacao do modelo
regressor <- randomForest::randomForest(x = base[1], y = base$custo,
                                        ntree = 500)
summary(regressor)

# Previsoes
previsoes <- stats::predict(regressor, newdata = base[-2])
cc <- miscTools::rSquared(base[['custo']], resid = base[['custo']] - previsoes)

x_teste <- base::seq(min(base$idade), max(base$idade), 0.01)
previsoes2 <- stats::predict(regressor, newdata = data.frame(idade = x_teste))

# Grafico
grafico <- ggplot2::ggplot() + 
  ggplot2::geom_point(aes(x = base$idade, y = base$custo), colour = 'blue') +
  ggplot2::geom_line(aes(x = x_teste, y = previsoes2), colour = 'red') +
  xlab("idade") + ylab("custo") +
  ggplot2::theme_classic()

plotly::ggplotly(grafico)

# Previsao a partir de um dado de entrada
idade_a_prever <- 40
df <- base::data.frame(idade = c(idade_a_prever))
previsao <- stats::predict(regressor, newdata = df)
print(previsao)
