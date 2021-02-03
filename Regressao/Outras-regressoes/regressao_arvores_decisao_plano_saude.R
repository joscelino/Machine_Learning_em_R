library(ggplot2)
library(rpart)
library(plotly)
library(miscTools)
library(gridExtra)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/plano_saude2.csv')
regressor <- rpart(formula = custo ~ idade, data = base, 
                   control = rpart.control(minsplit = 1))
summary(regressor)

previsoes <- predict(regressor, newdata = base[-2])
cc <- rSquared(base[['custo']], resid = base[['custo']] - previsoes)
print(cc)

# Grafico das previsoes com poucos dados
grafico_1 <- ggplot() + geom_point(aes(x = base$idade, y =  base$custo), 
                                   colour = 'blue') +
  geom_line(aes(x = base$idade, y = previsoes), colour = 'red') + 
  xlab('Idade') + ylab('Custo')


ggplotly(grafico_1)


# Grafico das previsoes com geracao de dados dummy para melhor visualizacao dos splits
x_teste <- seq(min(base$idade), max(base$idade), 0.1)
previsoes2 <- predict(regressor, newdata = data.frame(idade = x_teste))
grafico_2 <- ggplot() + geom_point(aes(x = base$idade, y =  base$custo), 
                                   colour = 'blue') +
  geom_line(aes(x = x_teste, y = previsoes2), colour = 'red') + 
  xlab('Idade') + ylab('Custo')


ggplotly(grafico_2)

# GRID
grid.arrange(grafico_1,
             grafico_2,
             nrow = 1, ncol = 2)

# Previsao para valor unico
df <- data.frame(idade = c(40))
previsao <- predict(regressor, newdata = df)
