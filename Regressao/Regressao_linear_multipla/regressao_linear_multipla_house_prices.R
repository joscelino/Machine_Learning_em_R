library('ggplot2')
library('caTools')
library('miscTools')

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/house_prices.csv')
base$id <- NULL
base$date <- NULL
base$sqft_living15 <- NULL
base$sqft_lot15 <- NULL
base$sqft_basement <- NULL

set.seed(1)
divisao <- sample.split(base$price, SplitRatio = 0.7)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao = FALSE)

# Aprendizado
regressor <- lm(formula = price ~., data = base_treinamento)
summary(regressor)

previsoes <- predict(regressor, newdata = base_teste[-1])

# Calculo do erro
mae <- mean(abs(base_teste[['price']] - previsoes))
print(mae)

# Calculo residual 
cr <- rSquared(base_teste[['price']], resid = base_teste[['price']] - previsoes)
