library(caTools)
library(miscTools)
library(rpart)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/house_prices.csv')
base$id <- NULL
base$date <- NULL
base$sqft_living15 <- NULL
base$sqft_lot15 <- NULL
base$sqft_basement <- NULL

set.seed(1)
divisao <- sample.split(base$price, SplitRatio = 0.70)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

regressor <- rpart(formula = price ~., data = base_treinamento)
summary(regressor)

previsoes_treinamento <- predict(regressor, newdata = base_treinamento[-1])
cc_treinamento <- rSquared(base_treinamento[['price']], 
                           resid = base_treinamento[['price']] - previsoes_treinamento)

previsoes_teste <- predict(regressor, newdata = base_teste[-1])
cc_teste <- rSquared(base_teste[['price']], 
                           resid = base_teste[['price']] - previsoes_teste)
mean_absolute_error <- mean(abs(base_teste[['price']] -  previsoes_teste))
print(mean_absolute_error)
