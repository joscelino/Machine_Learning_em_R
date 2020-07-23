library(caTools)
library(miscTools)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/house_prices.csv')
base$id <- NULL
base$date <- NULL
base$sqft_living15 <- NULL
base$sqft_lot15 <- NULL
base$sqft_basement <- NULL

base$bedrooms2 <- base$bedrooms^2
base$bedrooms3 <- base$bedrooms^3
base$bedrooms4 <- base$bedrooms^4
base$bedrooms5 <- base$bedrooms^5

base$bathrooms2 <- base$bathrooms^2
base$bathrooms3 <- base$bathrooms^3
base$bathrooms4 <- base$bathrooms^4
base$bathrooms5 <- base$bathrooms^5

base$sqft_living2 <- base$sqft_living^2
base$sqft_living3 <- base$sqft_living^3
base$sqft_living4 <- base$sqft_living^4
base$sqft_living5 <- base$sqft_living^5

base$sqft_lot2 <- base$sqft_lot^2
base$sqft_lot3 <- base$sqft_lot^3
base$sqft_lot4 <- base$sqft_lot^4
base$sqft_lot5 <- base$sqft_lot^5

base$floors2 <- base$floors^2
base$floors3 <- base$floors^3
base$floors4 <- base$floors^4
base$floors5 <- base$floors^5

base$waterfront2 <- base$waterfront^2
base$waterfront3 <- base$waterfront^3
base$waterfront4 <- base$waterfront^4
base$waterfront5 <- base$waterfront^5

base$view2 <- base$view^2
base$view3 <- base$view^3
base$view4 <- base$view^4
base$view5 <- base$view^5

base$condition2 <- base$condition^2
base$condition3 <- base$condition^3
base$condition4 <- base$condition^4
base$condition5 <- base$condition^5

base$grade2 <- base$grade^2
base$grade3 <- base$grade^3
base$grade4 <- base$grade^4
base$grade5 <- base$grade^5

base$sqft_above2 <- base$sqft_above^2
base$sqft_above3 <- base$sqft_above^3
base$sqft_above4 <- base$sqft_above^4
base$sqft_above5 <- base$sqft_above^5

base$yr_built2 <- base$yr_built^2
base$yr_built3 <- base$yr_built^3
base$yr_built4 <- base$yr_built^4
base$yr_built5 <- base$yr_built^5

base$yr_renovated2 <- base$yr_renovated^2
base$yr_renovated3 <- base$yr_renovated^3
base$yr_renovated4 <- base$yr_renovated^4
base$yr_renovated5 <- base$yr_renovated^5

base$zipcode2 <- base$zipcode^2
base$zipcode3 <- base$zipcode^3
base$zipcode4 <- base$zipcode^4
base$zipcode5 <- base$zipcode^5

base$lat2 <- base$lat^2
base$lat3 <- base$lat^3
base$lat4 <- base$lat^4
base$lat5 <- base$lat^5

base$long2 <- base$long^2
base$long3 <- base$long^3
base$long4 <- base$long^4
base$long5 <- base$long^5

set.seed(1)
divisao <- sample.split(base$price, SplitRatio = 0.70)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

regressor <- lm(formula = price ~ ., data = base_treinamento)
summary(regressor)

previsoes <- predict(regressor, newdata = base_teste[-1])
mean(abs(base_teste[['price']] - previsoes))

cc <- rSquared(base_teste[['price']], resid = base_teste[['price']] - previsoes)
