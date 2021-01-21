library(pacman)
pacman::p_load(caTools, readr, tibble, miscTools)

base <- read_csv('D:/Projetos_em_R/Machine_Learning/Dados/house_prices.csv')
base$id <- as.factor(base$id)
base$bedrooms <- as.factor(base$bedrooms)
base$bathrooms <- as.factor(base$bathrooms)
base$floors <- as.factor(base$floors)
base
set.seed(1)

divisao <- sample.split(base$price, SplitRatio = 0.7)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao = FALSE)

cor(base$sqft_living, base$price)

# Aprendizado
regressor <- lm(formula = price ~ sqft_living, data = base_treinamento)
summary(regressor)

previsoes_treinamento <- predict(regressor, newdata = base_treinamento[6])

# Grafico
pacman::p_load(ggplot2, plotly)
grafico <- ggplot() + geom_point(aes(x = base_treinamento$sqft_living, y = base_treinamento$price),
                      colour = 'blue') + geom_line(aes(base_treinamento$sqft_living,
                                                       y = previsoes_treinamento),
                                                   colour = 'red') +
  ylab("Precos - Base treinamento") + xlab("Sqrt living")
plotly::ggplotly(grafico)

# Previsoes
previsoes_teste <- predict(regressor, newdata = base_teste[6])

# Calculo do erro
resultado <- abs(base_teste[3] - previsoes_teste)

erro_medio <- mean(resultado[['price']])

# Calculo residual base de teste
cr <- rSquared(base_teste[['price']], resid = base_teste[['price']] - previsoes_teste)
