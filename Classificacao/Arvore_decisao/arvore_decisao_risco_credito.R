library(rpart)
library(rpart.plot)

base <- read.csv('risco_credito.csv')

classificador <- rpart(formula = risco ~ ., data = base, 
                       control = rpart.control(minbucket = 1))
print(classificador)
rpart.plot(classificador)

# historia: boa, divida: alta, garantias: nenhuma, renda > 35
# historia: ruim, divida: alta, garantias: adequada, renda < 15

# Criando Dataframe
historia <- c('boa', 'ruim')
divida <- c('alta', 'alta')
garantias <- c('nenhuma', 'adequada')
renda <- c('acima_35', '0_15')
df <- data.frame(historia, divida, garantias, renda)

# Encode da classe
base$risco <- factor(base$risco, levels = c(0, 1, 2))

# Previsao
previsao <- predict(classificador, newdata = df)
print(previsao)
