
base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/risco_credito.csv')
base <- base[base$risco != "moderado",]

classificador = glm(as.factor(base$risco) ~ ., family = binomial, data = base)

# historia: boa, divida: alta, garantias: nenhuma, renda > 35
# historia: ruim, divida: alta, garantias: adequada, renda < 15

# Criando Dataframe
historia <- c('boa', 'ruim')
divida <- c('alta', 'alta')
garantias <- c('nenhuma', 'adequada')
renda <- c('acima_35', '0_15')
df <- data.frame(historia, divida, garantias, renda)

# previsoes de probabilidade
probabilidades <- predict(classificador, type = 'response', newdata = df)
resposta <- ifelse(probabilidades > 0.5, 'baixo', 'alto')
