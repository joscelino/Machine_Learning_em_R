library(OneR)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/risco_credito.csv')

# Treinamento:

classificador <- OneR(x = base)
print(classificador)


# historia: boa, divida: alta, garantias: nenhuma, renda > 35
# historia: ruim, divida: alta, garantias: adequada, renda < 15

# Criando Dataframe
historia <- c('boa', 'ruim')
divida <- c('alta', 'alta')
garantias <- c('nenhuma', 'adequada')
renda <- c('acima_35', '0_15')
df <- data.frame(historia, divida, garantias, renda)
View(df)

# Previsoes
previsoes <- predict(classificador, newdata = df)
View(previsoes)
