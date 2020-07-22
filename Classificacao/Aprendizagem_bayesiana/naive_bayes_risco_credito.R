library(e1071)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/risco_credito.csv')
classificador <- naiveBayes(x = base[-5], y = base$risco)
print(classificador)

# historia: boa, divida: alta, garantias: nenhuma, renda > 35
# historia: ruim, divida: alta, garantias: adequada, renda < 15

# Criando Dataframe
historia <- c('ruim')
divida <- c('alta')
garantias <- c('nenhuma')
renda <- c('>15')
df <- data.frame(historia, divida, garantias, renda)

# Encode da classe
base$risco <- factor(base$risco, levels = c(0, 1, 2))

# Previsao
previsao <- predict(classificador, newdata = df, 'raw')
print(previsao)
