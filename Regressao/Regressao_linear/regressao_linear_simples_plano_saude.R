library('ggplot2')

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/plano_saude.csv')

# Analise de correlacao entre as variaveis
cor(base$idade, base$custo)

# Aprendizado
regressor <- lm(formula = custo ~ idade, data = base)

summary(regressor)

b0 <- regressor$coefficients[1]
b1 <- regressor$coefficients[2]

cr <- summary(regressor)$adj.r.squared

# Previsoes
previsoes <- predict(regressor, newdata = base[-2])

# Grafico

ggplot() + geom_point(aes(x = base$idade, y = base$custo), colour = 'blue') +
  geom_line(aes(x = base$idade, y = previsoes), colour = 'red') + 
  ggtitle('Idade x Custo') + xlab('Idade') + ylab('Custo')

# Previsao de novo valor
idade <- c(57)
df <- data.frame(idade)

# forma 1
previsao_1 <- predict(regressor, newdata = df)
previsao_2 <- b0 + b1 * idade
