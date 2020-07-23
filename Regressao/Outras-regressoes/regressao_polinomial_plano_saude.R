library(ggplot2)

base <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/plano_saude2.csv')

# Regressao Linear Simples
regressor1 <- lm(formula = custo ~ idade, data = base)
cc1 <- summary(regressor1)$r.squared

# Previsao para dado unico
idade <- c(40)
df <- data.frame(idade)

previsao1 <- predict(regressor1, newdata = df)
print(previsao1)

# Grafico regressao linear simpleas
ggplot() + geom_point(aes(x = base$idade, y = base$custo), colour = 'blue') +
  geom_line(aes(x = base$idade, y = predict(regressor1, newdata = base[-2])), 
            colour = 'red')

# Regressao Polinomial
base2 <- base
# Tranformacaoes na base de dados
base2$idade2 <- base2$idade^2
base2$idade3 <- base2$idade^3

# Regressao Linear polinomial
regressor2 <- lm(formula = custo ~ ., data = base2)
cc2 <- summary(regressor2)$r.squared

# Nova data frame
idade2 <- c(idade^2)
idade3 <- c(idade^3)
df2 <- data.frame(idade, idade2, idade3)
previsao2 <- predict(regressor2, newdata = df2)
print(previsao2)

# Grafico regressao linear simpleas
ggplot() + geom_point(aes(x = base$idade, y = base$custo), colour = 'blue') +
  geom_line(aes(x = base$idade, y = predict(regressor2, newdata = base2[-2])), 
            colour = 'red')