library(pacman)
pacman::p_load(dplyr, readr, tibble, car, lmtest, ggpubr)

## Modelo de Regressa Linear Simples
# Pressupostos:
# 1. Relacao linear entre as variaveis
# 2. Normalidade dos dados
# 3. Homocedasticidade
# 4. Ausencia de outliers
# 5. Independencia dos residuos

# Funcao de Teste de Normalidade dos Dados

testesNormalidade <- function(x) {
  
  " Verificando a suposicao de normalidade atraves de testes estatisticos "
  " se p-valor < 0.05, rejeita-se a hipotese nula (no caso, normalidade) "
  " Na comparacao de 2 grupos independentes, a suposicao de normalidade somente
  sera aceita quando houver normalidade em ambos os grupos (dados serao parametricos)"
  
  library(nortest)
  
  # Kolmogorov-Smirnov
  t1 <- ks.test(x, "pnorm")
  # Teste de Shapiro-Wilk
  t2 <- shapiro.test(x)
  # Teste de Shapiro-Francia
  t3 <- sf.test(x)
  # Teste Anderson-Darling
  t4 <- ad.test(x)
  # Teste Lilliefors
  t5 <- lillie.test(x)
  # Teste Cramer-Von Mises
  t6 <- cvm.test(x)
  # Teste Qui-quadrado Pearson
  t7 <- pearson.test(x)
  
  # Teste JarqueBera 
  library(tsoutliers)
  t8 <- JarqueBera.test(x)
  
  testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method, t6$method, 
              t7$method, t8[[1]]$method)
  estatisticas <- c(t1$statistic, t2$statistic, t3$statistic, t4$statistic, 
                    t5$statistic, t6$statistic, t7$statistic, t8[[1]]$statistic)
  estatisticas <- as.numeric(estatisticas)
  pvalues <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value, 
               t6$p.value, t7$p.value, t8[[1]]$p.value)
  pvalues <- as.numeric(pvalues)
  
  # Matriz de resultados
  estatisticas <- round(estatisticas,4)
  pvalues <- round(pvalues,4)
  
  resultados <- data.frame(testes, estatisticas, pvalues)
  
  # Densidades
  library(ggpubr)

  a <- ggdensity(x)
  b <- ggqqplot(x)
  print(a)
  print(b)

  return(resultados)
}


# Importacao dos Dados
dados <- read_csv2("D:/Projetos_em_R/Machine_Learning/Dados/dadosRegressaoPublicidade.csv")
glimpse(dados)

# Verificacao dos pressupostos
# Relacao linear entre as variaveis
cor(dados$Publicidade,dados$Vendas)
plot(dados$Publicidade,dados$Vendas)

# Construcao do Modelo
modelo <- lm(Vendas ~ Publicidade, dados)

# Analise Grafica (Normalidade, Outliers e Homocedasticidade dos residuos)
par(mfrow=c(2,2))
plot(modelo)
par(mfrow=c(1,1))

# Normalidade dos residuos
testesNormalidade(modelo$residuals)

# Outliers nos residuos
summary(rstandard(modelo))

# Independencia dos residuos (Durbin-Watson)
# Estatistica deve estar proxima de 2
# H0: autocorrelacao 0 - H1: autocorrelacao diferente de 0
durbinWatsonTest(modelo)

# Homocedasticidade (Breusch-Pagan)
# Este teste nao funciona com residuos nao-normais
bptest(modelo)

# Analise do Modelo
summary(modelo)

# Grafico de Dispersao
pacman::p_load(ggplot2, ggpubr)

grafico <- ggplot(data = dados, mapping = aes(x = Publicidade, y =  Vendas)) +
  geom_point() + geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    label.x = 0, label.y = 400
  ) +
  theme_bw()

