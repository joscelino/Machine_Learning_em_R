library(pacman)
pacman::p_load(dplyr, readr, tibble, rstatix, ggplot2, car, plotly,
               lmtest, QuantPsyc, psych, scatterplot3d, RColorBrewer)

# Importacao dos dados
dados <- read_csv2("D:/Projetos_em_R/Modelos_Estatisticos/Analise_Exploratoria/Dados/BancoDeDados12.csv")

# PRESSUPOSTOS
# 1. RELACAO LINEAR ENTRE VARIAVEIS
# 2. NORMALIDADE DOS DADOS
# 3. HOMOCEDASTICIDADE DOS DADOS
# 4. AUSENCIA DE OUTLIERS
# 5. INDEPENDENCIA DOS RESIDUOS
# 6. AUSENCIA DE MULTICOLINEARIDADE

# TESTES DE NORMALIDADE

testesNormalidade <- function(x) {
  
  " Verificando a suposicao de normalidade atraves de testes estatisticos "
  " se p-valor < 0.05, rejeita-se a hipotese nula (no caso, normalidade) "
  " Na comparacao de 2 grupos independentes, a suposicao de normalidade somente
  sera aceita quando houver normalidade em ambos os grupos (dados serao parametricos)"
  
  library(nortest)
  
  # Teste de Shapiro-Wilk
  t1 <- shapiro.test(x)
  # Teste de Shapiro-Francia
  t2 <- sf.test(x)
  # Teste Anderson-Darling
  t3 <- ad.test(x)
  # Teste Lilliefors
  t4 <- lillie.test(x)
  # Teste Cramer-Von Mises
  t5 <- cvm.test(x)
  # Teste Qui-quadrado Pearson
  t6 <- pearson.test(x)
  
  # Teste JarqueBera 
  library(tsoutliers)
  t7 <- JarqueBera.test(x)
  
  testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method, t6$method, 
              t7[[1]]$method)
  estatisticas <- c(t1$statistic, t2$statistic, t3$statistic, t4$statistic, 
                    t5$statistic, t6$statistic, t7[[1]]$statistic)
  estatisticas <- as.numeric(estatisticas)
  pvalues <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value, 
               t6$p.value, t7[[1]]$p.value)
  pvalues <- as.numeric(pvalues)
  
  # Matriz de resultados
  estatisticas <- round(estatisticas,4)
  pvalues <- round(pvalues,4)
  
  resultados <- data.frame(testes, estatisticas, pvalues)
  
  return(resultados)
}

# Funcao para identificar NAs em colunas de data frames
funcaoNA <- function(df){
  
  library(pacman)
  pacman::p_load(dplyr, tibble)
  
  index_col_na <- NULL
  quantidade_na <- NULL
  
  for (i in 1:ncol(df)) {
    if(sum(is.na(df[,i])) > 0) {
      index_col_na[i] <- i
      quantidade_na[i] <- sum(is.na(df[,i]))
    }
  }
  resultados <- data.frame(index_col_na,quantidade_na)
  resultados <- resultados %>% filter(quantidade_na>0)
  
  return(resultados)
}

# Verificando NAs
funcaoNA(dados)

# Visualizacao dos dados
dados
tibble::glimpse(dados)

# Construcao do Modelo
modelo <- stats::lm(Notas ~ Tempo_Rev + Tempo_Sono, dados)

residuos <- base::as.data.frame(modelo$residuals) 
base::names(residuos) <- c("residuos")
residuos <- tibble::as_tibble(residuos)

# Analise Grafica
graphics::par(mfrow=c(2,2))
graphics::plot(modelo)
graphics::par(mfrow=c(1,1))

# Verificando a normalidade dos residuos
testesNormalidade(modelo$residuals)

# Outliers nos residuos

graphics::boxplot(modelo$residuals, 
                  col = RColorBrewer::brewer.pal(n = 9, name = "RdBu"))
rstatix::identify_outliers(residuos)
base::summary(stats::rstandard(modelo))

# Homocedasticidade (breusch-Pagan)
lmtest::bptest(modelo)

# Ausencia de Multicolinearidade
psych::pairs.panels(dados) # Multicolinearidade: r> 0.9 (ou 0.8)

car::vif(modelo) # Multicolinearidade: VIF > 10

# Criacao do segundo modelo
modelo2 <- stats::lm(Notas ~ Tempo_Rev, dados)

# Analise dos modelos
base::summary(modelo)
base::summary(modelo2)

# Obtencao dos coeficientes padronizados
QuantPsyc::lm.beta(modelo)
QuantPsyc::lm.beta(modelo2)

# Obtencao do IC 95% para os coeficientes
stats::confint(modelo)
stats::confint(modelo2)

