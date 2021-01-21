library(GA)

mapa <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/mapa.csv', 
                 header = FALSE, sep = ";")

funcaoAjuste <- function(x) {
  distancia = 0
  for (i in 1:(length(mapa) - 1)) {
    cidade1 = x[i]
    cidade2 = x[i + 1]
    distancia = distancia + mapa[cidade1, cidade2]
  }
  return(-distancia)
}

numeroGeracoes <- 20
tamanhoPopulacao <- 100

resultado <- ga(type = "permutation", fitness = funcaoAjuste, popSize = tamanhoPopulacao, 
                lower = c(1, 1, 1, 1, 1), upper = c(5, 5, 5, 5, 5), monitor = TRUE,
                maxiter = numeroGeracoes, names = c("A", "B", "C", "D", "E"),
                elitism = 1, pcrossover = 0.8, pmutation = 0.05, optim = TRUE)

summary(resultado)
summary(resultado)$solution
plot(resultado)
