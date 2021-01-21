### Algoritmo genetico valor real

library(GA)

funcaoAjuste <- function(x){
  
  #Equacao 2 * x + 4 = 20
  resultado <- 2 * x + 4
  igualdade = 20
  
  if (resultado > igualdade) {
    return(igualdade - resultado)
  }
  else
    return(resultado - igualdade)
}

numeroGeracoes <- 10
tamanhoPopulacao <- 100000

resultado <- ga(type = "real-value", fitness = funcaoAjuste, monitor = TRUE,
                lower = round(c(-20),0), upper = round(c(20),0),  
                popSize = tamanhoPopulacao, maxiter = numeroGeracoes,
                elitism = 2, pcrossover = 0.75, pmutation = 0.05, optim = TRUE)

summary(resultado)
summary(resultado)$solution
plot(resultado)
