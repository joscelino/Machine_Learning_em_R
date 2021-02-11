# Dados de entrada para o Perceptron

entradas <- c(1, 7, 5)
pesos <- c(0.8, 0.1, 0)

# Funcao soma da Rede Neural
soma <- function (e, p) {
  return(e %*% p) # produto escalar
}

# Step function
stepFunction <- function(soma) {
  if (soma >= 1) {
    return(1)
  }
  return(0)
}

s <- soma(entradas, pesos)
r <- stepFunction(s)
