################################################################################
############################# REDE NEURAL DE UMA CAMADA ########################
################################################################################

## PRESSUPOSTOS
## Problemas linearmente separaveis

# Dados de entrada
entradas <- base::matrix(c(0,0,0,1,1,0,1,1), nrow = 4, ncol = 2, byrow = TRUE)
#saidas <- base::matrix(c(0,0,0,1)) #AND
saidas <- base::matrix(c(0,1,1,1)) #OR

pesos <- base::matrix(c(0.0,0.0))
taxaAprendizagem <- 0.1

# Step function
stepFunction <- function(soma) {
  if (soma >= 1) {
    return(1)
  }
  return(0)
}

# Calculando a saida
calculaSaida <- function(registro) {
  soma = registro %*% pesos
  return(stepFunction(soma))
}

# Perceptron de 1 camada
erroTotal = 1
while (erroTotal != 0) {
  erroTotal = 0
  for (i in 1:length(saidas)) {
    saidaCalculada = calculaSaida(c(entradas[i,]))
    erro = abs(saidas[i] - saidaCalculada)
    erroTotal = erroTotal + erro
    
    for (j in 1:length(pesos)) {
      pesos[j] = pesos[j] + (taxaAprendizagem * entradas[i,j] * erro)
      print(paste('Peso atualizado: ',pesos[j]))
    }
  }
  print(paste('Total de erros: ', erroTotal))
}

print("Rede Neural treinada!")
print(calculaSaida(c(entradas[1,])))
print(calculaSaida(c(entradas[2,])))
print(calculaSaida(c(entradas[3,])))
print(calculaSaida(c(entradas[4,])))
