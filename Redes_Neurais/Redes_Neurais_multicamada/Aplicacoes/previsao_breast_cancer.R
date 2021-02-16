library(tibble)
# Funcao de ativacao sigmoide
sigmoide <- function(soma) {
  return(1 / (1 + base::exp(-soma)))
}

# Funcao derivada da sigmoide
sigmoideDerivada <- function(sig) {
  return(sig * (1 - sig))
}

# Dados 
entradas <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/entradas.csv")
tibble::glimpse(entradas)
entradas <- data.matrix(entradas)

saidas <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/saidas.csv")
saidas <- data.matrix(saidas)

pesos0 <- base::matrix(stats::runif(ncol(entradas) * 3, min = -1, max = 1),
                       nrow = ncol(entradas), ncol = 3, byrow = TRUE)
pesos1 <- base::matrix(stats::runif(3, min = -1, max = 1),
                       nrow = 3, ncol = 1, byrow = TRUE)
epocas <- 10
momento <- 1
taxaAprendizagem <- 0.3

# Rede neural multi-camadas
for (j in 1:epocas) {
  camadaEntrada = entradas
  somaSinapse = camadaEntrada %*% pesos0
  camadaOculta = sigmoide(somaSinapse)
  
  somaSinapse1 = camadaOculta %*% pesos1
  camadaSaida = sigmoide(somaSinapse1)
  
  erroCamadaSaida = saidas - camadaSaida
  mediaAbsoluta = base::mean(abs(erroCamadaSaida))
  print(paste('Erro: ',mediaAbsoluta))
  
  derivadaSaida = sigmoideDerivada(camadaSaida)
  deltaSaida = erroCamadaSaida * derivadaSaida
  
  pesos1Transposta = t(pesos1)
  deltaSaidaXPeso = deltaSaida %*% pesos1Transposta
  deltaCamadaOculta = deltaSaidaXPeso * sigmoideDerivada(camadaOculta)
  
  camadaOcultaTransposta = t(camadaOculta)
  pesosNovo1 = camadaOcultaTransposta %*% deltaSaida
  pesos1 = (pesos1 * momento) + (pesosNovo1 * taxaAprendizagem)
  
  camadaEntradaTransposta = t(camadaEntrada)
  pesosNovo0 = camadaEntradaTransposta %*% deltaCamadaOculta
  pesos0 = (pesos0 * momento) + (pesosNovo0 * taxaAprendizagem)
  
}

