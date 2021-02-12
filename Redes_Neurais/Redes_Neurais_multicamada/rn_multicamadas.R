
# Funcao de ativacao sigmoide
sigmoide <- function(soma) {
  return(1 / (1 + base::exp(-soma)))
}

# Funcao derivada da sigmoide
sigmoideDerivada <- function(sig) {
  return(sig * (1 - sig))
}

# Dados 
entradas <- base::matrix(c(0,0,1,1,1,0,1,1), nrow = 4,
                         ncol = 2, byrow = TRUE)
saidas <- base::matrix(c(0,1,1,0))
pesos0 <- base::matrix(c(stats::rnorm(1), stats::rnorm(1),
                         stats::rnorm(1), stats::rnorm(1),
                         stats::rnorm(1), stats::rnorm(1)),
                      nrow = 2, ncol = 3, byrow = TRUE)
pesos1 <- base::matrix(c(stats::rnorm(1), stats::rnorm(1), stats::rnorm(1)),
                       nrow = 3, ncol = 1, byrow = TRUE)
epocas <- 1000000
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
                      
