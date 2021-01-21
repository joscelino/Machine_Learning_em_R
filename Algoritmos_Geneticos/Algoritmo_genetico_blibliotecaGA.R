library(RMySQL)
library(GA)

" Criando classes de produtos "
setClass(
  "Produto",
  slots = c(
    nome = "character",
    espaco = "numeric",
    valor = "numeric"
  )
)

" Conectando ao banco de dados e criacao do data frame "
conexao <- dbConnect(MySQL(), 
                     user = "", 
                     password = "", 
                     dbname = "produtos",
                     host = "localhost")
dataFrameProdutos <- dbGetQuery(conexao, 
                                "SELECT nome, espaco, valor, quantidade from produtos")
dbDisconnect(conexao)

" Algoritmo Genetico "
listaProdutos <- c()
for (i in 1:nrow(dataFrameProdutos)){
  for (j in 1:dataFrameProdutos[i, 4]) {
    listaProdutos <- c(listaProdutos, new("Produto",
                                          nome = dataFrameProdutos[i, 1],
                                          espaco = dataFrameProdutos[i, 2],
                                          valor = dataFrameProdutos[i, 3]))
  }
}

espacos <- c()
valores <- c()
nomes <- c()

for (produto in listaProdutos) {
  espacos = c(espacos, produto@espaco)
  valores = c(valores, produto@valor)
  nomes = c(nomes, produto@nome)
}

limite <- 10

" Funcao de avaliacao "
avaliacao <- function(cromossomo) {
  nota = 0
  somaEspaco = 0
  for (i in 1:length(cromossomo)){
    if (cromossomo[i] == '1'){
      nota = nota + valores[i]
      somaEspaco = somaEspaco + espacos[i]
    }
  }
  if (somaEspaco > limite) {
    nota = 1
  }
  return(nota)
}

" Criando o algoritmo genetico "
tamanhoPopulacao <- 20
numeroGeracoes <- 100
ag <- ga(type = "binary", fitness = avaliacao, nBits = length(listaProdutos),
         population = gabin_Population, selection = gabin_rwSelection,
         crossover = gabin_spCrossover, mutation = gabin_raMutation,
         popSize = tamanhoPopulacao, pcrossover = 0.75, pmutation = 0.05, 
         elitism = 0.05, maxiter = numeroGeracoes, keepBest = TRUE)

summary(ag)
plot(ag)

for (i in 1:length(listaProdutos)) {
  if (ag@solution[i] == '1') {
    cat("\n Produto: ", nomes[i], " - Valor (R$) : ", valores[i])
  }
}
