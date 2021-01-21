" APLICACAO: CARREGANDO CAMINHAO COM PRODUTOS COM OBJETIVO DE ATINGIR MAIOR 
FATURAMENTO, OBEDECENDO A CAPACIDADE VOLUMETRICA DE CARGA "
"
Referencia: https://sites.icmc.usp.br/andre/research/genetic/
"

" Criando classes de produtos "
setClass(
  "Produto",
  slots = c(
    nome = "character",
    espaco = "numeric",
    valor = "numeric"
  )
)

" Criando a classe de individuos "
setClass(
  "Individuo",
  slots = c(
    espacos = "numeric",
    valores = "numeric",
    limiteEspacos = "numeric",
    notaAvaliacao = "numeric",
    espacoUtilizado = "numeric",
    geracao = "numeric",
    cromossomo = "character"
  ),
  # Inicializando os atributos
  prototype = list(
    espacos = 0,
    valores = 0,
    limiteEspacos = 0,
    notaAvaliacao = 0,
    espacoUtilizado = 0,
    geracao = 0
  )
)

" Funcao de geracao do cromossomo "
gerarCromossomo <- function(tamanhoCromossomo){
  cromossomo = sample(x = c("0", "1"), size = tamanhoCromossomo, replace = TRUE)
  return(cromossomo)
}

" Funcao de avaliacao (fitness) "
avaliacao <- function(individuo){
  nota = 0
  somaEspacos = 0
  for (i in 1:length(individuo@cromossomo)) {
    if (individuo@cromossomo[i] == "1") {
      nota = nota + individuo@valores[i]
      somaEspacos = somaEspacos + individuo@espacos[i]
    }
  }
  if (somaEspacos > individuo@limiteEspacos) {
    nota = 1
  }
  individuo@notaAvaliacao = nota
  individuo@espacoUtilizado = somaEspacos
  return(individuo)
}

" Funcao crossover (reproducao) de um ponto "
crossover <- function(individuoA, individuoB) {
  indices = 1:length(individuoA@cromossomo)
  corte = sample(indices, 1) 
  cat("\nPonto de corte: ", corte)
  if (corte == length(individuoA@cromossomo)) {
    filho1 = individuoB@cromossomo[1:corte]
    filho2 = individuoA@cromossomo[1:corte]
  }else {
    filho1 = c(individuoB@cromossomo[1:corte], 
               individuoA@cromossomo[(corte + 1):length(individuoA@cromossomo)])
    filho2 = c(individuoA@cromossomo[1:corte], 
               individuoB@cromossomo[(corte + 1):length(individuoB@cromossomo)])
  }
  filhos = list(
    new("Individuo", espacos = individuoA@espacos, valores = individuoA@valores,
        limiteEspacos = individuoA@limiteEspacos, geracao = individuoA@geracao + 1),
    new("Individuo", espacos = individuoB@espacos, valores = individuoB@valores,
        limiteEspacos = individuoB@limiteEspacos, geracao = individuoB@geracao + 1)
    )
  
  filhos[[1]]@cromossomo <- filho1
  filhos[[2]]@cromossomo <- filho2
  return(filhos)
}

" Funcao de mutacao"
mutacao <- function(individuo, taxaMutacao) {
  for (i in 1:length(individuo@cromossomo)) {
    if (runif(n = 1, min = 0, max = 1) < taxaMutacao) {
      if (individuo@cromossomo[i] == '1') {
        individuo@cromossomo[i] = '0'
      }else {
        individuo@cromossomo[i] = '1'
      }
    }
  }
  return(individuo)
}

" Inicializacao da Populacao "
# Armazenando as solucoes
setClass(
  "algoritmoGenetico",
  slots = c(
    tamanhoPopulacao = "numeric",
    populacao = "list",
    geracao = "numeric",
    melhorSolucao = "Individuo",
    listaSolucoes = "list"
  )
)

inicializaPopulacao <- function(algoritmoGenetico, espacos, valores, limite) {
  for (i in 1:algoritmoGenetico@tamanhoPopulacao) {
    algoritmoGenetico@populacao[[i]] = new("Individuo", espacos = espacos, 
                                           valores = valores, 
                                           limiteEspacos = limite)
    algoritmoGenetico@populacao[[i]]@cromossomo = gerarCromossomo(length(espacos))
  }
  return(algoritmoGenetico)
}

" Ordenacao da Populacao "
ordenaPopulacao <- function(populacao) {
  populacaoOrdenada = c()
  notasAvaliacao = c()
  for (individuo in populacao) {
    notasAvaliacao = c(notasAvaliacao, individuo@notaAvaliacao)
  }
  listaPosicao = order(notasAvaliacao, decreasing = TRUE)
  for (i in 1:length(listaPosicao)) {
    populacaoOrdenada = c(populacaoOrdenada, populacao[[listaPosicao[i]]])
  }
  return(populacaoOrdenada)
}

" Funcao para definir o melhor individuo"
melhorIndividuo <- function(algoritmoGenetico, individuo) {
  algoritmoGenetico@listaSolucoes = c(algoritmoGenetico@listaSolucoes, 
                                      individuo@notaAvaliacao)
  if (individuo@notaAvaliacao > algoritmoGenetico@melhorSolucao@notaAvaliacao) {
    algoritmoGenetico@melhorSolucao = individuo
  }
  return(algoritmoGenetico)
}

" Funcao para somar as avaliacoes "
somaAvaliacoes <- function(algoritmoGenetico) {
  soma = 0
  for (individuo in algoritmoGenetico@populacao) {
    soma = soma + individuo@notaAvaliacao
  }
  return(soma)
}

" Metodo da Roleta (Roda do Acaso) "
selecionaPai <- function(algoritmoGenetico, somaAvaliacoes) {
  pai = 0
  valorSorteado =  runif(1, 0, 1) * somaAvaliacoes
  soma = 0
  i = 1
  while (i < length(algoritmoGenetico@populacao) && soma < valorSorteado) {
    soma = soma + algoritmoGenetico@populacao[[i]]@notaAvaliacao
    pai = pai + 1
    i = i + 1
  }
  return(pai)
}

" Metodo de visualizacao de geracoes de individuos "
visualizaGeracao <- function(algoritmoGenetico) {
  melhor = algoritmoGenetico@populacao[[1]]
  cat("\nG: ", melhor@geracao, " Valor: ", melhor@notaAvaliacao,
      " Espaco: ", melhor@espacoUtilizado, " Cromossomo: ", melhor@cromossomo)
}

" Metodo para solucao do problema "
resolver <- function(algoritmoGenetico, taxaMutacao, numeroGeracoes, espacos,
                     valores, limiteEspaco) {
  ag = algoritmoGenetico
  ag = inicializaPopulacao(algoritmoGenetico = ag, espacos = espacos, 
                           valores = valores, limite = limiteEspaco)
  for (i in 1:ag@tamanhoPopulacao) {
    ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
  }
  ag@populacao = ordenaPopulacao(ag@populacao)
  ag@melhorSolucao = ag@populacao[[1]]
  visualizaGeracao(algoritmoGenetico = ag)
  
  " Evolucao das populacoes "
  for (geracao in 1:numeroGeracoes) {
    soma = somaAvaliacoes(algoritmoGenetico = ag)
    
    " Gerando novos individuos "
    novaPopulacao = c()
    for (individuosGerados in 1:(ag@tamanhoPopulacao / 2 )) {
      pai1 = selecionaPai(algoritmoGenetico = ag, somaAvaliacoes = soma)
      pai2 = selecionaPai(algoritmoGenetico = ag, somaAvaliacoes = soma)
      
      filhos = crossover(individuoA = ag@populacao[[pai1]], 
                         individuoB = ag@populacao[[pai2]])
      novaPopulacao = c(novaPopulacao, mutacao(individuo = filhos[[1]], 
                                               taxaMutacao = taxaMutacao))
      novaPopulacao = c(novaPopulacao, mutacao(individuo = filhos[[2]], 
                                               taxaMutacao = taxaMutacao))
    }
    ag@populacao = novaPopulacao
    
    " Nova avaliacao "
    for (i in 1:ag@tamanhoPopulacao) {
      ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
    }
    ag@populacao = ordenaPopulacao(ag@populacao)
    visualizaGeracao(algoritmoGenetico = ag)
    ag = melhorIndividuo(algoritmoGenetico = ag, individuo = ag@populacao[[1]])
  }
  
  cat("\n\nMelhor solucao - G: ", ag@melhorSolucao@geracao, " Valor: ", 
      ag@melhorSolucao@notaAvaliacao, " Espaco: ", ag@melhorSolucao@espacoUtilizado,
      " Cromossomo: ", ag@melhorSolucao@cromossomo)
  return(ag)
}

" Criando objetos de produtos "
listaProdutos <- c(new("Produto", nome = "Geladeira Dako", espaco = 0.751, valor = 999.90),
                  new("Produto", nome = "Iphone 6", espaco = 0.0000899, valor = 2911.12),
                  new("Produto", nome = "TV 55' ", espaco = 0.430, valor = 4346.99),
                  new("Produto", nome = "TV 50' ", espaco = 0.450, valor = 3999.90),
                  new("Produto", nome = "TV 42' ", espaco = 0.458, valor = 2999.00),
                  new("Produto", nome = "Notebook Dell", espaco = 0.350, valor = 2499.90),
                  new("Produto", nome = "Ventilador Panasonic", espaco = 0.496, valor = 199.90),
                  new("Produto", nome = "Microondas Electrolux", espaco = 0.444, valor = 429.90),
                  new("Produto", nome = "Microondas Panasonic", espaco = 0.439, valor = 299.29),
                  new("Produto", nome = "Geladeira Brastemp", espaco = 0.895, valor = 849.00),
                  new("Produto", nome = "Geladeira Consul", espaco = 0.870, valor = 1199.89),
                  new("Produto", nome = "Notebook Lenovo", espaco = 0.378, valor = 1999.90),
                  new("Produto", nome = "Notebook Asus", espaco = 0.337, valor = 3999.00)
)

espacos <- c()
valores <- c()
nomes <- c()

for (produto in listaProdutos) {
  espacos = c(espacos, produto@espaco)
  valores = c(valores, produto@valor)
  nomes = c(nomes, produto@nome)
}

" Definindo a capacidade do caminhao "
limite <- 3

" Definindo o numero de individuos da populacao "
tamanho <- 20

" Definindo a probabilidade de Mutacao "
probabilidadeMutacao <- 0.05

" Definindo o numero de geracoes "
numeroGeracoes <- 3000

" Inicializando o Algoritmo Genetico "
ag <- new("algoritmoGenetico", tamanhoPopulacao = tamanho)
ag <- resolver(algoritmoGenetico = ag, taxaMutacao = probabilidadeMutacao,
               numeroGeracoes = numeroGeracoes, espacos = espacos,
               valores = valores, limiteEspaco = limite)

" Listando os produtos a serem carregados "
cat("\n\nLista de itens para carregamento")
for (i in 1:length(listaProdutos)) {
  if (ag@melhorSolucao@cromossomo[i] == '1') {
    cat("\nNome: ", nomes[i], " - Valor em R$ : ", valores[i])
  }
}

plot(
  x = 1:numeroGeracoes,
  y = ag@listaSolucoes,
  type = "l",
  main = "Grafico dos resultados",
  col = "red",
  ylab = "Valores (R$)",
  xlab = "Geracoes"
)


