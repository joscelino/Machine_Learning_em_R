base <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv")
base$clientid <- NULL
idades_invalidas <- base[base$age < 0 & !is.na(base$age), ]
base$age <- ifelse(base$age < 0, mean(base$age[base$age > 0], na.rm = TRUE), base$age)
base[is.na(base$age), ]
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)
base[, 1:3] <- scale(base[, 1:3])
base$default <- factor(base$default, levels = c(0, 1))

# carregando os classificadores selecionados
rfFinal <- readRDS('rfFinal.rds')
rnaFinal <- readRDS('rnaFinal.rds')


previsoesrf <- predict(rfFinal, newdata = base[1, -4])
previsoesrf <- as.numeric(trimws(previsoesrf))

previsoesrna <- predict(rnaFinal, newdata = as.h2o(base[1, -4]))
#previsoesrna <- previsoesrna[1]
previsoesrna <- as.numeric(as.vector(previsoesrna))

# Combinacao dos classificadores
classe_0 <- 0
classe_1 <- 0

if(previsoesrf == 1){
  classe_1 <- classe_1 + 1
} else {
  classe_0 <- classe_0 + 1
}

if(previsoesrna == 1){
  classe_1 <- classe_1 + 1
} else {
  classe_0 <- classe_0 + 1
}

if(classe_0 > classe_1){
  print('classe 0')
}else if (classe_0 == classe_1){
  print('Empate')
}else{
  print('classe 1')
}