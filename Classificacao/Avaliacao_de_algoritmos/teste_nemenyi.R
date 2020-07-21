require(TStools)
library(tsutils)

dados <- read.csv('dados_testes.csv')
matriz <- as.matrix(dados)
View(matriz)

# Tste de Nemenyi
nemenyi(matriz, conf.level = 0.95,plottype="vline")


