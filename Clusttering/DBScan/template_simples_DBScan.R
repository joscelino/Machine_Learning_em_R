library(readr)
library(dbscan)

dados <- read_csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_card_clients.csv")
dados$ID <- base::factor(dados$ID)
dplyr::glimpse(dados)
dados$BILL_TOTAL <- dados$BILL_AMT1 + dados$BILL_AMT2 + dados$BILL_AMT3 +
                    dados$BILL_AMT4 + dados$BILL_AMT5 + dados$BILL_AMT6
x <- base::data.frame(limite = dados$LIMIT_BAL , gasto = dados$BILL_TOTAL)
x <- base::scale(x)

# Criando o objeto
bdscan <- dbscan(x, eps = 0.37, minPts = 4)
previsoes = bdscan$cluster

# Numero de registros por cluster
base::table(previsoes)

# Plotagem do grafico 
plot(x, col = previsoes)
