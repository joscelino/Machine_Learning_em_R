base <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv")
base$clientid <- NULL
idades_invalidas <- base[base$age < 0 & !is.na(base$age), ]
base$age <- ifelse(base$age < 0, mean(base$age[base$age > 0], na.rm = TRUE), base$age)
base[is.na(base$age), ]
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)
base[, 1:3] <- scale(base[, 1:3])
base$default <- factor(base$default, levels = c(0, 1))

# Previsoes Random Forest
rfFinal <- readRDS('rfFinal.rds')
previsoesrf <- predict(rfFinal, newdata = base[-4])


# Previsoes Rede Neural
rnaFinal <- readRDS('rnaFinal.rds')
previsoesrna <- predict(rnaFinal, newdata = as.h2o(base[-4]))
