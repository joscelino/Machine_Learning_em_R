
base <- read.csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv")
base$clientid <- NULL
idades_invalidas <- base[base$age < 0 & !is.na(base$age), ]
base$age <- ifelse(base$age < 0, mean(base$age[base$age > 0], na.rm = TRUE), base$age)
base[is.na(base$age), ]
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)
base[, 1:3] <- scale(base[, 1:3])
base$default <- factor(base$default, levels = c(0, 1))


# Criando classificador Random Forest
library(randomForest)
classificadorRandomForest <- randomForest(x = base[-4], y = base$default,
                                          ntree = 30, mtry = 2)

# Criando classificador Rede Neural
library(h2o)
h2o.init(nthreads = -1)
classificadorRedeNeural <- h2o.deeplearning(y = 'default', 
                                            training_frame = as.h2o(base), 
                                            activation = 'Rectifier', 
                                            hidden = c(100), epochs = 100)

# Salvando Classificadores
saveRDS(classificadorRandomForest, 'rfFinal.rds')
saveRDS(classificadorRedeNeural, 'rnaFinal.rds')
