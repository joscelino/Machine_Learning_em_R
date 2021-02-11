library(readr)
library(tibble)
library(plotly)
library(pacman)


dados <- read_csv("D:/Projetos_em_R/Machine_Learning/Dados/credit_data.csv")
dados$clientid <- base::factor(dados$clientid)
dados$age <- ifelse(dados$age < 0, mean(dados$age), dados$age)

# BoxPlot Idade
fig <- plotly::plot_ly(dados, y=~age, type = "box",
                       quartilemethod = "linear", jitter = 0.3)
fig <- fig %>% layout(title = "Box Plot idade")
fig

# Identificando os Outliers
pacman::p_load(rstatix)
dados %>% identify_outliers(age)

# Identificando outliers pelo grafico de dispersao (2 variaveis)

fig2 <- plot_ly(data = dados, x = ~income, y = ~age, color = ~age)
fig2

fig3 <- plot_ly(data = dados, x = ~income, y = ~loan, color = ~age)
fig3

fig4 <- plot_ly(data = dados, x = ~age, y = ~loan, color = ~age)
fig4

dadosCensus <- read_csv("D:/Projetos_em_R/Machine_Learning/Dados/census.csv")
dadosCensus$X1 <- base::factor(dadosCensus$X1)
dadosCensus$education <- base::factor(dadosCensus$education)
dadosCensus$education.num <- base::factor(dadosCensus$education.num)
dadosCensus$marital.status <- base::factor(dadosCensus$marital.status)
dadosCensus$occupation <- base::factor(dadosCensus$occupation)
dadosCensus$relationship <- base::factor(dadosCensus$relationship)
dadosCensus$race <- base::factor(dadosCensus$race)
dadosCensus$sex  <- base::factor(dadosCensus$sex)
dadosCensus$native.country   <- base::factor(dadosCensus$native.country)
dadosCensus$income <- base::factor(dadosCensus$income)
glimpse(dadosCensus)

# BoxPlot Idade
fig5 <- plotly::plot_ly(dadosCensus, y=~age, type = "box", 
                        color = ~education, 
                       quartilemethod = "linear", jitter = 0.3)
fig5

# Grafico de dispersao
# Age x Final.wieght
fig6 <- plotly::plot_ly(data = dadosCensus, x = ~age, y = ~final.weight, 
                color = ~relationship)
fig6

# Identificando os Outliers
pacman::p_load(rstatix, dplyr)
outliers <- dadosCensus %>% dplyr::group_by(age) %>% identify_outliers(final.weight)
glimpse(outliers)
