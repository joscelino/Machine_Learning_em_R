library(prophet)

df <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/dol3.csv')
df$Volume <- NULL
df$Open <- NULL
df$High <- NULL
df$Low <- NULL
df$Close <- NULL
df[2] <- NULL

colnames(df)[1] <- "ds"
colnames(df)[2] <- "y"

# Treinamento
modelo <- prophet(df, daily.seasonality=TRUE, yearly.seasonality=TRUE,
                  weekly.seasonality = TRUE, initialize_scales = TRUE,
                  seasonality.mode =  'multiplicative')

future <- make_future_dataframe(modelo, periods = 30)
tail(future)

forecast <- predict(modelo, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Grafico das projecoes
dyplot.prophet(modelo, forecast)
prophet_plot_components(modelo, forecast)

