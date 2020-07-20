library(prophet)

df <- read.csv('D:/Projetos_em_R/Machine_Learning/Dados/teste.csv')

# Treinamento
modelo <- prophet(df, daily.seasonality=TRUE)

future <- make_future_dataframe(modelo, periods = 14)
tail(future)

forecast <- predict(modelo, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Grafico das projecoes
dyplot.prophet(modelo, forecast)
prophet_plot_components(modelo, forecast)

