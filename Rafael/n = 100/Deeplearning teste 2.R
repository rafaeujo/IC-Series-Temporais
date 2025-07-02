library(Metrics)
deep2 <- read.csv('Rafael/imput.deep.10.csv')

metricas_deep <- function(alfa, data, coluna, antigo) {
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}


## Calculando RMSE
deep.puro <- read.csv('Rafael/dados_imputados_1.csv')
medidasAR04.10.100.1 <- metricas_deep(10, deep2, "Dado10", deep2$Coluna.controle)
print(medidasAR04.10.100.1)

medidasAR04.10.100.2 <- metricas_deep(10, deep2, "Dado10", deep2$Coluna.controle)
print(medidasAR04.10.100.2)

deep3 <- read.csv('Rafael/dados_imputados_cross.csv')
medidasAR04.10.100.3 <- metricas_deep(10, deep3, "Dado10", deep3$Coluna.controle)
print(medidasAR04.10.100.3)

deeplag <- read.csv('Rafael/dados_imputados_lag.csv')
medidasAR04.10.100.lag <- metricas_deep(10, deeplag, "Dado10", deeplag$Coluna.controle)
print(medidasAR04.10.100.lag)
