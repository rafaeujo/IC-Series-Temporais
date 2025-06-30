library(Metrics)
deep2 <- read.csv('Rafael/imput.deep.10.csv')

metricas_deep <- function(alfa, data, coluna, antigo) {
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}


## Calculando RMSE

medidasAR04.10.100.2 <- metricas_deep(10, deep, "Dado10", deep2$Coluna.controle)
print(medidasAR04.10.100)
