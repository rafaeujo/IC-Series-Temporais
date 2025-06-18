library(Metrics)
deep <- read.csv('Rafael/Dados_Completos_Imputados.csv')
print(deep)


metricas_deep <- function(alfa, data, coluna, antigo) {
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}


## Calculando RMSE

medidasAR04.05.100 <- metricas_deep(5,deep, "Dado5", deep$Coluna.controle)
medidasAR04.10.100 <- metricas_deep(10, deep, "Dado10", deep$Coluna.controle)
medidasAR04.20.100 <- metricas_deep(20, deep, "Dado20", deep$Coluna.controle)
medidasAR04.40.100 <- metricas_deep(40, deep, "Dado40", deep$Coluna.controle)

## Tabelando os valores

mAR04.tab5.100 <- unname(unlist(medidasAR04.05.100))
mAR04.tab10.100 <- unname(unlist(medidasAR04.10.100))
mAR04.tab20.100  <- unname(unlist(medidasAR04.20.100))
mAR04.tab40.100  <- unname(unlist(medidasAR04.40.100))

dtabelaARGeral.100 <- data.frame(Porcentagem = c(mAR04.tab5.100[1], mAR04.tab10.100[1], mAR04.tab20.100[1], mAR04.tab40.100[1]), 
                                 AR.Rmse.04 = c(mAR04.tab5.100[2], mAR04.tab10.100[2], mAR04.tab20.100[2], mAR04.tab40.100[2]))
