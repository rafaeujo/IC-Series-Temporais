##Modelando com n = 1000

##Funcao para calculo de Rmsd e vicio

library(Metrics)

metricas_med <- function(alfa, data, coluna, antigo) {
  
  data[[coluna]][is.na(data[[coluna]])] <- median(data[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}

## Gerando Modelo AR1 (phi = 0.4)
ar1.04.med <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.04.csv")

## Calculando RMSE

mmedidasAR01.05.med <- metricas_med(5, ar1.04.med, "Dado5", ar1.04.med$Coluna.controle)
mmedidasAR01.10.med <- metricas_med(10, ar1.04.med, "Dado10", ar1.04.med$Coluna.controle)
mmedidasAR01.20.med <- metricas_med(20, ar1.04.med, "Dado20", ar1.04.med$Coluna.controle)
mmedidasAR01.40.med <- metricas_med(40, ar1.04.med, "Dado40", ar1.04.med$Coluna.controle)

## Tabelando os valores

mAR01.tab5.med <- unname(unlist(mmedidasAR01.05.med))
mAR01.tab10.med <- unname(unlist(mmedidasAR01.10.med))
mAR01.tab20.med <- unname(unlist(mmedidasAR01.20.med))
mAR01.tab40.med <- unname(unlist(mmedidasAR01.40.med))

## Gerando Modelo AR1 (phi = 0.6)

ar1.06.med <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.06.csv")

## Calculando RMSE

nmedidasAR01.05.med <- metricas_med(5, ar1.06.med, "Dado5", ar1.06.med$Coluna.controle)
nmedidasAR01.10.med <- metricas_med(10, ar1.06.med, "Dado10", ar1.06.med$Coluna.controle)
nmedidasAR01.20.med <- metricas_med(20, ar1.06.med, "Dado20", ar1.06.med$Coluna.controle)
nmedidasAR01.40.med <- metricas_med(40, ar1.06.med, "Dado40", ar1.06.med$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.2)

ar2.0402.med <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0402.csv")

## Calculando RMSE

mmedidasAR02.05.med <- metricas_med(5, ar2.0402.med, "Dado5", ar2.0402.med$Coluna.controle)
mmedidasAR02.10.med <- metricas_med(10, ar2.0402.med, "Dado10", ar2.0402.med$Coluna.controle)
mmedidasAR02.20.med <- metricas_med(20, ar2.0402.med, "Dado20", ar2.0402.med$Coluna.controle)
mmedidasAR02.40.med <- metricas_med(40, ar2.0402.med, "Dado40", ar2.0402.med$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.5)

ar2.0405.med <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0405.csv")

## Calculando RMSE

mmedidasAR02_2.05.med <- metricas_med(5, ar2.0405.med, "Dado5", ar2.0405.med$Coluna.controle)
mmedidasAR02_2.10.med <- metricas_med(10, ar2.0405.med, "Dado10", ar2.0405.med$Coluna.controle)
mmedidasAR02_2.20.med <- metricas_med(20, ar2.0405.med, "Dado20", ar2.0405.med$Coluna.controle)
mmedidasAR02_2.40.med <- metricas_med(40, ar2.0405.med, "Dado40", ar2.0405.med$Coluna.controle)

## Gerando Modelo ARMA (0.4; 0.2)

arma.med <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/ARMA.csv")

## Calculando RMSE

medidasARMA1.05.med <- metricas_med(5,arma.med, "Dado5", arma.med$Coluna.controle)
medidasARMA1.10.med <- metricas_med(10, arma.med, "Dado10", arma.med$Coluna.controle)
medidasARMA1.20.med <- metricas_med(20, arma.med, "Dado20", arma.med$Coluna.controle)
medidasARMA1.40.med <- metricas_med(40, arma.med, "Dado40", arma.med$Coluna.controle)

## Tabelando os valores

mAR01.tab5.med <- unname(unlist(mmedidasAR01.05.med))
mAR01.tab10.med <- unname(unlist(mmedidasAR01.10.med))
mAR01.tab20.med  <- unname(unlist(mmedidasAR01.20.med))
mAR01.tab40.med  <- unname(unlist(mmedidasAR01.40.med))
nAR01.tab5.med <- unname(unlist(nmedidasAR01.05.med))
nAR01.tab10.med <- unname(unlist(nmedidasAR01.10.med))
nAR01.tab20.med  <- unname(unlist(nmedidasAR01.20.med))
nAR01.tab40.med  <- unname(unlist(nmedidasAR01.40.med))
mAR02.tab5.med <- unname(unlist(mmedidasAR02.05.med))
mAR02.tab10.med <- unname(unlist(mmedidasAR02.10.med))
mAR02.tab20.med <- unname(unlist(mmedidasAR02.20.med))
mAR02.tab40.med <- unname(unlist(mmedidasAR02.40.med))
mAR02_2.tab5.med <- unname(unlist(mmedidasAR02_2.05.med))
mAR02_2.tab10.med <- unname(unlist(mmedidasAR02_2.10.med))
mAR02_2.tab20.med <- unname(unlist(mmedidasAR02_2.20.med))
mAR02_2.tab40.med <- unname(unlist(mmedidasAR02_2.40.med))
mARMA1.tab5.med <- unname(unlist(medidasARMA1.05.med))
mARMA1.tab10.med <- unname(unlist(medidasARMA1.10.med))
mARMA1.tab20.med <- unname(unlist(medidasARMA1.20.med))
mARMA1.tab40.med <- unname(unlist(medidasARMA1.40.med))

mtabelaARGeral.med <- data.frame(Porcentagem = c(mAR02.tab5.med[1], mAR02.tab10.med[1], mAR02.tab20.med[1], mAR02.tab40.med[1]), 
                                     AR.Rmse.04 = c(mAR01.tab5.med[2], mAR01.tab10.med[2], mAR01.tab20.med[2], mAR01.tab40.med[2]),
                                     AR.Rmse.06 = c(nAR01.tab5.med[2], nAR01.tab10.med[2], nAR01.tab20.med[2], nAR01.tab40.med[2]),
                                     AR.Rmse.0204 = c(mAR02.tab5.med[2], mAR02.tab10.med[2], mAR02.tab20.med[2], mAR02.tab40.med[2]),
                                     AR.Rmse.0405 = c(mAR02_2.tab5.med[2], mAR02_2.tab10.med[2], mAR02_2.tab20.med[2], mAR02_2.tab40.med[2]),
                                     ARMA.Rmse.0402 = c(mARMA1.tab5.med[2], mARMA1.tab10.med[2], mARMA1.tab20.med[2], mARMA1.tab40.med[2]))


write.csv(mtabelaARGeral.med,file ="milmediana.csv", row.names = FALSE)