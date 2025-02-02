## Modelando com n = 100

## Função para cálculo de RMSE e viés

library(Metrics)

metricas_mean <- function(alfa, data, coluna, antigo) {
  data[[coluna]][is.na(data[[coluna]])] <- mean(data[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}

## Gerando Modelo AR1 (phi = 0.4)
ar1.04 <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.04.csv")

## Calculando RMSE

mmedidasAR01.05 <- metricas_mean(5, ar1.04, "Dado5", ar1.04$Coluna.controle)
mmedidasAR01.10 <- metricas_mean(10, ar1.04, "Dado10", ar1.04$Coluna.controle)
mmedidasAR01.20 <- metricas_mean(20, ar1.04, "Dado20", ar1.04$Coluna.controle)
mmedidasAR01.40 <- metricas_mean(40, ar1.04, "Dado40", ar1.04$Coluna.controle)

## Tabelando os valores

mAR01.tab5 <- unname(unlist(mmedidasAR01.05))
mAR01.tab10 <- unname(unlist(mmedidasAR01.10))
mAR01.tab20 <- unname(unlist(mmedidasAR01.20))
mAR01.tab40 <- unname(unlist(mmedidasAR01.40))

## Gerando Modelo AR1 (phi = 0.6)

ar1.06 <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.06.csv")

## Calculando RMSE

nmedidasAR01.05 <- metricas_mean(5, ar1.06, "Dado5", ar1.06$Coluna.controle)
nmedidasAR01.10 <- metricas_mean(10, ar1.06, "Dado10", ar1.06$Coluna.controle)
nmedidasAR01.20 <- metricas_mean(20, ar1.06, "Dado20", ar1.06$Coluna.controle)
nmedidasAR01.40 <- metricas_mean(40, ar1.06, "Dado40", ar1.06$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.2)

ar2.0402 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0402.csv")

## Calculando RMSE

mmedidasAR02.05 <- metricas_mean(5, ar2.0402, "Dado5", ar2.0402$Coluna.controle)
mmedidasAR02.10 <- metricas_mean(10, ar2.0402, "Dado10", ar2.0402$Coluna.controle)
mmedidasAR02.20 <- metricas_mean(20, ar2.0402, "Dado20", ar2.0402$Coluna.controle)
mmedidasAR02.40 <- metricas_mean(40, ar2.0402, "Dado40", ar2.0402$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.5)

ar2.0405 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0405.csv")

## Calculando RMSE

mmedidasAR02_2.05 <- metricas_mean(5, ar2.0405, "Dado5", ar2.0405$Coluna.controle)
mmedidasAR02_2.10 <- metricas_mean(10, ar2.0405, "Dado10", ar2.0405$Coluna.controle)
mmedidasAR02_2.20 <- metricas_mean(20, ar2.0405, "Dado20", ar2.0405$Coluna.controle)
mmedidasAR02_2.40 <- metricas_mean(40, ar2.0405, "Dado40", ar2.0405$Coluna.controle)

## Gerando Modelo ARMA (0.4; 0.2)

arma <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/ARMA.csv")

## Calculando RMSE

medidasARMA1.05 <- metricas_mean(5,arma, "Dado5", arma$Coluna.controle)
medidasARMA1.10 <- metricas_mean(10, arma, "Dado10", arma$Coluna.controle)
medidasARMA1.20 <- metricas_mean(20, arma, "Dado20", arma$Coluna.controle)
medidasARMA1.40 <- metricas_mean(40, arma, "Dado40", arma$Coluna.controle)

## Tabelando os valores

mAR01.tab5 <- unname(unlist(mmedidasAR01.05))
mAR01.tab10 <- unname(unlist(mmedidasAR01.10))
mAR01.tab20  <- unname(unlist(mmedidasAR01.20))
mAR01.tab40  <- unname(unlist(mmedidasAR01.40))
nAR01.tab5 <- unname(unlist(nmedidasAR01.05))
nAR01.tab10 <- unname(unlist(nmedidasAR01.10))
nAR01.tab20  <- unname(unlist(nmedidasAR01.20))
nAR01.tab40  <- unname(unlist(nmedidasAR01.40))
mAR02.tab5 <- unname(unlist(mmedidasAR02.05))
mAR02.tab10 <- unname(unlist(mmedidasAR02.10))
mAR02.tab20 <- unname(unlist(mmedidasAR02.20))
mAR02.tab40 <- unname(unlist(mmedidasAR02.40))
mAR02_2.tab5 <- unname(unlist(mmedidasAR02_2.05))
mAR02_2.tab10 <- unname(unlist(mmedidasAR02_2.10))
mAR02_2.tab20 <- unname(unlist(mmedidasAR02_2.20))
mAR02_2.tab40 <- unname(unlist(mmedidasAR02_2.40))
mARMA1.tab5 <- unname(unlist(medidasARMA1.05))
mARMA1.tab10 <- unname(unlist(medidasARMA1.10))
mARMA1.tab20 <- unname(unlist(medidasARMA1.20))
mARMA1.tab40 <- unname(unlist(medidasARMA1.40))

mtabelaARGeral <- data.frame(Porcentagem = c(mAR02.tab5[1], mAR02.tab10[1], mAR02.tab20[1], mAR02.tab40[1]), 
                                 AR.Rmse.04 = c(mAR01.tab5[2], mAR01.tab10[2], mAR01.tab20[2], mAR01.tab40[2]),
                                 AR.Rmse.06 = c(nAR01.tab5[2], nAR01.tab10[2], nAR01.tab20[2], nAR01.tab40[2]),
                                 AR.Rmse.0204 = c(mAR02.tab5[2], mAR02.tab10[2], mAR02.tab20[2], mAR02.tab40[2]),
                                 AR.Rmse.0405 = c(mAR02_2.tab5[2], mAR02_2.tab10[2], mAR02_2.tab20[2], mAR02_2.tab40[2]),
                                 ARMA.Rmse.0402 = c(mARMA1.tab5[2], mARMA1.tab10[2], mARMA1.tab20[2], mARMA1.tab40[2]))


write.csv(mtabelaARGeral,file ="miledia.csv", row.names = FALSE)

