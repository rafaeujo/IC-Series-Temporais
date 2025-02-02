## Modelando com n = 1000

#bibliotecas
library(ggplot2)
library(missMethods)
library(Metrics)
library(tidyr)
library(imputeTS)

## Função para cálculo de RMSE e viés

metricas_int <- function(alfa, data, coluna, antigo) {
  data_preenchido <- data %>%
    na_interpolation(all_of(coluna), option = "spline")
  b <- rmse(antigo, data_preenchido[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Medida para" = alfa, "RMSE" = b))
}

## Gerando Modelo AR1 (phi = 0.4)
ar1.04.int <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.04.csv")

## Calculando RMSE

mmedidasAR01.05.int <- metricas_int(5, ar1.04.int, "Dado5", ar1.04.int$Coluna.controle)
mmedidasAR01.10.int <- metricas_int(10, ar1.04.int, "Dado10", ar1.04.int$Coluna.controle)
mmedidasAR01.20.int <- metricas_int(20, ar1.04.int, "Dado20", ar1.04.int$Coluna.controle)
mmedidasAR01.40.int <- metricas_int(40, ar1.04.int, "Dado40", ar1.04.int$Coluna.controle)

## Tabelando os valores

mAR01.tab5.int <- unname(unlist(mmedidasAR01.05.int))
mAR01.tab10.int <- unname(unlist(mmedidasAR01.10.int))
mAR01.tab20.int <- unname(unlist(mmedidasAR01.20.int))
mAR01.tab40.int <- unname(unlist(mmedidasAR01.40.int))

## Gerando Modelo AR1 (phi = 0.6)

ar1.06.int <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.06.csv")

## Calculando RMSE

nmedidasAR01.05.int <- metricas_int(5, ar1.06.int, "Dado5", ar1.06.int$Coluna.controle)
nmedidasAR01.10.int <- metricas_int(10, ar1.06.int, "Dado10", ar1.06.int$Coluna.controle)
nmedidasAR01.20.int <- metricas_int(20, ar1.06.int, "Dado20", ar1.06.int$Coluna.controle)
nmedidasAR01.40.int <- metricas_int(40, ar1.06.int, "Dado40", ar1.06.int$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.2)

ar2.0402.int <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0402.csv")

## Calculando RMSE

mmedidasAR02.05.int <- metricas_int(5, ar2.0402.int, "Dado5", ar2.0402.int$Coluna.controle)
mmedidasAR02.10.int <- metricas_int(10, ar2.0402.int, "Dado10", ar2.0402.int$Coluna.controle)
mmedidasAR02.20.int <- metricas_int(20, ar2.0402.int, "Dado20", ar2.0402.int$Coluna.controle)
mmedidasAR02.40.int <- metricas_int(40, ar2.0402.int, "Dado40", ar2.0402.int$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.5)

ar2.0405.int <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0405.csv")

## Calculando RMSE

mmedidasAR02_2.05.int <- metricas_int(5, ar2.0405.int, "Dado5", ar2.0405.int$Coluna.controle)
mmedidasAR02_2.10.int <- metricas_int(10, ar2.0405.int, "Dado10", ar2.0405.int$Coluna.controle)
mmedidasAR02_2.20.int <- metricas_int(20, ar2.0405.int, "Dado20", ar2.0405.int$Coluna.controle)
mmedidasAR02_2.40.int <- metricas_int(40, ar2.0405.int, "Dado40", ar2.0405.int$Coluna.controle)

## Gerando Modelo ARMA (0.4; 0.2)

arma.int <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/ARMA.csv")

## Calculando RMSE

medidasARMA1.05.int <- metricas_int(5,arma.int, "Dado5", arma.int$Coluna.controle)
medidasARMA1.10.int <- metricas_int(10, arma.int, "Dado10", arma.int$Coluna.controle)
medidasARMA1.20.int <- metricas_int(20, arma.int, "Dado20", arma.int$Coluna.controle)
medidasARMA1.40.int <- metricas_int(40, arma.int, "Dado40", arma.int$Coluna.controle)

## Tabelando os valores

mAR01.tab5.int <- unname(unlist(mmedidasAR01.05.int))
mAR01.tab10.int <- unname(unlist(mmedidasAR01.10.int))
mAR01.tab20.int  <- unname(unlist(mmedidasAR01.20.int))
mAR01.tab40.int  <- unname(unlist(mmedidasAR01.40.int))
nAR01.tab5.int <- unname(unlist(nmedidasAR01.05.int))
nAR01.tab10.int <- unname(unlist(nmedidasAR01.10.int))
nAR01.tab20.int  <- unname(unlist(nmedidasAR01.20.int))
nAR01.tab40.int  <- unname(unlist(nmedidasAR01.40.int))
mAR02.tab5.int <- unname(unlist(mmedidasAR02.05.int))
mAR02.tab10.int <- unname(unlist(mmedidasAR02.10.int))
mAR02.tab20.int <- unname(unlist(mmedidasAR02.20.int))
mAR02.tab40.int <- unname(unlist(mmedidasAR02.40.int))
mAR02_2.tab5.int <- unname(unlist(mmedidasAR02_2.05.int))
mAR02_2.tab10.int <- unname(unlist(mmedidasAR02_2.10.int))
mAR02_2.tab20.int <- unname(unlist(mmedidasAR02_2.20.int))
mAR02_2.tab40.int <- unname(unlist(mmedidasAR02_2.40.int))
mARMA1.tab5.int <- unname(unlist(medidasARMA1.05.int))
mARMA1.tab10.int <- unname(unlist(medidasARMA1.10.int))
mARMA1.tab20.int <- unname(unlist(medidasARMA1.20.int))
mARMA1.tab40.int <- unname(unlist(medidasARMA1.40.int))

mtabelaARGeral.int <- data.frame(Porcentagem = c(mAR02.tab5.int[1], mAR02.tab10.int[1], mAR02.tab20.int[1], mAR02.tab40.int[1]), 
                                     AR.Rmse.04 = c(mAR01.tab5.int[2], mAR01.tab10.int[2], mAR01.tab20.int[2], mAR01.tab40.int[2]),
                                     AR.Rmse.06 = c(nAR01.tab5.int[2], nAR01.tab10.int[2], nAR01.tab20.int[2], nAR01.tab40.int[2]),
                                     AR.Rmse.0204 = c(mAR02.tab5.int[2], mAR02.tab10.int[2], mAR02.tab20.int[2], mAR02.tab40.int[2]),
                                     AR.Rmse.0405 = c(mAR02_2.tab5.int[2], mAR02_2.tab10.int[2], mAR02_2.tab20.int[2], mAR02_2.tab40.int[2]),
                                     ARMA.Rmse.0402 = c(mARMA1.tab5.int[2], mARMA1.tab10.int[2], mARMA1.tab20.int[2], mARMA1.tab40.int[2]))


write.csv(mtabelaARGeral.int,file ="milint.csv", row.names = FALSE)

