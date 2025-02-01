## Modelando com n = 100

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
ar1.04.int.100 <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR1.04.100.csv")

## Calculando RMSE

mmedidasAR01.05.int.100 <- metricas_int(5, ar1.04.int.100, "Dado5", ar1.04.int.100$Coluna.controle)
mmedidasAR01.10.int.100 <- metricas_int(10, ar1.04.int.100, "Dado10", ar1.04.int.100$Coluna.controle)
mmedidasAR01.20.int.100 <- metricas_int(20, ar1.04.int.100, "Dado20", ar1.04.int.100$Coluna.controle)
mmedidasAR01.40.int.100 <- metricas_int(40, ar1.04.int.100, "Dado40", ar1.04.int.100$Coluna.controle)

## Tabelando os valores

mAR01.tab5.int.100 <- unname(unlist(mmedidasAR01.05.int.100))
mAR01.tab10.int.100 <- unname(unlist(mmedidasAR01.10.int.100))
mAR01.tab20.int.100 <- unname(unlist(mmedidasAR01.20.int.100))
mAR01.tab40.int.100 <- unname(unlist(mmedidasAR01.40.int.100))

## Gerando Modelo AR1 (phi = 0.6)

ar1.06.int.100 <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR1.06.100.csv")

## Calculando RMSE

nmedidasAR01.05.int.100 <- metricas_int(5, ar1.06.int.100, "Dado5", ar1.06.int.100$Coluna.controle)
nmedidasAR01.10.int.100 <- metricas_int(10, ar1.06.int.100, "Dado10", ar1.06.int.100$Coluna.controle)
nmedidasAR01.20.int.100 <- metricas_int(20, ar1.06.int.100, "Dado20", ar1.06.int.100$Coluna.controle)
nmedidasAR01.40.int.100 <- metricas_int(40, ar1.06.int.100, "Dado40", ar1.06.int.100$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.2)

ar2.0402.int.100 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR2.0402.100.csv")

## Calculando RMSE

mmedidasAR02.05.int.100 <- metricas_int(5, ar2.0402.int.100, "Dado5", ar2.0402.int.100$Coluna.controle)
mmedidasAR02.10.int.100 <- metricas_int(10, ar2.0402.int.100, "Dado10", ar2.0402.int.100$Coluna.controle)
mmedidasAR02.20.int.100 <- metricas_int(20, ar2.0402.int.100, "Dado20", ar2.0402.int.100$Coluna.controle)
mmedidasAR02.40.int.100 <- metricas_int(40, ar2.0402.int.100, "Dado40", ar2.0402.int.100$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.5)

ar2.0405.int.100 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR2.0405.100.csv")

## Calculando RMSE

mmedidasAR02_2.05.int.100 <- metricas_int(5, ar2.0405.int.100, "Dado5", ar2.0405.int.100$Coluna.controle)
mmedidasAR02_2.10.int.100 <- metricas_int(10, ar2.0405.int.100, "Dado10", ar2.0405.int.100$Coluna.controle)
mmedidasAR02_2.20.int.100 <- metricas_int(20, ar2.0405.int.100, "Dado20", ar2.0405.int.100$Coluna.controle)
mmedidasAR02_2.40.int.100 <- metricas_int(40, ar2.0405.int.100, "Dado40", ar2.0405.int.100$Coluna.controle)

## Gerando Modelo ARMA (0.4; 0.2)

arma.int.100 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/ARMA.100.csv")

## Calculando RMSE

medidasARMA1.05.int.100 <- metricas_int(5,arma.int.100, "Dado5", arma.int.100$Coluna.controle)
medidasARMA1.10.int.100 <- metricas_int(10, arma.int.100, "Dado10", arma.int.100$Coluna.controle)
medidasARMA1.20.int.100 <- metricas_int(20, arma.int.100, "Dado20", arma.int.100$Coluna.controle)
medidasARMA1.40.int.100 <- metricas_int(40, arma.int.100, "Dado40", arma.int.100$Coluna.controle)

## Tabelando os valores

mAR01.tab5.int.100 <- unname(unlist(mmedidasAR01.05.int.100))
mAR01.tab10.int.100 <- unname(unlist(mmedidasAR01.10.int.100))
mAR01.tab20.int.100  <- unname(unlist(mmedidasAR01.20.int.100))
mAR01.tab40.int.100  <- unname(unlist(mmedidasAR01.40.int.100))
nAR01.tab5.int.100 <- unname(unlist(nmedidasAR01.05.int.100))
nAR01.tab10.int.100 <- unname(unlist(nmedidasAR01.10.int.100))
nAR01.tab20.int.100  <- unname(unlist(nmedidasAR01.20.int.100))
nAR01.tab40.int.100  <- unname(unlist(nmedidasAR01.40.int.100))
mAR02.tab5.int.100 <- unname(unlist(mmedidasAR02.05.int.100))
mAR02.tab10.int.100 <- unname(unlist(mmedidasAR02.10.int.100))
mAR02.tab20.int.100 <- unname(unlist(mmedidasAR02.20.int.100))
mAR02.tab40.int.100 <- unname(unlist(mmedidasAR02.40.int.100))
mAR02_2.tab5.int.100 <- unname(unlist(mmedidasAR02_2.05.int.100))
mAR02_2.tab10.int.100 <- unname(unlist(mmedidasAR02_2.10.int.100))
mAR02_2.tab20.int.100 <- unname(unlist(mmedidasAR02_2.20.int.100))
mAR02_2.tab40.int.100 <- unname(unlist(mmedidasAR02_2.40.int.100))
mARMA1.tab5.int.100 <- unname(unlist(medidasARMA1.05.int.100))
mARMA1.tab10.int.100 <- unname(unlist(medidasARMA1.10.int.100))
mARMA1.tab20.int.100 <- unname(unlist(medidasARMA1.20.int.100))
mARMA1.tab40.int.100 <- unname(unlist(medidasARMA1.40.int.100))

mtabelaARGeral.int.100 <- data.frame(Porcentagem = c(mAR02.tab5.int.100[1], mAR02.tab10.int.100[1], mAR02.tab20.int.100[1], mAR02.tab40.int.100[1]), 
                                      AR.Rmse.04 = c(mAR01.tab5.int.100[2], mAR01.tab10.int.100[2], mAR01.tab20.int.100[2], mAR01.tab40.int.100[2]),
                                      AR.Rmse.06 = c(nAR01.tab5.int.100[2], nAR01.tab10.int.100[2], nAR01.tab20.int.100[2], nAR01.tab40.int.100[2]),
                                      AR.Rmse.0204 = c(mAR02.tab5.int.100[2], mAR02.tab10.int.100[2], mAR02.tab20.int.100[2], mAR02.tab40.int.100[2]),
                                      AR.Rmse.0405 = c(mAR02_2.tab5.int.100[2], mAR02_2.tab10.int.100[2], mAR02_2.tab20.int.100[2], mAR02_2.tab40.int.100[2]),
                                      ARMA.Rmse.0402 = c(mARMA1.tab5.int.100[2], mARMA1.tab10.int.100[2], mARMA1.tab20.int.100[2], mARMA1.tab40.int.100[2]))


write.csv(mtabelaARGeral.int.100,file ="cemint.csv", row.names = FALSE)

