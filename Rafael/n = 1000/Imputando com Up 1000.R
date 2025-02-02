## Modelando com n = 1000

#bibliotecas
library(ggplot2)
library(missMethods)
library(Metrics)
library(tidyr)

## Função para cálculo de RMSE e viés

metricas_up <- function(alfa, data, coluna, antigo) {
  data_preenchido <- data %>%
    fill(all_of(coluna), .direction = "up")
  data_preenchido[[coluna]][is.na(data_preenchido[[coluna]])] <- mean(data_preenchido[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data_preenchido[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Medida para" = alfa, "RMSE" = b))
}

## Gerando Modelo AR1 (phi = 0.4)
ar1.04.up <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.04.csv")

## Calculando RMSE

mmedidasAR01.05.up <- metricas_up(5, ar1.04.up, "Dado5", ar1.04.up$Coluna.controle)
mmedidasAR01.10.up <- metricas_up(10, ar1.04.up, "Dado10", ar1.04.up$Coluna.controle)
mmedidasAR01.20.up <- metricas_up(20, ar1.04.up, "Dado20", ar1.04.up$Coluna.controle)
mmedidasAR01.40.up <- metricas_up(40, ar1.04.up, "Dado40", ar1.04.up$Coluna.controle)

## Tabelando os valores

mAR01.tab5.up <- unname(unlist(mmedidasAR01.05.up))
mAR01.tab10.up <- unname(unlist(mmedidasAR01.10.up))
mAR01.tab20.up <- unname(unlist(mmedidasAR01.20.up))
mAR01.tab40.up <- unname(unlist(mmedidasAR01.40.up))

## Gerando Modelo AR1 (phi = 0.6)

ar1.06.up <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.06.csv")

## Calculando RMSE

nmedidasAR01.05.up <- metricas_up(5, ar1.06.up, "Dado5", ar1.06.up$Coluna.controle)
nmedidasAR01.10.up <- metricas_up(10, ar1.06.up, "Dado10", ar1.06.up$Coluna.controle)
nmedidasAR01.20.up <- metricas_up(20, ar1.06.up, "Dado20", ar1.06.up$Coluna.controle)
nmedidasAR01.40.up <- metricas_up(40, ar1.06.up, "Dado40", ar1.06.up$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.2)

ar2.0402.up <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0402.csv")

## Calculando RMSE

mmedidasAR02.05.up <- metricas_up(5, ar2.0402.up, "Dado5", ar2.0402.up$Coluna.controle)
mmedidasAR02.10.up <- metricas_up(10, ar2.0402.up, "Dado10", ar2.0402.up$Coluna.controle)
mmedidasAR02.20.up <- metricas_up(20, ar2.0402.up, "Dado20", ar2.0402.up$Coluna.controle)
mmedidasAR02.40.up <- metricas_up(40, ar2.0402.up, "Dado40", ar2.0402.up$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.5)

ar2.0405.up <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0405.csv")

## Calculando RMSE

mmedidasAR02_2.05.up <- metricas_up(5, ar2.0405.up, "Dado5", ar2.0405.up$Coluna.controle)
mmedidasAR02_2.10.up <- metricas_up(10, ar2.0405.up, "Dado10", ar2.0405.up$Coluna.controle)
mmedidasAR02_2.20.up <- metricas_up(20, ar2.0405.up, "Dado20", ar2.0405.up$Coluna.controle)
mmedidasAR02_2.40.up <- metricas_up(40, ar2.0405.up, "Dado40", ar2.0405.up$Coluna.controle)

## Gerando Modelo ARMA (0.4; 0.2)

arma.up <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/ARMA.csv")

## Calculando RMSE

medidasARMA1.05.up <- metricas_up(5,arma.up, "Dado5", arma.up$Coluna.controle)
medidasARMA1.10.up <- metricas_up(10, arma.up, "Dado10", arma.up$Coluna.controle)
medidasARMA1.20.up <- metricas_up(20, arma.up, "Dado20", arma.up$Coluna.controle)
medidasARMA1.40.up <- metricas_up(40, arma.up, "Dado40", arma.up$Coluna.controle)

## Tabelando os valores

mAR01.tab5.up <- unname(unlist(mmedidasAR01.05.up))
mAR01.tab10.up <- unname(unlist(mmedidasAR01.10.up))
mAR01.tab20.up  <- unname(unlist(mmedidasAR01.20.up))
mAR01.tab40.up  <- unname(unlist(mmedidasAR01.40.up))
nAR01.tab5.up <- unname(unlist(nmedidasAR01.05.up))
nAR01.tab10.up <- unname(unlist(nmedidasAR01.10.up))
nAR01.tab20.up  <- unname(unlist(nmedidasAR01.20.up))
nAR01.tab40.up  <- unname(unlist(nmedidasAR01.40.up))
mAR02.tab5.up <- unname(unlist(mmedidasAR02.05.up))
mAR02.tab10.up <- unname(unlist(mmedidasAR02.10.up))
mAR02.tab20.up <- unname(unlist(mmedidasAR02.20.up))
mAR02.tab40.up <- unname(unlist(mmedidasAR02.40.up))
mAR02_2.tab5.up <- unname(unlist(mmedidasAR02_2.05.up))
mAR02_2.tab10.up <- unname(unlist(mmedidasAR02_2.10.up))
mAR02_2.tab20.up <- unname(unlist(mmedidasAR02_2.20.up))
mAR02_2.tab40.up <- unname(unlist(mmedidasAR02_2.40.up))
mARMA1.tab5.up <- unname(unlist(medidasARMA1.05.up))
mARMA1.tab10.up <- unname(unlist(medidasARMA1.10.up))
mARMA1.tab20.up <- unname(unlist(medidasARMA1.20.up))
mARMA1.tab40.up <- unname(unlist(medidasARMA1.40.up))

mtabelaARGeral.up <- data.frame(Porcentagem = c(mAR02.tab5.up[1], mAR02.tab10.up[1], mAR02.tab20.up[1], mAR02.tab40.up[1]), 
                                    AR.Rmse.04 = c(mAR01.tab5.up[2], mAR01.tab10.up[2], mAR01.tab20.up[2], mAR01.tab40.up[2]),
                                    AR.Rmse.06 = c(nAR01.tab5.up[2], nAR01.tab10.up[2], nAR01.tab20.up[2], nAR01.tab40.up[2]),
                                    AR.Rmse.0204 = c(mAR02.tab5.up[2], mAR02.tab10.up[2], mAR02.tab20.up[2], mAR02.tab40.up[2]),
                                    AR.Rmse.0405 = c(mAR02_2.tab5.up[2], mAR02_2.tab10.up[2], mAR02_2.tab20.up[2], mAR02_2.tab40.up[2]),
                                    ARMA.Rmse.0402 = c(mARMA1.tab5.up[2], mARMA1.tab10.up[2], mARMA1.tab20.up[2], mARMA1.tab40.up[2]))


write.csv(mtabelaARGeral.up,file ="milup.csv", row.names = FALSE)

