## Modelando com n = 1000

#bibliotecas
library(ggplot2)
library(missMethods)
library(Metrics)
library(tidyr)

## Função para cálculo de RMSE e viés

metricas_down <- function(alfa, data, coluna, antigo) {
  data_preenchido <- data %>%
    fill(all_of(coluna))
  data_preenchido[[coluna]][is.na(data_preenchido[[coluna]])] <- mean(data_preenchido[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data_preenchido[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Medida para" = alfa, "RMSE" = b))
}

## Gerando Modelo AR1 (phi = 0.4)
ar1.04.down <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.04.csv")

## Calculando RMSE

mmedidasAR01.05.down <- metricas_down(5, ar1.04.down, "Dado5", ar1.04.down$Coluna.controle)
mmedidasAR01.10.down <- metricas_down(10, ar1.04.down, "Dado10", ar1.04.down$Coluna.controle)
mmedidasAR01.20.down <- metricas_down(20, ar1.04.down, "Dado20", ar1.04.down$Coluna.controle)
mmedidasAR01.40.down <- metricas_down(40, ar1.04.down, "Dado40", ar1.04.down$Coluna.controle)

## Tabelando os valores

mAR01.tab5.down <- unname(unlist(mmedidasAR01.05.down))
mAR01.tab10.down <- unname(unlist(mmedidasAR01.10.down))
mAR01.tab20.down <- unname(unlist(mmedidasAR01.20.down))
mAR01.tab40.down <- unname(unlist(mmedidasAR01.40.down))

## Gerando Modelo AR1 (phi = 0.6)

ar1.06.down <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR1.06.csv")

## Calculando RMSE

nmedidasAR01.05.down <- metricas_down(5, ar1.06.down, "Dado5", ar1.06.down$Coluna.controle)
nmedidasAR01.10.down <- metricas_down(10, ar1.06.down, "Dado10", ar1.06.down$Coluna.controle)
nmedidasAR01.20.down <- metricas_down(20, ar1.06.down, "Dado20", ar1.06.down$Coluna.controle)
nmedidasAR01.40.down <- metricas_down(40, ar1.06.down, "Dado40", ar1.06.down$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.2)

ar2.0402.down <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0402.csv")

## Calculando RMSE

mmedidasAR02.05.down <- metricas_down(5, ar2.0402.down, "Dado5", ar2.0402.down$Coluna.controle)
mmedidasAR02.10.down <- metricas_down(10, ar2.0402.down, "Dado10", ar2.0402.down$Coluna.controle)
mmedidasAR02.20.down <- metricas_down(20, ar2.0402.down, "Dado20", ar2.0402.down$Coluna.controle)
mmedidasAR02.40.down <- metricas_down(40, ar2.0402.down, "Dado40", ar2.0402.down$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.5)

ar2.0405.down <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/AR2.0405.csv")

## Calculando RMSE

mmedidasAR02_2.05.down <- metricas_down(5, ar2.0405.down, "Dado5", ar2.0405.down$Coluna.controle)
mmedidasAR02_2.10.down <- metricas_down(10, ar2.0405.down, "Dado10", ar2.0405.down$Coluna.controle)
mmedidasAR02_2.20.down <- metricas_down(20, ar2.0405.down, "Dado20", ar2.0405.down$Coluna.controle)
mmedidasAR02_2.40.down <- metricas_down(40, ar2.0405.down, "Dado40", ar2.0405.down$Coluna.controle)

## Gerando Modelo ARMA (0.4; 0.2)

arma.down <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n = 1000/ARMA.csv")

## Calculando RMSE

medidasARMA1.05.down <- metricas_down(5,arma.down, "Dado5", arma.down$Coluna.controle)
medidasARMA1.10.down <- metricas_down(10, arma.down, "Dado10", arma.down$Coluna.controle)
medidasARMA1.20.down <- metricas_down(20, arma.down, "Dado20", arma.down$Coluna.controle)
medidasARMA1.40.down <- metricas_down(40, arma.down, "Dado40", arma.down$Coluna.controle)

## Tabelando os valores

mAR01.tab5.down <- unname(unlist(mmedidasAR01.05.down))
mAR01.tab10.down <- unname(unlist(mmedidasAR01.10.down))
mAR01.tab20.down  <- unname(unlist(mmedidasAR01.20.down))
mAR01.tab40.down  <- unname(unlist(mmedidasAR01.40.down))
nAR01.tab5.down <- unname(unlist(nmedidasAR01.05.down))
nAR01.tab10.down <- unname(unlist(nmedidasAR01.10.down))
nAR01.tab20.down  <- unname(unlist(nmedidasAR01.20.down))
nAR01.tab40.down  <- unname(unlist(nmedidasAR01.40.down))
mAR02.tab5.down <- unname(unlist(mmedidasAR02.05.down))
mAR02.tab10.down <- unname(unlist(mmedidasAR02.10.down))
mAR02.tab20.down <- unname(unlist(mmedidasAR02.20.down))
mAR02.tab40.down <- unname(unlist(mmedidasAR02.40.down))
mAR02_2.tab5.down <- unname(unlist(mmedidasAR02_2.05.down))
mAR02_2.tab10.down <- unname(unlist(mmedidasAR02_2.10.down))
mAR02_2.tab20.down <- unname(unlist(mmedidasAR02_2.20.down))
mAR02_2.tab40.down <- unname(unlist(mmedidasAR02_2.40.down))
mARMA1.tab5.down <- unname(unlist(medidasARMA1.05.down))
mARMA1.tab10.down <- unname(unlist(medidasARMA1.10.down))
mARMA1.tab20.down <- unname(unlist(medidasARMA1.20.down))
mARMA1.tab40.down <- unname(unlist(medidasARMA1.40.down))

mtabelaARGeral.down <- data.frame(Porcentagem = c(mAR02.tab5.down[1], mAR02.tab10.down[1], mAR02.tab20.down[1], mAR02.tab40.down[1]), 
                                      AR.Rmse.04 = c(mAR01.tab5.down[2], mAR01.tab10.down[2], mAR01.tab20.down[2], mAR01.tab40.down[2]),
                                      AR.Rmse.06 = c(nAR01.tab5.down[2], nAR01.tab10.down[2], nAR01.tab20.down[2], nAR01.tab40.down[2]),
                                      AR.Rmse.0204 = c(mAR02.tab5.down[2], mAR02.tab10.down[2], mAR02.tab20.down[2], mAR02.tab40.down[2]),
                                      AR.Rmse.0405 = c(mAR02_2.tab5.down[2], mAR02_2.tab10.down[2], mAR02_2.tab20.down[2], mAR02_2.tab40.down[2]),
                                      ARMA.Rmse.0402 = c(mARMA1.tab5.down[2], mARMA1.tab10.down[2], mARMA1.tab20.down[2], mARMA1.tab40.down[2]))


write.csv(mtabelaARGeral.down,file ="mildown.csv", row.names = FALSE)

