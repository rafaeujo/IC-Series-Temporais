## Modelando com n = 1000

#bibliotecas
library(ggplot2)
library(missMethods)
library(Metrics)
library(tidyr)

## Gerando Modelo AR1 (phi = 0.4)
set.seed(31)

## Legenda:
# mAR01 tem phi = 0.4
# nAR01 tem phi = 0.6

mAR01a <- arima.sim(n = 1000, list(ar = c(0.4)))
mAR01b <- arima.sim(n = 1000, list(ar = c(0.4)))

## Imputando dados faltantes

mdataAR01 <- data.frame(Dado = mAR01a, Coluna.controle = mAR01b)

mmiss05_AR01 <- delete_MAR_censoring(mdataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR01 <- delete_MAR_censoring(mdataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR01 <- delete_MAR_censoring(mdataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR01 <- delete_MAR_censoring(mdataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

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

## Calculando RMSE

mmedidasAR01.down.05 <- metricas_down(5, mmiss05_AR01, "Dado", mdataAR01$Dado)
mmedidasAR01.down.10 <- metricas_down(10, mmiss10_AR01, "Dado", mdataAR01$Dado)
mmedidasAR01.down.20 <- metricas_down(20, mmiss20_AR01, "Dado", mdataAR01$Dado)
mmedidasAR01.down.40 <- metricas_down(40, mmiss40_AR01, "Dado", mdataAR01$Dado)

## Gerando Modelo AR1 (phi = 0.6)

nAR01a <- arima.sim(n = 1000, list(ar = c(0.6)))
nAR01b <- arima.sim(n = 1000, list(ar = c(0.6)))

## Imputando dados faltantes

ndataAR01 <- data.frame(Dado = nAR01a, Coluna.controle = nAR01b)

nmiss05_AR01 <- delete_MAR_censoring(ndataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
nmiss10_AR01 <- delete_MAR_censoring(ndataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
nmiss20_AR01 <- delete_MAR_censoring(ndataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
nmiss40_AR01 <- delete_MAR_censoring(ndataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

nmedidasAR01.down.05 <- metricas_down(5, nmiss05_AR01, "Dado", ndataAR01$Dado)
nmedidasAR01.down.10 <- metricas_down(10, nmiss10_AR01, "Dado", ndataAR01$Dado)
nmedidasAR01.down.20 <- metricas_down(20, nmiss20_AR01, "Dado", ndataAR01$Dado)
nmedidasAR01.down.40 <- metricas_down(40, nmiss40_AR01, "Dado", ndataAR01$Dado)

## Gerando Modelo AR2 (0.4; 0.2)

mAR02a <- arima.sim(n = 1000, list(ar = c(0.4, 0.2)))
mAR02b <- arima.sim(n = 1000, list(ar = c(0.4, 0.2)))

## Imputando dados faltantes

mdataAR02 <- data.frame(Dado = mAR02a, Coluna.controle = mAR02b)

mmiss05_AR02 <- delete_MAR_censoring(mdataAR02, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02 <- delete_MAR_censoring(mdataAR02, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02 <- delete_MAR_censoring(mdataAR02, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02 <- delete_MAR_censoring(mdataAR02, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

mmedidasAR02.down.05 <- metricas_down(5, mmiss05_AR02, "Dado", mdataAR02$Dado)
mmedidasAR02.down.10 <- metricas_down(10, mmiss10_AR02, "Dado", mdataAR02$Dado)
mmedidasAR02.down.20 <- metricas_down(20, mmiss20_AR02, "Dado", mdataAR02$Dado)
mmedidasAR02.down.40 <- metricas_down(40, mmiss40_AR02, "Dado", mdataAR02$Dado)

## Gerando Modelo AR2 (0.4; 0.5)

mAR02c <- arima.sim(n = 1000, list(ar = c(0.4, 0.5)))
mAR02d <- arima.sim(n = 1000, list(ar = c(0.4, 0.5)))

## Imputando dados faltantes

mdataAR02_2 <- data.frame(Dado = mAR02c, Coluna.controle = mAR02d)

mmiss05_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

mmedidasAR02_2.down..05 <- metricas_down(5, mmiss05_AR02_2, "Dado", mdataAR02_2$Dado)
mmedidasAR02_2.down..10 <- metricas_down(10, mmiss10_AR02_2, "Dado", mdataAR02_2$Dado)
mmedidasAR02_2.down..20 <- metricas_down(20, mmiss20_AR02_2, "Dado", mdataAR02_2$Dado)
mmedidasAR02_2.down..40 <- metricas_down(40, mmiss40_AR02_2, "Dado", mdataAR02_2$Dado)

## Gerando Modelo ARMA (0.4; 0.2)

ARMA11a <- arima.sim(n = 1000, list(ar = c(0.4), ma = c(0.2)))
ARMA11b <- arima.sim(n = 1000, list(ar = c(0.4), ma = c(0.2)))

## Imputando dados faltantes

mdataARMA1 <- data.frame(Dado = ARMA11a, Coluna.controle = ARMA11b)

miss05_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

medidasARMA1.down.05 <- metricas_down(5, miss05_ARMA1, "Dado", mdataARMA1$Dado)
medidasARMA1.down.10 <- metricas_down(10, miss10_ARMA1, "Dado", mdataARMA1$Dado)
medidasARMA1.down.20 <- metricas_down(20, miss20_ARMA1, "Dado", mdataARMA1$Dado)
medidasARMA1.down.40 <- metricas_down(40, miss40_ARMA1, "Dado", mdataARMA1$Dado)

## Tabelando os valores

mAR01.tab.down.5 <- unname(unlist(mmedidasAR01.down.05))
mAR01.tab.down.10 <- unname(unlist(mmedidasAR01.down.10))
mAR01.tab.down.20 <- unname(unlist(mmedidasAR01.down.20))
mAR01.tab.down.40 <- unname(unlist(mmedidasAR01.down.40))
nAR01.tab.down.5 <- unname(unlist(nmedidasAR01.down.05))
nAR01.tab.down.10 <- unname(unlist(nmedidasAR01.down.10))
nAR01.tab.down.20 <- unname(unlist(nmedidasAR01.down.20))
nAR01.tab.down.40 <- unname(unlist(nmedidasAR01.down.40))
mAR02.tab.down.5 <- unname(unlist(mmedidasAR02.down.05))
mAR02.tab.down.10 <- unname(unlist(mmedidasAR02.down.10))
mAR02.tab.down.20 <- unname(unlist(mmedidasAR02.down.20))
mAR02.tab.down.40 <- unname(unlist(mmedidasAR02.down.40))
mAR02_2.tab.down.5 <- unname(unlist(mmedidasAR02_2.down..05))
mAR02_2.tab.down.10 <- unname(unlist(mmedidasAR02_2.down..10))
mAR02_2.tab.down.20 <- unname(unlist(mmedidasAR02_2.down..20))
mAR02_2.tab.down.40 <- unname(unlist(mmedidasAR02_2.down..40))
mARMA1.tab.down.5 <- unname(unlist(medidasARMA1.down.05))
mARMA1.tab.down.10 <- unname(unlist(medidasARMA1.down.10))
mARMA1.tab.down.20 <- unname(unlist(medidasARMA1.down.20))
mARMA1.tab.down.40 <- unname(unlist(medidasARMA1.down.40))

mtabelaARGeral.down <- data.frame(Porcentagem = c(mAR02.tab.down.5[1], mAR02.tab.down.10[1], mAR02.tab.down.20[1], mAR02.tab.down.40[1]), 
                                      AR.Rmse.04 = c(mAR01.tab.down.5[2], mAR01.tab.down.10[2], mAR01.tab.down.20[2], mAR01.tab.down.40[2]),
                                      AR.Rmse.06 = c(nAR01.tab.down.5[2], nAR01.tab.down.10[2], nAR01.tab.down.20[2], nAR01.tab.down.40[2]),
                                      AR.Rmse.0204 = c(mAR02.tab.down.5[2], mAR02.tab.down.10[2], mAR02.tab.down.20[2], mAR02.tab.down.40[2]),
                                      AR.Rmse.0405 = c(mAR02_2.tab.down.5[2], mAR02_2.tab.down.10[2], mAR02_2.tab.down.20[2], mAR02_2.tab.down.40[2]),
                                      ARMA.Rmse.0402 = c(mARMA1.tab.down.5[2], mARMA1.tab.down.10[2], mARMA1.tab.down.20[2], mARMA1.tab.down.40[2]))


write.csv(mtabelaARGeral.down,file ="mildown.csv", row.names = FALSE)
