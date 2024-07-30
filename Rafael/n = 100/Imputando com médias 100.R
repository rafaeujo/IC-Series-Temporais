## Modelando com n = 100

## Gerando Modelo AR1 (phi = 0.4)
set.seed(31)

## Legenda:
# mAR01 tem phi = 0.4
# nAR01 tem phi = 0.6

mAR01a.100 <- arima.sim(n = 100, list(ar = c(0.4)))
mAR01b.100 <- arima.sim(n = 100, list(ar = c(0.4)))

## Imputando dados faltantes

library(missMethods)

mdataAR01.100 <- data.frame(Dado = mAR01a.100, Coluna.controle = mAR01b.100)

mmiss05_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Função para cálculo de RMSE e viés

library(Metrics)

metricas_mean.100 <- function(alfa, data, coluna, antigo) {
  data[[coluna]][is.na(data[[coluna]])] <- mean(data[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}

## Calculando RMSE

mmedidasAR01.05.100 <- metricas_mean.100(5, mmiss05_AR01.100, "Dado", mdataAR01.100$Dado)
mmedidasAR01.10.100 <- metricas_mean.100(10, mmiss10_AR01.100, "Dado", mdataAR01.100$Dado)
mmedidasAR01.20.100 <- metricas_mean.100(20, mmiss20_AR01.100, "Dado", mdataAR01.100$Dado)
mmedidasAR01.40.100 <- metricas_mean.100(40, mmiss40_AR01.100, "Dado", mdataAR01.100$Dado)

## Tabelando os valores

mAR01.tab5.100 <- unname(unlist(mmedidasAR01.05.100))
mAR01.tab10.100 <- unname(unlist(mmedidasAR01.10.100))
mAR01.tab20.100 <- unname(unlist(mmedidasAR01.20.100))
mAR01.tab40.100 <- unname(unlist(mmedidasAR01.40.100))

## Gerando Modelo AR1 (phi = 0.6)

nAR01a.100 <- arima.sim(n = 100, list(ar = c(0.6)))
nAR01b.100 <- arima.sim(n = 100, list(ar = c(0.6)))

## Imputando dados faltantes

ndataAR01.100 <- data.frame(Dado = nAR01a.100, Coluna.controle = nAR01b.100)

nmiss05_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
nmiss10_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
nmiss20_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
nmiss40_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

nmedidasAR01.05.100 <- metricas_mean.100(5, nmiss05_AR01.100, "Dado", ndataAR01.100$Dado)
nmedidasAR01.10.100 <- metricas_mean.100(10, nmiss10_AR01.100, "Dado", ndataAR01.100$Dado)
nmedidasAR01.20.100 <- metricas_mean.100(20, nmiss20_AR01.100, "Dado", ndataAR01.100$Dado)
nmedidasAR01.40.100 <- metricas_mean.100(40, nmiss40_AR01.100, "Dado", ndataAR01.100$Dado)

## Tabelando os valores

nAR01.tab5.100 <- unname(unlist(nmedidasAR01.05.100))
nAR01.tab10.100 <- unname(unlist(nmedidasAR01.10.100))
nAR01.tab20.100 <- unname(unlist(nmedidasAR01.20.100))
nAR01.tab40.100 <- unname(unlist(nmedidasAR01.40.100))

miltabelaAR01.100 <- data.frame(Porcentagem = c(mAR01.tab5.100[1], mAR01.tab10.100[1], mAR01.tab20.100[1], mAR01.tab40.100[1]), 
                                Rmse04 = c(mAR01.tab5.100[2], mAR01.tab10.100[2], mAR01.tab20.100[2], mAR01.tab40.100[2]),
                                Rmse06 = c(nAR01.tab5.100[2], nAR01.tab10.100[2], nAR01.tab20.100[2], nAR01.tab40.100[2]))

## Gerando Modelo AR2 (0.4; 0.2)

mAR02a.100 <- arima.sim(n = 100, list(ar = c(0.4, 0.2)))
mAR02b.100 <- arima.sim(n = 100, list(ar = c(0.4, 0.2)))

## Imputando dados faltantes

mdataAR02.100 <- data.frame(Dado = mAR02a.100, Coluna.controle = mAR02b.100)

mmiss05_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

mmedidasAR02.05.100 <- metricas_mean.100(5, mmiss05_AR02.100, "Dado", mdataAR02.100$Dado)
mmedidasAR02.10.100 <- metricas_mean.100(10, mmiss10_AR02.100, "Dado", mdataAR02.100$Dado)
mmedidasAR02.20.100 <- metricas_mean.100(20, mmiss20_AR02.100, "Dado", mdataAR02.100$Dado)
mmedidasAR02.40.100 <- metricas_mean.100(40, mmiss40_AR02.100, "Dado", mdataAR02.100$Dado)

## Gerando Modelo AR2 (0.4; 0.5)

mAR02c.100 <- arima.sim(n = 100, list(ar = c(0.4, 0.5)))
mAR02d.100 <- arima.sim(n = 100, list(ar = c(0.4, 0.5)))

## Imputando dados faltantes

mdataAR02_2.100 <- data.frame(Dado = mAR02c.100, Coluna.controle = mAR02d.100)

mmiss05_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

mmedidasAR02_2.05.100 <- metricas_mean.100(5, mmiss05_AR02_2.100, "Dado", mdataAR02_2.100$Dado)
mmedidasAR02_2.10.100 <- metricas_mean.100(10, mmiss10_AR02_2.100, "Dado", mdataAR02_2.100$Dado)
mmedidasAR02_2.20.100 <- metricas_mean.100(20, mmiss20_AR02_2.100, "Dado", mdataAR02_2.100$Dado)
mmedidasAR02_2.40.100 <- metricas_mean.100(40, mmiss40_AR02_2.100, "Dado", mdataAR02_2.100$Dado)

## Gerando Modelo ARMA (0.4; 0.2)

ARMA11a.100 <- arima.sim(n = 100, list(ar = c(0.4), ma = c(0.2)))
ARMA11b.100 <- arima.sim(n = 100, list(ar = c(0.4), ma = c(0.2)))

## Imputando dados faltantes

mdataARMA1.100 <- data.frame(Dado = ARMA11a.100, Coluna.controle = ARMA11b.100)

miss05_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Calculando RMSE

medidasARMA1.05.100 <- metricas_mean.100(5, miss05_ARMA1.100, "Dado", mdataARMA1.100$Dado)
medidasARMA1.10.100 <- metricas_mean.100(10, miss10_ARMA1.100, "Dado", mdataARMA1.100$Dado)
medidasARMA1.20.100 <- metricas_mean.100(20, miss20_ARMA1.100, "Dado", mdataARMA1.100$Dado)
medidasARMA1.40.100 <- metricas_mean.100(40, miss40_ARMA1.100, "Dado", mdataARMA1.100$Dado)

## Tabelando os valores

mAR02.tab5.100 <- unname(unlist(mmedidasAR02.05.100))
mAR02.tab10.100 <- unname(unlist(mmedidasAR02.10.100))
mAR02.tab20.100 <- unname(unlist(mmedidasAR02.20.100))
mAR02.tab40.100 <- unname(unlist(mmedidasAR02.40.100))
mAR02_2.tab5.100 <- unname(unlist(mmedidasAR02_2.05.100))
mAR02_2.tab10.100 <- unname(unlist(mmedidasAR02_2.10.100))
mAR02_2.tab20.100 <- unname(unlist(mmedidasAR02_2.20.100))
mAR02_2.tab40.100 <- unname(unlist(mmedidasAR02_2.40.100))
mARMA1.tab5.100 <- unname(unlist(medidasARMA1.05.100))
mARMA1.tab10.100 <- unname(unlist(medidasARMA1.10.100))
mARMA1.tab20.100 <- unname(unlist(medidasARMA1.20.100))
mARMA1.tab40.100 <- unname(unlist(medidasARMA1.40.100))

mtabelaARGeral.100 <- data.frame(Porcentagem = c(mAR02.tab5.100[1], mAR02.tab10.100[1], mAR02.tab20.100[1], mAR02.tab40.100[1]), 
                                 AR.Rmse.04 = c(mAR01.tab5.100[2], mAR01.tab10.100[2], mAR01.tab20.100[2], mAR01.tab40.100[2]),
                                 AR.Rmse.06 = c(nAR01.tab5.100[2], nAR01.tab10.100[2], nAR01.tab20.100[2], nAR01.tab40.100[2]),
                                 AR.Rmse.0204 = c(mAR02.tab5.100[2], mAR02.tab10.100[2], mAR02.tab20.100[2], mAR02.tab40.100[2]),
                                 AR.Rmse.0405 = c(mAR02_2.tab5.100[2], mAR02_2.tab10.100[2], mAR02_2.tab20.100[2], mAR02_2.tab40.100[2]),
                                 ARMA.Rmse.0402 = c(mARMA1.tab5.100[2], mARMA1.tab10.100[2], mARMA1.tab20.100[2], mARMA1.tab40.100[2]))

## Gerando os Gráficos

x11()
par(mfrow = c(3, 2))
ts.plot(mAR01a.100)
ts.plot(nAR01a.100)
ts.plot(mAR02a.100)
ts.plot(mAR02c.100)
ts.plot(ARMA11a.100)
