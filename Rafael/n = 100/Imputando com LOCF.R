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


library(tidyr)

testeLOCF.100 <- fill(mmiss05_AR01.100, Dado)

x11()
par(mfrow=c(1,2))
plot.ts(mmiss05_AR01.100$Dado)
plot.ts(testeLOCF.100$Dado)
