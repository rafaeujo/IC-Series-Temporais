##Simulando os modelos ARIMA

library(missMethods)
set.seed(2003)

## Gerando Modelo AR1 (phi = 0.4)
mAR01a.100 <- arima.sim(n = 100, list(ar = c(0.4)))
mdataAR01.100 <- data.frame(Dado = mAR01a.100, Coluna.controle = mAR01a.100)

mmiss05_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar1.04.100 <- data.frame(Dado5 = mmiss05_AR01.100$Dado, Dado10 = mmiss10_AR01.100$Dado,
                         Dado20 = mmiss20_AR01.100$Dado, Dado40 = mmiss40_AR01.100$Dado,
                         Coluna.controle = mAR01a.100)
write.csv(ar1.04.100,file ="AR1.04.100.csv", row.names = FALSE)

## Gerando Modelo AR1 (phi = 0.6)
nAR01a.100 <- arima.sim(n = 100, list(ar = c(0.6)))
ndataAR01.100 <- data.frame(Dado = nAR01a.100, Coluna.controle = nAR01a.100)

nmiss05_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
nmiss10_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
nmiss20_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
nmiss40_AR01.100 <- delete_MAR_censoring(ndataAR01.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar1.06.100 <- data.frame(Dado5 = nmiss05_AR01.100$Dado, Dado10 = nmiss10_AR01.100$Dado,
                         Dado20 = nmiss20_AR01.100$Dado, Dado40 = nmiss40_AR01.100$Dado,
                         Coluna.controle = nAR01a.100)
write.csv(ar1.06.100,file ="AR1.06.100.csv", row.names = FALSE)

## Gerando Modelo AR2 (0.4; 0.2)
mAR02a.100 <- arima.sim(n = 100, list(ar = c(0.4, 0.2)))
mdataAR02.100 <- data.frame(Dado = mAR02a.100, Coluna.controle = mAR02a.100)

mmiss05_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02.100 <- delete_MAR_censoring(mdataAR02.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar2.0402.100 <- data.frame(Dado5 = mmiss05_AR02.100$Dado, Dado10 = mmiss10_AR02.100$Dado,
                         Dado20 = mmiss20_AR02.100$Dado, Dado40 = mmiss40_AR02.100$Dado,
                         Coluna.controle = mAR02a.100)
write.csv(ar2.0402.100,file ="AR2.0402.100.csv", row.names = FALSE)

## Gerando Modelo AR2 (0.4; 0.5)
mAR02c.100 <- arima.sim(n = 100, list(ar = c(0.4, 0.5)))
mdataAR02_2.100 <- data.frame(Dado = mAR02c.100, Coluna.controle = mAR02c.100)

mmiss05_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02_2.100 <- delete_MAR_censoring(mdataAR02_2.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar2.0405.100 <- data.frame(Dado5 = mmiss05_AR02_2.100$Dado, Dado10 = mmiss10_AR02_2.100$Dado,
                           Dado20 = mmiss20_AR02_2.100$Dado, Dado40 = mmiss40_AR02_2.100$Dado,
                           Coluna.controle = mAR02c.100)
write.csv(ar2.0405.100,file ="AR2.0405.100.csv", row.names = FALSE)

## Gerando Modelo ARMA (0.4; 0.2)
ARMA11a.100 <- arima.sim(n = 100, list(ar = c(0.4), ma = c(0.2)))
mdataARMA1.100 <- data.frame(Dado = ARMA11a.100, Coluna.controle = ARMA11a.100)

miss05_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")


arma.100 <- data.frame(Dado5 = miss05_ARMA1.100$Dado, Dado10 = miss10_ARMA1.100$Dado,
                           Dado20 = miss20_ARMA1.100$Dado, Dado40 = miss40_ARMA1.100$Dado,
                           Coluna.controle = ARMA11a.100)
write.csv(arma.100,file ="ARMA.100.csv", row.names = FALSE)
