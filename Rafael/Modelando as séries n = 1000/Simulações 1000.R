##Simulando os modelos ARIMA

library(missMethods)
set.seed(2003)

## Gerando Modelo AR1 (phi = 0.4)
mAR01a <- arima.sim(n = 1000, list(ar = c(0.4)))
mdataAR01 <- data.frame(Dado = mAR01a, Coluna.controle = mAR01a)

mmiss05_AR01 <- delete_MAR_censoring(mdataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR01 <- delete_MAR_censoring(mdataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR01 <- delete_MAR_censoring(mdataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR01 <- delete_MAR_censoring(mdataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar1.04 <- data.frame(Dado5 = mmiss05_AR01$Dado, Dado10 = mmiss10_AR01$Dado,
                         Dado20 = mmiss20_AR01$Dado, Dado40 = mmiss40_AR01$Dado,
                         Coluna.controle = mAR01a)
write.csv(ar1.04,file ="AR1.04.csv", row.names = FALSE)

## Gerando Modelo AR1 (phi = 0.6)
nAR01a <- arima.sim(n = 1000, list(ar = c(0.6)))
ndataAR01 <- data.frame(Dado = nAR01a, Coluna.controle = nAR01a)

nmiss05_AR01 <- delete_MAR_censoring(ndataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
nmiss10_AR01 <- delete_MAR_censoring(ndataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
nmiss20_AR01 <- delete_MAR_censoring(ndataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
nmiss40_AR01 <- delete_MAR_censoring(ndataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar1.06 <- data.frame(Dado5 = nmiss05_AR01$Dado, Dado10 = nmiss10_AR01$Dado,
                         Dado20 = nmiss20_AR01$Dado, Dado40 = nmiss40_AR01$Dado,
                         Coluna.controle = nAR01a)
write.csv(ar1.06,file ="AR1.06.csv", row.names = FALSE)

## Gerando Modelo AR2 (0.4; 0.2)
mAR02a <- arima.sim(n = 1000, list(ar = c(0.4, 0.2)))
mdataAR02 <- data.frame(Dado = mAR02a, Coluna.controle = mAR02a)

mmiss05_AR02 <- delete_MAR_censoring(mdataAR02, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02 <- delete_MAR_censoring(mdataAR02, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02 <- delete_MAR_censoring(mdataAR02, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02 <- delete_MAR_censoring(mdataAR02, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar2.0402 <- data.frame(Dado5 = mmiss05_AR02$Dado, Dado10 = mmiss10_AR02$Dado,
                           Dado20 = mmiss20_AR02$Dado, Dado40 = mmiss40_AR02$Dado,
                           Coluna.controle = mAR02a)
write.csv(ar2.0402,file ="AR2.0402.csv", row.names = FALSE)

## Gerando Modelo AR2 (0.4; 0.5)
mAR02c <- arima.sim(n = 1000, list(ar = c(0.4, 0.5)))
mdataAR02_2 <- data.frame(Dado = mAR02c, Coluna.controle = mAR02c)

mmiss05_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.40, "Dado", cols_ctrl = "Coluna.controle")

ar2.0405 <- data.frame(Dado5 = mmiss05_AR02_2$Dado, Dado10 = mmiss10_AR02_2$Dado,
                           Dado20 = mmiss20_AR02_2$Dado, Dado40 = mmiss40_AR02_2$Dado,
                           Coluna.controle = mAR02c)
write.csv(ar2.0405,file ="AR2.0405.csv", row.names = FALSE)

## Gerando Modelo ARMA (0.4; 0.2)
ARMA11a <- arima.sim(n = 1000, list(ar = c(0.4), ma = c(0.2)))
mdataARMA1 <- data.frame(Dado = ARMA11a, Coluna.controle = ARMA11a)

miss05_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.40, "Dado", cols_ctrl = "Coluna.controle")


arma <- data.frame(Dado5 = miss05_ARMA1$Dado, Dado10 = miss10_ARMA1$Dado,
                       Dado20 = miss20_ARMA1$Dado, Dado40 = miss40_ARMA1$Dado,
                       Coluna.controle = ARMA11a)
write.csv(arma,file ="ARMA.csv", row.names = FALSE)
