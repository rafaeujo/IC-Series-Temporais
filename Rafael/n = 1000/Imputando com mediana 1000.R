##Modelando com n = 1000

##Gerando Modelo AR1 (phi = 0.4)
set.seed(31)

##Legenda
# mAR01 tem phi = 0,4
# nAR01 tem phi = 0,6

mAR01a <-  arima.sim(n = 1000, list(ar = c(0.4)))
mAR01b <-  arima.sim(n = 1000, list(ar = c(0.4)))

##Imputando dados faltantes

library(missMethods)

mdataAR01 <- data.frame(Dado = mAR01a, Coluna.controle = mAR01b)

mmiss05_AR01 <- delete_MAR_censoring(mdataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR01 <- delete_MAR_censoring(mdataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR01 <- delete_MAR_censoring(mdataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR01 <- delete_MAR_censoring(mdataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

##Funcao para calculo de Rmsd e vicio

library(Metrics)

metricas_med <- function(alfa, data, coluna, antigo) {
  
  data[[coluna]][is.na(data[[coluna]])] <- median(data[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}

#Calculando RMSE

mmedidasAR01.05.med <- metricas_med(5, mmiss05_AR01, "Dado", mdataAR01$Dado)
mmedidasAR01.10.med <- metricas_med(10, mmiss10_AR01, "Dado", mdataAR01$Dado)
mmedidasAR01.20.med <- metricas_med(20, mmiss20_AR01, "Dado", mdataAR01$Dado)
mmedidasAR01.40.med <- metricas_med(40, mmiss40_AR01, "Dado", mdataAR01$Dado)

##Tabelando os valores

mAR01.tab5.med <- unname(unlist(mmedidasAR01.05.med))
mAR01.tab10.med <- unname(unlist(mmedidasAR01.10.med))
mAR01.tab20.med  <- unname(unlist(mmedidasAR01.20.med))
mAR01.tab40.med  <- unname(unlist(mmedidasAR01.40.med))


##Gerando Modelo AR1 (phi = 0.6)

nAR01a <-  arima.sim(n = 1000, list(ar = c(0.6)))
nAR01b <-  arima.sim(n = 1000, list(ar = c(0.6)))

##Imputando dados faltantes

library(missMethods)

ndataAR01 <- data.frame(Dado = nAR01a, Coluna.controle = nAR01b)

nmiss05_AR01 <- delete_MAR_censoring(ndataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
nmiss10_AR01 <- delete_MAR_censoring(ndataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
nmiss20_AR01 <- delete_MAR_censoring(ndataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
nmiss40_AR01 <- delete_MAR_censoring(ndataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

nmedidasAR01.05.med <- metricas_med(5, nmiss05_AR01, "Dado", ndataAR01$Dado)
nmedidasAR01.10.med <- metricas_med(10, nmiss10_AR01, "Dado", ndataAR01$Dado)
nmedidasAR01.20.med <- metricas_med(20, nmiss20_AR01, "Dado", ndataAR01$Dado)
nmedidasAR01.40.med <- metricas_med(40, nmiss40_AR01, "Dado", ndataAR01$Dado)

##Tabelando os valores

nAR01.tab5.med <- unname(unlist(nmedidasAR01.05.med))
nAR01.tab10.med <- unname(unlist(nmedidasAR01.10.med))
nAR01.tab20.med  <- unname(unlist(nmedidasAR01.20.med))
nAR01.tab40.med  <- unname(unlist(nmedidasAR01.40.med))

miltabelaAR01.med <- data.frame(Porcentagem = c(mAR01.tab5.med[1], mAR01.tab10.med[1], mAR01.tab20.med[1], mAR01.tab40.med[1]), 
                                Rmse04.med = c(mAR01.tab5.med[2], mAR01.tab10.med[2], mAR01.tab20.med[2], mAR01.tab40.med[2]),
                                Rmse06.med = c(nAR01.tab5.med[2], nAR01.tab10.med[2], nAR01.tab20.med[2], nAR01.tab40.med[2]))

##Gerando Modelo AR2 (0,4; 0,2)

mAR02a <-  arima.sim(n = 1000, list(ar = c(0.4, 0.2)))
mAR02b <-  arima.sim(n = 1000, list(ar = c(0.4, 0.2)))

##Imputando dados faltantes

library(missMethods)

mdataAR02 <- data.frame(Dado = mAR02a, Coluna.controle = mAR02b)

mmiss05_AR02 <- delete_MAR_censoring(mdataAR02, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02 <- delete_MAR_censoring(mdataAR02, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02 <- delete_MAR_censoring(mdataAR02, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02 <- delete_MAR_censoring(mdataAR02, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

mmedidasAR02.05.med <- metricas_med(5, mmiss05_AR02, "Dado", mdataAR02$Dado)
mmedidasAR02.10.med <- metricas_med(10, mmiss10_AR02, "Dado", mdataAR02$Dado)
mmedidasAR02.20.med <- metricas_med(20, mmiss20_AR02, "Dado", mdataAR02$Dado)
mmedidasAR02.40.med <- metricas_med(40, mmiss40_AR02, "Dado", mdataAR02$Dado)

##Gerando Modelo AR2 (0,4; 0,5)

mAR02c <-  arima.sim(n = 1000, list(ar = c(0.4, 0.5)))
mAR02d <-  arima.sim(n = 1000, list(ar = c(0.4, 0.5)))

##Imputando dados faltantes

mdataAR02_2 <- data.frame(Dado = mAR02c, Coluna.controle = mAR02d)

mmiss05_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02_2 <- delete_MAR_censoring(mdataAR02_2, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

mmedidasAR02_2.05.med <- metricas_med(5, mmiss05_AR02_2, "Dado", mdataAR02_2$Dado)
mmedidasAR02_2.10.med <- metricas_med(10, mmiss10_AR02_2, "Dado", mdataAR02_2$Dado)
mmedidasAR02_2.20.med <- metricas_med(20, mmiss20_AR02_2, "Dado", mdataAR02_2$Dado)
mmedidasAR02_2.40.med <- metricas_med(40, mmiss40_AR02_2, "Dado", mdataAR02_2$Dado)

##Gerando Modelo ARMA (0,4; 0,2)

ARMA11a <-  arima.sim(n = 1000, list(ar = c(0.4), ma = c(0.2)))
ARMA11b <-  arima.sim(n = 1000, list(ar = c(0.4), ma = c(0.2)))

##Imputando dados faltantes

mdataARMA1 <- data.frame(Dado = ARMA11a, Coluna.controle = ARMA11b)

miss05_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_ARMA1 <- delete_MAR_censoring(mdataARMA1, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

medidasARMA1.05.med <- metricas_med(5, miss05_ARMA1, "Dado", mdataARMA1$Dado)
medidasARMA1.10.med <- metricas_med(10, miss10_ARMA1, "Dado", mdataARMA1$Dado)
medidasARMA1.20.med <- metricas_med(20, miss20_ARMA1, "Dado", mdataARMA1$Dado)
medidasARMA1.40.med <- metricas_med(40, miss40_ARMA1, "Dado", mdataARMA1$Dado)

##Tabelando os valores

mAR02.tab5.med <- unname(unlist(mmedidasAR02.05.med))
mAR02.tab10.med <- unname(unlist(mmedidasAR02.10.med))
mAR02.tab20.med  <- unname(unlist(mmedidasAR02.20.med))
mAR02.tab40.med  <- unname(unlist(mmedidasAR02.40.med))
mAR02.tab5_2.med <- unname(unlist(mmedidasAR02_2.05.med))
mAR02.tab10_2.med <- unname(unlist(mmedidasAR02_2.10.med))
mAR02.tab20_2.med  <- unname(unlist(mmedidasAR02_2.20.med))
mAR02.tab40_2.med  <- unname(unlist(mmedidasAR02_2.40.med))
mARMA1.tab5.med <- unname(unlist(medidasARMA1.05.med))
mARMA1.tab10.med <- unname(unlist(medidasARMA1.10.med))
mARMA1.tab20.med  <- unname(unlist(medidasARMA1.20.med))
mARMA1.tab40.med  <- unname(unlist(medidasARMA1.40.med))


mtabelaARGeral.med <- data.frame(Porcentagem = c(mAR02.tab5.med[1], mAR02.tab10.med[1], mAR02.tab20.med[1], mAR02.tab40.med[1]), 
                                 AR.Rmse.04.med = c(mAR01.tab5.med[2], mAR01.tab10.med[2], mAR01.tab20.med[2], mAR01.tab40.med[2]),
                                 AR.Rmse.06.med = c(nAR01.tab5.med[2], nAR01.tab10.med[2], nAR01.tab20.med[2], nAR01.tab40.med[2]),
                                 AR.Rmse.0204.med = c(mAR02.tab5.med[2], mAR02.tab10.med[2], mAR02.tab20.med[2], mAR02.tab40.med[2]),
                                 AR.Rmse.0405.med = c(mAR02.tab5_2.med[2], mAR02.tab10_2.med[2], mAR02.tab20_2.med[2], mAR02.tab40_2.med[2]),
                                 ARMA.Rmse.0402.med = c(mARMA1.tab5.med[2], mARMA1.tab10.med[2], mARMA1.tab20.med[2], mARMA1.tab40.med[2]))

#Gerando os Gráficos

x11()
par(mfrow=c(3,2))
ts.plot(mAR01a)
ts.plot(nAR01a)
ts.plot(mAR02a)
ts.plot(mAR02c)
ts.plot(ARMA11a)
