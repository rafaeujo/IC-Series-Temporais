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

mmiss05_AR01 <-delete_MAR_censoring(mdataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR01 <-delete_MAR_censoring(mdataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR01 <-delete_MAR_censoring(mdataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR01 <-delete_MAR_censoring(mdataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

##Funcao para calculo de Rmsd e vicio

library(Metrics)

metricas_mean <- function(alfa,data, coluna, antigo) {
  
  data[[coluna]][is.na(data[[coluna]])] <- mean(data[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b))
}

#Calculando RMSE

mmedidasAR01.05 <- metricas_mean(5,mmiss05_AR01, "Dado",mdataAR01$Dado)
mmedidasAR01.10 <- metricas_mean(10,mmiss10_AR01, "Dado",mdataAR01$Dado)
mmedidasAR01.20 <- metricas_mean(20,mmiss20_AR01, "Dado",mdataAR01$Dado)
mmedidasAR01.40 <- metricas_mean(40,mmiss40_AR01, "Dado",mdataAR01$Dado)

##Tabelando os valores

mAR01.tab5 <- unname(unlist(mmedidasAR01.05))
mAR01.tab10 <- unname(unlist(mmedidasAR01.10))
mAR01.tab20  <- unname(unlist(mmedidasAR01.20))
mAR01.tab40  <- unname(unlist(mmedidasAR01.40))


##Gerando Modelo AR1 (phi = 0.6)

nAR01a <-  arima.sim(n = 1000, list(ar = c(0.6)))
nAR01b <-  arima.sim(n = 1000, list(ar = c(0.6)))

##Imputando dados faltantes

library(missMethods)

ndataAR01 <- data.frame(Dado = nAR01a, Coluna.controle = nAR01b)

nmiss05_AR01 <-delete_MAR_censoring(ndataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
nmiss10_AR01 <-delete_MAR_censoring(ndataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
nmiss20_AR01 <-delete_MAR_censoring(ndataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
nmiss40_AR01 <-delete_MAR_censoring(ndataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

nmedidasAR01.05 <- metricas_mean(5,nmiss05_AR01, "Dado",ndataAR01$Dado)
nmedidasAR01.10 <- metricas_mean(10,nmiss10_AR01, "Dado",ndataAR01$Dado)
nmedidasAR01.20 <- metricas_mean(20,nmiss20_AR01, "Dado",ndataAR01$Dado)
nmedidasAR01.40 <- metricas_mean(40,nmiss40_AR01, "Dado",ndataAR01$Dado)

##Tabelando os valores

nAR01.tab5 <- unname(unlist(nmedidasAR01.05))
nAR01.tab10 <- unname(unlist(nmedidasAR01.10))
nAR01.tab20  <- unname(unlist(nmedidasAR01.20))
nAR01.tab40  <- unname(unlist(nmedidasAR01.40))

miltabelaAR01 <- data.frame(Porcentagem = c(mAR01.tab5[1],mAR01.tab10[1],mAR01.tab20[1],mAR01.tab40[1]), 
                            Rmse04 = c(mAR01.tab5[2],mAR01.tab10[2],mAR01.tab20[2],mAR01.tab40[2]),
                            Rmse06 = c(nAR01.tab5[2],nAR01.tab10[2],nAR01.tab20[2],nAR01.tab40[2]))

##Gerando Modelo AR2 (0,4; 0,2)

mAR02a <-  arima.sim(n = 1000, list(ar = c(0.4,0.2)))
mAR02b <-  arima.sim(n = 1000, list(ar = c(0.4,0.2)))

##Imputando dados faltantes

library(missMethods)

mdataAR02 <- data.frame(Dado = mAR02a, Coluna.controle = mAR02b)

mmiss05_AR02 <-delete_MAR_censoring(mdataAR02, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02 <-delete_MAR_censoring(mdataAR02, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02 <-delete_MAR_censoring(mdataAR02, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02 <-delete_MAR_censoring(mdataAR02, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

mmedidasAR02.05 <- metricas_mean(5,mmiss05_AR02, "Dado",mdataAR02$Dado)
mmedidasAR02.10 <- metricas_mean(10,mmiss10_AR02, "Dado",mdataAR02$Dado)
mmedidasAR02.20 <- metricas_mean(20,mmiss20_AR02, "Dado",mdataAR02$Dado)
mmedidasAR02.40 <- metricas_mean(40,mmiss40_AR02, "Dado",mdataAR02$Dado)

##Gerando Modelo AR2 (0,4; 0,5)

mAR02c <-  arima.sim(n = 1000, list(ar = c(0.4,0.5)))
mAR02d <-  arima.sim(n = 1000, list(ar = c(0.4,0.5)))

##Imputando dados faltantes

mdataAR02_2 <- data.frame(Dado = mAR02c, Coluna.controle = mAR02d)

mmiss05_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

mmedidasAR02_2.05 <- metricas_mean(5,mmiss05_AR02_2, "Dado",mdataAR02_2$Dado)
mmedidasAR02_2.10 <- metricas_mean(10,mmiss10_AR02_2, "Dado",mdataAR02_2$Dado)
mmedidasAR02_2.20 <- metricas_mean(20,mmiss20_AR02_2, "Dado",mdataAR02_2$Dado)
mmedidasAR02_2.40 <- metricas_mean(40,mmiss40_AR02_2, "Dado",mdataAR02_2$Dado)

##Gerando Modelo ARMA (0,4; 0,2)

ARMA11a <-  arima.sim(n = 1000, list(ar = c(0.4), ma = c(0.2)))
ARMA11b <-  arima.sim(n = 1000, list(ar = c(0.4), ma = c(0.2)))

##Imputando dados faltantes

mdataARMA1 <- data.frame(Dado = ARMA11a, Coluna.controle = ARMA11b)

miss05_ARMA1 <-delete_MAR_censoring(mdataARMA1, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1 <-delete_MAR_censoring(mdataARMA1, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_ARMA1 <-delete_MAR_censoring(mdataARMA1, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_ARMA1 <-delete_MAR_censoring(mdataARMA1, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando RMSE

medidasARMA1.05 <- metricas_mean(5,miss05_ARMA1, "Dado",mdataARMA1$Dado)
medidasARMA1.10 <- metricas_mean(10,miss10_ARMA1, "Dado",mdataARMA1$Dado)
medidasARMA1.20 <- metricas_mean(20,miss20_ARMA1, "Dado",mdataARMA1$Dado)
medidasARMA1.40 <- metricas_mean(40,miss40_ARMA1, "Dado",mdataARMA1$Dado)

##Tabelando os valores

mAR02.tab5 <- unname(unlist(mmedidasAR02.05))
mAR02.tab10 <- unname(unlist(mmedidasAR02.10))
mAR02.tab20  <- unname(unlist(mmedidasAR02.20))
mAR02.tab40  <- unname(unlist(mmedidasAR02.40))
mAR02.tab5_2 <- unname(unlist(mmedidasAR02_2.05))
mAR02.tab10_2 <- unname(unlist(mmedidasAR02_2.10))
mAR02.tab20_2  <- unname(unlist(mmedidasAR02_2.20))
mAR02.tab40_2  <- unname(unlist(mmedidasAR02_2.40))
mARMA1.tab5 <- unname(unlist(medidasARMA1.05))
mARMA1.tab10 <- unname(unlist(medidasARMA1.10))
mARMA1.tab20  <- unname(unlist(medidasARMA1.20))
mARMA1.tab40  <- unname(unlist(medidasARMA1.40))


mtabelaARGeral <- data.frame(Porcentagem = c(mAR02.tab5[1],mAR02.tab10[1],mAR02.tab20[1],mAR02.tab40[1]), 
                             AR.Rmse.04 = c(mAR01.tab5[2],mAR01.tab10[2],mAR01.tab20[2],mAR01.tab40[2]),
                             AR.Rmse.06 = c(nAR01.tab5[2],nAR01.tab10[2],nAR01.tab20[2],nAR01.tab40[2]),
                             AR.Rmse.0204 = c(mAR02.tab5[2],mAR02.tab10[2],mAR02.tab20[2],mAR02.tab40[2]),
                             AR.Rmse.0405 = c(mAR02.tab5_2[2],mAR02.tab10_2[2],mAR02.tab20_2[2],mAR02.tab40_2[2]),
                             ARMA.Rmse.0402 = c(mARMA1.tab5[2],mARMA1.tab10[2],mARMA1.tab20[2],mARMA1.tab40[2]))

#Gerando os Gráficos

x11()
par(mfrow=c(3,2))
ts.plot(mAR01a)
ts.plot(nAR01a)
ts.plot(mAR02a)
ts.plot(mAR02c)
ts.plot(ARMA11a)

