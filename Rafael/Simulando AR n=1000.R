##Modelando com n = 1000

##Gerando Modelo AR1
set.seed(42)

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
  
  # Calcula o vício
  VA <- bias(antigo,data[[coluna]])
  
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSE" = b, "Vicio" = VA))
}

#Calculando rmmsd e vicio

mmedidasAR01.05 <- metricas_mean(5,mmiss05_AR01, "Dado",mdataAR01$Dado)
mmedidasAR01.10 <- metricas_mean(10,mmiss10_AR01, "Dado",mdataAR01$Dado)
mmedidasAR01.20 <- metricas_mean(20,mmiss20_AR01, "Dado",mdataAR01$Dado)
mmedidasAR01.40 <- metricas_mean(40,mmiss40_AR01, "Dado",mdataAR01$Dado)

##Tabelando os valores

mAR01.tab5 <- unname(unlist(mmedidasAR01.05))
mAR01.tab10 <- unname(unlist(mmedidasAR01.10))
mAR01.tab20  <- unname(unlist(mmedidasAR01.20))
mAR01.tab40  <- unname(unlist(mmedidasAR01.40))

miltabelaAR01 <- data.frame(Porcentagem = c(mAR01.tab5[1],mAR01.tab10[1],mAR01.tab20[1],mAR01.tab40[1]), 
                         Rmsd = c(mAR01.tab5[2],mAR01.tab10[2],mAR01.tab20[2],mAR01.tab40[2]),
                         Vicio = c(mAR01.tab5[3],mAR01.tab10[3],mAR01.tab20[3],mAR01.tab40[3]))

##Gerando Modelo AR2 (0,4; 0,2)

mAR02a <-  arima.sim(n = 100, list(ar = c(0.4,0.2)))
mAR02b <-  arima.sim(n = 100, list(ar = c(0.4,0.2)))

##Imputando dados faltantes

library(missMethods)

mdataAR02 <- data.frame(Dado = mAR02a, Coluna.controle = mAR02b)

mmiss05_AR02 <-delete_MAR_censoring(mdataAR02, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02 <-delete_MAR_censoring(mdataAR02, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02 <-delete_MAR_censoring(mdataAR02, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02 <-delete_MAR_censoring(mdataAR02, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando rmmsd e vicio

mmedidasAR02.05 <- metricas_mean(5,miss05_AR02, "Dado",dataAR02$Dado)
mmedidasAR02.10 <- metricas_mean(10,miss10_AR02, "Dado",dataAR02$Dado)
mmedidasAR02.20 <- metricas_mean(20,miss20_AR02, "Dado",dataAR02$Dado)
mmedidasAR02.40 <- metricas_mean(40,miss40_AR02, "Dado",dataAR02$Dado)

##Gerando Modelo AR2 (0,4; 0,5)

mAR02c <-  arima.sim(n = 100, list(ar = c(0.4,0.5)))
mAR02d <-  arima.sim(n = 100, list(ar = c(0.4,0.5)))

##Imputando dados faltantes


mdataAR02_2 <- data.frame(Dado = mAR02c, Coluna.controle = mAR02d)

mmiss05_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR02_2 <-delete_MAR_censoring(mdataAR02_2, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando rmmsd e vicio

mmedidasAR02_2.05 <- metricas_mean(5,mmiss05_AR02_2, "Dado",mdataAR02_2$Dado)
mmedidasAR02_2.10 <- metricas_mean(10,mmiss10_AR02_2, "Dado",mdataAR02_2$Dado)
mmedidasAR02_2.20 <- metricas_mean(20,mmiss20_AR02_2, "Dado",mdataAR02_2$Dado)
mmedidasAR02_2.40 <- metricas_mean(40,mmiss40_AR02_2, "Dado",mdataAR02_2$Dado)

##Tabelando os valores

mAR02.tab5 <- unname(unlist(mmedidasAR02.05))
mAR02.tab10 <- unname(unlist(mmedidasAR02.10))
mAR02.tab20  <- unname(unlist(mmedidasAR02.20))
mAR02.tab40  <- unname(unlist(mmedidasAR02.40))
mAR02.tab5_2 <- unname(unlist(mmedidasAR02_2.05))
mAR02.tab10_2 <- unname(unlist(mmedidasAR02_2.10))
mAR02.tab20_2  <- unname(unlist(mmedidasAR02_2.20))
mAR02.tab40_2  <- unname(unlist(mmedidasAR02_2.40))

mtabelaAR02 <- data.frame(Porcentagem = c(mAR02.tab5[1],mAR02.tab10[1],mAR02.tab20[1],mAR02.tab40[1]), 
                         Rmsd.0204 = c(mAR02.tab5[2],mAR02.tab10[2],mAR02.tab20[2],mAR02.tab40[2]),
                         Rmsd.0405 = c(mAR02.tab5_2[2],mAR02.tab10_2[2],mAR02.tab20_2[2],mAR02.tab40_2[2]),
                         Vicio.0204 = c(mAR02.tab5[3],mAR02.tab10[3],mAR02.tab20[3],mAR02.tab40[3]),
                         Vicio.0405 = c(mAR02.tab5_2[3],mAR02.tab10_2[3],mAR02.tab20_2[3],mAR02.tab40_2[3]))

mtabelaARGeral <- data.frame(Porcentagem = c(mAR02.tab5[1],mAR02.tab10[1],mAR02.tab20[1],mAR02.tab40[1]), 
                            Rmsd.04 = c(mAR01.tab5[2],mAR01.tab10[2],mAR01.tab20[2],mAR01.tab40[2]),
                            Rmsd.0204 = c(mAR02.tab5[2],mAR02.tab10[2],mAR02.tab20[2],mAR02.tab40[2]),
                            Rmsd.0405 = c(mAR02.tab5_2[2],mAR02.tab10_2[2],mAR02.tab20_2[2],mAR02.tab40_2[2]),
                            Vicio.04 = c(mAR01.tab5[3],mAR01.tab10[3],mAR01.tab20[3],mAR01.tab40[3]),
                            Vicio.0204 = c(mAR02.tab5[3],mAR02.tab10[3],mAR02.tab20[3],mAR02.tab40[3]),
                            Vicio.0405 = c(mAR02.tab5_2[3],mAR02.tab10_2[3],mAR02.tab20_2[3],mAR02.tab40_2[3]))




