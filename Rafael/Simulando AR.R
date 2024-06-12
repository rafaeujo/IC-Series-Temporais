##Gerando Modelo AR1
set.seed(1)

AR01a <-  arima.sim(n = 100, list(ar = c(0.4)))
AR01b <-  arima.sim(n = 100, list(ar = c(0.4)))

##Graficos das AR's

x11()
par(mfrow=c(2,1))
ts.plot(AR01a)
ts.plot(AR02a)
ts.plot(AR02c)
ts.plot(AR02d)

##Imputando dados faltantes

library(missMethods)

dataAR01 <- data.frame(Dado = AR01a, Coluna.controle = AR01b)

miss05_AR01 <-delete_MAR_censoring(dataAR01, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_AR01 <-delete_MAR_censoring(dataAR01, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_AR01 <-delete_MAR_censoring(dataAR01, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_AR01 <-delete_MAR_censoring(dataAR01, 0.40, "Dado", cols_ctrl = "Coluna.controle")

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

medidasAR01.05 <- metricas_mean(5,miss05_AR01, "Dado",dataAR01$Dado)
medidasAR01.10 <- metricas_mean(10,miss10_AR01, "Dado",dataAR01$Dado)
medidasAR01.20 <- metricas_mean(20,miss20_AR01, "Dado",dataAR01$Dado)
medidasAR01.40 <- metricas_mean(40,miss40_AR01, "Dado",dataAR01$Dado)

##Tabelando os valores

AR01.tab5 <- unname(unlist(medidasAR01.05))
AR01.tab10 <- unname(unlist(medidasAR01.10))
AR01.tab20  <- unname(unlist(medidasAR01.20))
AR01.tab40  <- unname(unlist(medidasAR01.40))

tabelaAR01 <- data.frame(Porcentagem = c(AR01.tab5[1],AR01.tab10[1],AR01.tab20[1],AR01.tab40[1]), 
                     Rmsd = c(AR01.tab5[2],AR01.tab10[2],AR01.tab20[2],AR01.tab40[2]),
                     Vicio = c(AR01.tab5[3],AR01.tab10[3],AR01.tab20[3],AR01.tab40[3]))

##Gerando Modelo AR2 (0,4; 0,2)

AR02a <-  arima.sim(n = 100, list(ar = c(0.4,0.2)))
AR02b <-  arima.sim(n = 100, list(ar = c(0.4,0.2)))

##Imputando dados faltantes

library(missMethods)

dataAR02 <- data.frame(Dado = AR02a, Coluna.controle = AR02b)

miss05_AR02 <-delete_MAR_censoring(dataAR02, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_AR02 <-delete_MAR_censoring(dataAR02, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_AR02 <-delete_MAR_censoring(dataAR02, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_AR02 <-delete_MAR_censoring(dataAR02, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando rmmsd e vicio

medidasAR02.05 <- metricas_mean(5,miss05_AR02, "Dado",dataAR02$Dado)
medidasAR02.10 <- metricas_mean(10,miss10_AR02, "Dado",dataAR02$Dado)
medidasAR02.20 <- metricas_mean(20,miss20_AR02, "Dado",dataAR02$Dado)
medidasAR02.40 <- metricas_mean(40,miss40_AR02, "Dado",dataAR02$Dado)

##Gerando Modelo AR2 (0,4; 0,5)

AR02c <-  arima.sim(n = 100, list(ar = c(0.4,0.5)))
AR02d <-  arima.sim(n = 100, list(ar = c(0.4,0.5)))

##Imputando dados faltantes


dataAR02_2 <- data.frame(Dado = AR02c, Coluna.controle = AR02d)

miss05_AR02_2 <-delete_MAR_censoring(dataAR02_2, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_AR02_2 <-delete_MAR_censoring(dataAR02_2, 0.10, "Dado", cols_ctrl = "Coluna.controle")
miss20_AR02_2 <-delete_MAR_censoring(dataAR02_2, 0.20, "Dado", cols_ctrl = "Coluna.controle")
miss40_AR02_2 <-delete_MAR_censoring(dataAR02_2, 0.40, "Dado", cols_ctrl = "Coluna.controle")

#Calculando rmmsd e vicio

medidasAR02_2.05 <- metricas_mean(5,miss05_AR02_2, "Dado",dataAR02_2$Dado)
medidasAR02_2.10 <- metricas_mean(10,miss10_AR02_2, "Dado",dataAR02_2$Dado)
medidasAR02_2.20 <- metricas_mean(20,miss20_AR02_2, "Dado",dataAR02_2$Dado)
medidasAR02_2.40 <- metricas_mean(40,miss40_AR02_2, "Dado",dataAR02_2$Dado)

##Tabelando os valores

AR02.tab5 <- unname(unlist(medidasAR02.05))
AR02.tab10 <- unname(unlist(medidasAR02.10))
AR02.tab20  <- unname(unlist(medidasAR02.20))
AR02.tab40  <- unname(unlist(medidasAR02.40))
AR02.tab5_2 <- unname(unlist(medidasAR02_2.05))
AR02.tab10_2 <- unname(unlist(medidasAR02_2.10))
AR02.tab20_2  <- unname(unlist(medidasAR02_2.20))
AR02.tab40_2  <- unname(unlist(medidasAR02_2.40))

tabelaAR02 <- data.frame(Porcentagem = c(AR02.tab5[1],AR02.tab10[1],AR02.tab20[1],AR02.tab40[1]), 
                         Rmsd.0204 = c(AR02.tab5[2],AR02.tab10[2],AR02.tab20[2],AR02.tab40[2]),
                         Rmsd.0405 = c(AR02.tab5_2[2],AR02.tab10_2[2],AR02.tab20_2[2],AR02.tab40_2[2]),
                         Vicio.0204 = c(AR02.tab5[3],AR02.tab10[3],AR02.tab20[3],AR02.tab40[3]),
                         Vicio.0405 = c(AR02.tab5_2[3],AR02.tab10_2[3],AR02.tab20_2[3],AR02.tab40_2[3]))

tabelaARGeral <- data.frame(Porcentagem = c(AR02.tab5[1],AR02.tab10[1],AR02.tab20[1],AR02.tab40[1]), 
                            Rmsd.04 = c(AR01.tab5[2],AR01.tab10[2],AR01.tab20[2],AR01.tab40[2]),
                            Rmsd.0204 = c(AR02.tab5[2],AR02.tab10[2],AR02.tab20[2],AR02.tab40[2]),
                            Rmsd.0405 = c(AR02.tab5_2[2],AR02.tab10_2[2],AR02.tab20_2[2],AR02.tab40_2[2]),
                            Vicio.04 = c(AR01.tab5[3],AR01.tab10[3],AR01.tab20[3],AR01.tab40[3]),
                            Vicio.0204 = c(AR02.tab5[3],AR02.tab10[3],AR02.tab20[3],AR02.tab40[3]),
                            Vicio.0405 = c(AR02.tab5_2[3],AR02.tab10_2[3],AR02.tab20_2[3],AR02.tab40_2[3]))
                            
x11()
par(mfrow=c(3,1))
ts.plot(AR01a)
ts.plot(AR02a)
ts.plot(AR02c)



