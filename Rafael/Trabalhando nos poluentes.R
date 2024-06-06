XI <- read.table("Data_CPDO.txt")

ts.plot(XI$CO)
ts.plot(XI$PM10)
ts.plot(XI$NO)
ts.plot(XI$NO2)
ts.plot(XI$NOX)
ts.plot(XI$O3)

x11()
par(mfrow=c(2,1))
acf(XI$NO)
pacf(XI$NO)

fitNO <- arima(XI$NO,order=c(1,0,1))
x11()
par(mfrow=c(2,1))
acf(fitNO$residuals)
pacf(fitNO$residuals)

x11()
par(mfrow=c(2,1))
acf(XI$NO2)
pacf(XI$NO2)

fitNO2 <- arima(XI$NO2,order=c(1,0,1))
x11()
par(mfrow=c(2,1))
acf(fitNO2$residuals)
pacf(fitNO2$residuals)

x11()
par(mfrow=c(2,1))
acf(XI$CO)
pacf(XI$CO)

fitCO <- arima(XI$CO,order=c(1,0,1))
x11()
par(mfrow=c(2,1))
acf(fitCO$residuals)
pacf(fitCO$residuals)

x11()
par(mfrow=c(2,1))
acf(XI$PM10)
pacf(XI$PM10)

fitPM10 <- arima(XI$PM10,order=c(1,0,1))
x11()
par(mfrow=c(2,1))
acf(fitPM10$residuals)
pacf(fitPM10$residuals)

x11()
par(mfrow=c(2,1))
acf(XI$NOX)
pacf(XI$NOX)

fitNOX <- arima(XI$NOX,order=c(1,0,1))
x11()
par(mfrow=c(2,1))
acf(fitNOX$residuals)
pacf(fitNOX$residuals)

x11()
par(mfrow=c(2,1))
acf(XI$O3)
pacf(XI$O3)

fitO3 <- arima(XI$O3,order=c(1,0,1))
x11()
par(mfrow=c(2,1))
acf(fitO3$residuals)
pacf(fitO3$residuals)

##Missing data

install.packages("missMethods")
library(missMethods)

Xi <- data.frame(XI)

Xi_mar05 <-delete_MAR_censoring(Xi, 0.05, "CO", cols_ctrl = "O3")
Xi_mar10 <-delete_MAR_censoring(Xi, 0.1, "CO", cols_ctrl = "O3")
Xi_mar20 <-delete_MAR_censoring(Xi, 0.2, "CO", cols_ctrl = "O3")
Xi_mar40 <-delete_MAR_censoring(Xi, 0.4, "CO", cols_ctrl = "O3")

meanXI <- mean(Xi$CO)
meanXi_mar05 <- Xi_mar05
meanXi_mar10 <- Xi_mar10
meanXi_mar20 <- Xi_mar20
meanXi_mar40 <- Xi_mar40
exemplo <- Xi_mar05
exemplo2 <- Xi_mar05

medXI <- median(XI$CO)
medXi_mar05 <- Xi_mar05
medXi_mar10 <- Xi_mar10
medXi_mar20 <- Xi_mar20
medXi_mar40 <- Xi_mar40

##Calculando RMSD e vício com média no NA

install.packages('Metrics')
library('Metrics')

##Funções

metricas_mean <- function(alfa,data, coluna, antigo) {
 
  data[[coluna]][is.na(data[[coluna]])] <- mean(data[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data[[coluna]])
  RMSD <- sqrt(b)
  
  # Calcula o vício
  VA <- bias(antigo,data[[coluna]])
  
  
  # Retorna as métricas calculadas
  return(list("Media para" = alfa, "RMSD" = RMSD, "Vicio" = VA))
}

metricas_med <- function(alfa,data, coluna, antigo) {
  
  data[[coluna]][is.na(data[[coluna]])] <- median(data[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data[[coluna]])
  RMSD <- sqrt(b)
  
  # Calcula o vício
  VA <- bias(antigo,data[[coluna]])
  
  
  # Retorna as métricas calculadas
  return(list("Mediana para:" = alfa, "RMSD" = RMSD, "Vicio" = VA))
}

## Resultados

amem5A <- metricas_mean(5,meanXi_mar05,"CO",XI$CO)
amem5B <- metricas_med(5,medXi_mar05,"CO",XI$CO)
amem5A
amem5B

amem10A <- metricas_mean(10,meanXi_mar10,"CO",XI$CO)
amem10B <-metricas_med(10,medXi_mar10,"CO",XI$CO)
amem10A
amem10B

amem20A <-metricas_mean(20,meanXi_mar20,"CO",XI$CO)
amem20B<- metricas_med(20,medXi_mar20,"CO",XI$CO)
amem20A
amem20B

amem40A <-metricas_mean(40,meanXi_mar40,"CO",XI$CO)
amem40B <-metricas_med(40,medXi_mar40,"CO",XI$CO)
amem40A
amem40B

##Tabelando os valores

tab5A <- unname(unlist(amem5A))
tab5B <- unname(unlist(amem5B))
tab10A <- unname(unlist(amem10A))
tab10B <- unname(unlist(amem10B))
tab20A <- unname(unlist(amem20A))
tab20B <- unname(unlist(amem20B))
tab40A <- unname(unlist(amem40A))
tab40B <- unname(unlist(amem40B))

tabela <- data.frame(Porcentagem = c(tab5A[1],tab10A[1],tab20A[1],tab40A[1]), 
                     Rmsd.media = c(tab5A[2],tab10A[2],tab20A[2],tab40A[2]),
                     Vicio.media = c(tab5A[3],tab10A[3],tab20A[3],tab40A[3]),
                     Rmsd.mediana = c(tab5B[2],tab10B[2],tab20B[2],tab40B[2]),
                     Vicio.mediana = c(tab5B[3],tab10B[3],tab20B[3],tab40B[3]))

