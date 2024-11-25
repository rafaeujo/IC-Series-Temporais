## Modelando com n = 100

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

mAR01a.100 <- arima.sim(n = 100, list(ar = c(0.4)))
mAR01b.100 <- arima.sim(n = 100, list(ar = c(0.4)))

## Imputando dados faltantes

mdataAR01.100 <- data.frame(Dado = mAR01a.100, Coluna.controle = mAR01b.100)

mmiss05_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
mmiss10_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")
mmiss20_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.20, "Dado", cols_ctrl = "Coluna.controle")
mmiss40_AR01.100 <- delete_MAR_censoring(mdataAR01.100, 0.40, "Dado", cols_ctrl = "Coluna.controle")

## Função para cálculo de RMSE e viés

metricas_up.100 <- function(alfa, data, coluna, antigo) {
  data_preenchido <- data %>%
    fill(all_of(coluna), .direction = "up")
  data_preenchido[[coluna]][is.na(data_preenchido[[coluna]])] <- mean(data_preenchido[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data_preenchido[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Medida para" = alfa, "RMSE" = b))
}

## Calculando RMSE

mmedidasAR01.up.05.100 <- metricas_up.100(5, mmiss05_AR01.100, "Dado", mdataAR01.100$Dado)
mmedidasAR01.up.10.100 <- metricas_up.100(10, mmiss10_AR01.100, "Dado", mdataAR01.100$Dado)
mmedidasAR01.up.20.100 <- metricas_up.100(20, mmiss20_AR01.100, "Dado", mdataAR01.100$Dado)
mmedidasAR01.up.40.100 <- metricas_up.100(40, mmiss40_AR01.100, "Dado", mdataAR01.100$Dado)

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

nmedidasAR01.up.05.100 <- metricas_up.100(5, nmiss05_AR01.100, "Dado", ndataAR01.100$Dado)
nmedidasAR01.up.10.100 <- metricas_up.100(10, nmiss10_AR01.100, "Dado", ndataAR01.100$Dado)
nmedidasAR01.up.20.100 <- metricas_up.100(20, nmiss20_AR01.100, "Dado", ndataAR01.100$Dado)
nmedidasAR01.up.40.100 <- metricas_up.100(40, nmiss40_AR01.100, "Dado", ndataAR01.100$Dado)

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

mmedidasAR02.up.05.100 <- metricas_up.100(5, mmiss05_AR02.100, "Dado", mdataAR02.100$Dado)
mmedidasAR02.up.10.100 <- metricas_up.100(10, mmiss10_AR02.100, "Dado", mdataAR02.100$Dado)
mmedidasAR02.up.20.100 <- metricas_up.100(20, mmiss20_AR02.100, "Dado", mdataAR02.100$Dado)
mmedidasAR02.up.40.100 <- metricas_up.100(40, mmiss40_AR02.100, "Dado", mdataAR02.100$Dado)

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

mmedidasAR02_2.up..05.100 <- metricas_up.100(5, mmiss05_AR02_2.100, "Dado", mdataAR02_2.100$Dado)
mmedidasAR02_2.up..10.100 <- metricas_up.100(10, mmiss10_AR02_2.100, "Dado", mdataAR02_2.100$Dado)
mmedidasAR02_2.up..20.100 <- metricas_up.100(20, mmiss20_AR02_2.100, "Dado", mdataAR02_2.100$Dado)
mmedidasAR02_2.up..40.100 <- metricas_up.100(40, mmiss40_AR02_2.100, "Dado", mdataAR02_2.100$Dado)

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

medidasARMA1.up.05.100 <- metricas_up.100(5, miss05_ARMA1.100, "Dado", mdataARMA1.100$Dado)
medidasARMA1.up.10.100 <- metricas_up.100(10, miss10_ARMA1.100, "Dado", mdataARMA1.100$Dado)
medidasARMA1.up.20.100 <- metricas_up.100(20, miss20_ARMA1.100, "Dado", mdataARMA1.100$Dado)
medidasARMA1.up.40.100 <- metricas_up.100(40, miss40_ARMA1.100, "Dado", mdataARMA1.100$Dado)

## Tabelando os valores

mAR01.tab.up.5.100 <- unname(unlist(mmedidasAR01.up.05.100))
mAR01.tab.up.10.100 <- unname(unlist(mmedidasAR01.up.10.100))
mAR01.tab.up.20.100 <- unname(unlist(mmedidasAR01.up.20.100))
mAR01.tab.up.40.100 <- unname(unlist(mmedidasAR01.up.40.100))
nAR01.tab.up.5.100 <- unname(unlist(nmedidasAR01.up.05.100))
nAR01.tab.up.10.100 <- unname(unlist(nmedidasAR01.up.10.100))
nAR01.tab.up.20.100 <- unname(unlist(nmedidasAR01.up.20.100))
nAR01.tab.up.40.100 <- unname(unlist(nmedidasAR01.up.40.100))
mAR02.tab.up.5.100 <- unname(unlist(mmedidasAR02.up.05.100))
mAR02.tab.up.10.100 <- unname(unlist(mmedidasAR02.up.10.100))
mAR02.tab.up.20.100 <- unname(unlist(mmedidasAR02.up.20.100))
mAR02.tab.up.40.100 <- unname(unlist(mmedidasAR02.up.40.100))
mAR02_2.tab.up.5.100 <- unname(unlist(mmedidasAR02_2.up..05.100))
mAR02_2.tab.up.10.100 <- unname(unlist(mmedidasAR02_2.up..10.100))
mAR02_2.tab.up.20.100 <- unname(unlist(mmedidasAR02_2.up..20.100))
mAR02_2.tab.up.40.100 <- unname(unlist(mmedidasAR02_2.up..40.100))
mARMA1.tab.up.5.100 <- unname(unlist(medidasARMA1.up.05.100))
mARMA1.tab.up.10.100 <- unname(unlist(medidasARMA1.up.10.100))
mARMA1.tab.up.20.100 <- unname(unlist(medidasARMA1.up.20.100))
mARMA1.tab.up.40.100 <- unname(unlist(medidasARMA1.up.40.100))

mtabelaARGeral.up.100 <- data.frame(Porcentagem = c(mAR02.tab.up.5.100[1], mAR02.tab.up.10.100[1], mAR02.tab.up.20.100[1], mAR02.tab.up.40.100[1]), 
                                      AR.Rmse.04 = c(mAR01.tab.up.5.100[2], mAR01.tab.up.10.100[2], mAR01.tab.up.20.100[2], mAR01.tab.up.40.100[2]),
                                      AR.Rmse.06 = c(nAR01.tab.up.5.100[2], nAR01.tab.up.10.100[2], nAR01.tab.up.20.100[2], nAR01.tab.up.40.100[2]),
                                      AR.Rmse.0204 = c(mAR02.tab.up.5.100[2], mAR02.tab.up.10.100[2], mAR02.tab.up.20.100[2], mAR02.tab.up.40.100[2]),
                                      AR.Rmse.0405 = c(mAR02_2.tab.up.5.100[2], mAR02_2.tab.up.10.100[2], mAR02_2.tab.up.20.100[2], mAR02_2.tab.up.40.100[2]),
                                      ARMA.Rmse.0402 = c(mARMA1.tab.up.5.100[2], mARMA1.tab.up.10.100[2], mARMA1.tab.up.20.100[2], mARMA1.tab.up.40.100[2]))

#Gráfico

# Imputação LOCF (Last Observation Carried Forward)
serie_imputada <- miss10_ARMA1.100$Dado
for (i in 2:length(serie_imputada)) {
  if (is.na(serie_imputada[i])) {
    serie_imputada[i] <- serie_imputada[i + 1]
  }
}
serie_imputada[is.na(serie_imputada)] <- mean(miss10_ARMA1.100$Coluna.controle, na.rm = TRUE)

# Criar um dataframe com as séries original e imputada
dados <- data.frame(
  Tempo = 1:length(miss10_ARMA1.100$Dado),
  Observacao =  c(miss10_ARMA1.100$Dado, serie_imputada),
  Tipo = rep(c("Original", "Imputada"), each = length(miss10_ARMA1.100$Dado))
)

# Corrigir o vetor da série imputada para manter os valores originais quando não há imputação
dados$Observacao[dados$Tipo == "Imputada" & is.na(miss10_ARMA1.100$Dado)] <- serie_imputada[is.na(miss10_ARMA1.100$Dado)]

# Criar um dataframe para os pontos de imputação
pontos_imputacao <- data.frame(
  Tempo = which(is.na(miss10_ARMA1.100$Dado)),
  Observacao = serie_imputada[is.na(miss10_ARMA1.100$Dado)]
)

# Criar o gráfico com ggplot2
ggplot(dados, aes(x = Tempo, y = Observacao, color = Tipo)) +
  geom_line(size = 1) +
  geom_point(data = pontos_imputacao, aes(x = Tempo, y = Observacao), color = "green", size = 2) +
  labs(title = "Imputação com LOCF UP", x = "Tempo", y = "Observações") +
  scale_color_manual(values = c("Original" = "black", "Imputada" = "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())

write.csv(mtabelaARGeral.up.100,file ="cemup.csv", row.names = FALSE)
