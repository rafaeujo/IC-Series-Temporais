## Modelando com n = 100

#bibliotecas
library(ggplot2)
library(missMethods)
library(Metrics)
library(tidyr)

## Função para cálculo de RMSE e viés

metricas_down <- function(alfa, data, coluna, antigo) {
  data_preenchido <- data %>%
    fill(all_of(coluna))
  data_preenchido[[coluna]][is.na(data_preenchido[[coluna]])] <- mean(data_preenchido[[coluna]], na.rm = TRUE)
  b <- rmse(antigo, data_preenchido[[coluna]])
  RMSE <- b
  
  # Retorna as métricas calculadas
  return(list("Medida para" = alfa, "RMSE" = b))
}

## Gerando Modelo AR1 (phi = 0.4)
ar1.04.down.100 <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR1.04.100.csv")

## Calculando RMSE

mmedidasAR01.05.down.100 <- metricas_down(5, ar1.04.down.100, "Dado5", ar1.04.down.100$Coluna.controle)
mmedidasAR01.10.down.100 <- metricas_down(10, ar1.04.down.100, "Dado10", ar1.04.down.100$Coluna.controle)
mmedidasAR01.20.down.100 <- metricas_down(20, ar1.04.down.100, "Dado20", ar1.04.down.100$Coluna.controle)
mmedidasAR01.40.down.100 <- metricas_down(40, ar1.04.down.100, "Dado40", ar1.04.down.100$Coluna.controle)

## Tabelando os valores

mAR01.tab5.down.100 <- unname(unlist(mmedidasAR01.05.down.100))
mAR01.tab10.down.100 <- unname(unlist(mmedidasAR01.10.down.100))
mAR01.tab20.down.100 <- unname(unlist(mmedidasAR01.20.down.100))
mAR01.tab40.down.100 <- unname(unlist(mmedidasAR01.40.down.100))

## Gerando Modelo AR1 (phi = 0.6)

ar1.06.down.100 <- read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR1.06.100.csv")

## Calculando RMSE

nmedidasAR01.05.down.100 <- metricas_down(5, ar1.06.down.100, "Dado5", ar1.06.down.100$Coluna.controle)
nmedidasAR01.10.down.100 <- metricas_down(10, ar1.06.down.100, "Dado10", ar1.06.down.100$Coluna.controle)
nmedidasAR01.20.down.100 <- metricas_down(20, ar1.06.down.100, "Dado20", ar1.06.down.100$Coluna.controle)
nmedidasAR01.40.down.100 <- metricas_down(40, ar1.06.down.100, "Dado40", ar1.06.down.100$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.2)

ar2.0402.down.100 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR2.0402.100.csv")

## Calculando RMSE

mmedidasAR02.05.down.100 <- metricas_down(5, ar2.0402.down.100, "Dado5", ar2.0402.down.100$Coluna.controle)
mmedidasAR02.10.down.100 <- metricas_down(10, ar2.0402.down.100, "Dado10", ar2.0402.down.100$Coluna.controle)
mmedidasAR02.20.down.100 <- metricas_down(20, ar2.0402.down.100, "Dado20", ar2.0402.down.100$Coluna.controle)
mmedidasAR02.40.down.100 <- metricas_down(40, ar2.0402.down.100, "Dado40", ar2.0402.down.100$Coluna.controle)

## Gerando Modelo AR2 (0.4; 0.5)

ar2.0405.down.100 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/AR2.0405.100.csv")

## Calculando RMSE

mmedidasAR02_2.05.down.100 <- metricas_down(5, ar2.0405.down.100, "Dado5", ar2.0405.down.100$Coluna.controle)
mmedidasAR02_2.10.down.100 <- metricas_down(10, ar2.0405.down.100, "Dado10", ar2.0405.down.100$Coluna.controle)
mmedidasAR02_2.20.down.100 <- metricas_down(20, ar2.0405.down.100, "Dado20", ar2.0405.down.100$Coluna.controle)
mmedidasAR02_2.40.down.100 <- metricas_down(40, ar2.0405.down.100, "Dado40", ar2.0405.down.100$Coluna.controle)

## Gerando Modelo ARMA (0.4; 0.2)

arma.down.100 <-  read.csv("~/IC-Series-Temporais/Rafael/Modelando as séries n=100/ARMA.100.csv")

## Calculando RMSE

medidasARMA1.05.down.100 <- metricas_down(5,arma.down.100, "Dado5", arma.down.100$Coluna.controle)
medidasARMA1.10.down.100 <- metricas_down(10, arma.down.100, "Dado10", arma.down.100$Coluna.controle)
medidasARMA1.20.down.100 <- metricas_down(20, arma.down.100, "Dado20", arma.down.100$Coluna.controle)
medidasARMA1.40.down.100 <- metricas_down(40, arma.down.100, "Dado40", arma.down.100$Coluna.controle)

## Tabelando os valores

mAR01.tab5.down.100 <- unname(unlist(mmedidasAR01.05.down.100))
mAR01.tab10.down.100 <- unname(unlist(mmedidasAR01.10.down.100))
mAR01.tab20.down.100  <- unname(unlist(mmedidasAR01.20.down.100))
mAR01.tab40.down.100  <- unname(unlist(mmedidasAR01.40.down.100))
nAR01.tab5.down.100 <- unname(unlist(nmedidasAR01.05.down.100))
nAR01.tab10.down.100 <- unname(unlist(nmedidasAR01.10.down.100))
nAR01.tab20.down.100  <- unname(unlist(nmedidasAR01.20.down.100))
nAR01.tab40.down.100  <- unname(unlist(nmedidasAR01.40.down.100))
mAR02.tab5.down.100 <- unname(unlist(mmedidasAR02.05.down.100))
mAR02.tab10.down.100 <- unname(unlist(mmedidasAR02.10.down.100))
mAR02.tab20.down.100 <- unname(unlist(mmedidasAR02.20.down.100))
mAR02.tab40.down.100 <- unname(unlist(mmedidasAR02.40.down.100))
mAR02_2.tab5.down.100 <- unname(unlist(mmedidasAR02_2.05.down.100))
mAR02_2.tab10.down.100 <- unname(unlist(mmedidasAR02_2.10.down.100))
mAR02_2.tab20.down.100 <- unname(unlist(mmedidasAR02_2.20.down.100))
mAR02_2.tab40.down.100 <- unname(unlist(mmedidasAR02_2.40.down.100))
mARMA1.tab5.down.100 <- unname(unlist(medidasARMA1.05.down.100))
mARMA1.tab10.down.100 <- unname(unlist(medidasARMA1.10.down.100))
mARMA1.tab20.down.100 <- unname(unlist(medidasARMA1.20.down.100))
mARMA1.tab40.down.100 <- unname(unlist(medidasARMA1.40.down.100))

mtabelaARGeral.down.100 <- data.frame(Porcentagem = c(mAR02.tab5.down.100[1], mAR02.tab10.down.100[1], mAR02.tab20.down.100[1], mAR02.tab40.down.100[1]), 
                                    AR.Rmse.04 = c(mAR01.tab5.down.100[2], mAR01.tab10.down.100[2], mAR01.tab20.down.100[2], mAR01.tab40.down.100[2]),
                                    AR.Rmse.06 = c(nAR01.tab5.down.100[2], nAR01.tab10.down.100[2], nAR01.tab20.down.100[2], nAR01.tab40.down.100[2]),
                                    AR.Rmse.0204 = c(mAR02.tab5.down.100[2], mAR02.tab10.down.100[2], mAR02.tab20.down.100[2], mAR02.tab40.down.100[2]),
                                    AR.Rmse.0405 = c(mAR02_2.tab5.down.100[2], mAR02_2.tab10.down.100[2], mAR02_2.tab20.down.100[2], mAR02_2.tab40.down.100[2]),
                                    ARMA.Rmse.0402 = c(mARMA1.tab5.down.100[2], mARMA1.tab10.down.100[2], mARMA1.tab20.down.100[2], mARMA1.tab40.down.100[2]))


write.csv(mtabelaARGeral.down.100,file ="cemdown.csv", row.names = FALSE)

#Gráfico

# Imputação LOCF (Last Observation Carried Forward)
serie_imputada <- miss10_ARMA1.100$Dado
for (i in 2:length(serie_imputada)) {
  if (is.na(serie_imputada[i])) {
    serie_imputada[i] <- serie_imputada[i - 1]
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
  geom_point(data = pontos_imputacao, aes(x = Tempo, y = Observacao), color = "purple", size = 2) +
  labs(title = "Imputação com LOCF", x = "Tempo", y = "Observações") +
  scale_color_manual(values = c("Original" = "black", "Imputada" = "purple")) +
  theme_minimal() +
  theme(legend.title = element_blank())

write.csv(mtabelaARGeral.down.100,file ="cemdown.csv", row.names = FALSE)
