#Gráficos das Séries

set.seed(31)

## Gerando Modelo ARMA (0.4; 0.2)

ARMA11a.100 <- arima.sim(n = 100, list(ar = c(0.4), ma = c(0.2)))

## Imputando dados faltantes

mdataARMA1.100 <- data.frame(Dado = ARMA11a.100, Coluna.controle = ARMA11b.100)

miss05_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")

#Plotando a série bonitinha

#Média
# Carregar a biblioteca ggplot2
library(ggplot2)

# Gerar uma cópia da série com imputação da média nos valores faltantes
serie_imputada <- miss10_ARMA1.100$Dado
serie_imputada[is.na(serie_imputada)] <- mean(miss10_ARMA1.100$Coluna.controle, na.rm = TRUE)

# Criar um dataframe com as séries original e imputada
dados <- data.frame(
  Tempo = 1:length(miss10_ARMA1.100$Dado),
  Observacao = c(miss10_ARMA1.100$Dado, serie_imputada),
  Tipo = rep(c("Original", "Imputada"), each = length(miss10_ARMA1.100$Dado))
)

# Corrigir o vetor da série imputada para manter os valores originais quando não há imputação
dados$Observacao[dados$Tipo == "Imputada" & is.na(miss10_ARMA1.100$Dado)] <- serie_imputada[is.na(miss10_ARMA1.100$Dado)]

pontos_imputacao <- data.frame(
  Tempo = which(is.na(miss10_ARMA1.100$Dado)),
  Observacao = serie_imputada[is.na(miss10_ARMA1.100$Dado)]
)

# Criar o gráfico com ggplot2
ggplot(dados, aes(x = Tempo, y = Observacao, color = Tipo)) +
  geom_line(size = 1) +
  geom_point(data = pontos_imputacao, aes(x = Tempo, y = Observacao), color = "red", size = 2) +
  labs(title = "Imputação com a Média", x = "Tempo", y = "Observações") +
  scale_color_manual(values = c("Original" = "black", "Imputada" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

#Mediana
# Carregar a biblioteca ggplot2
library(ggplot2)

# Gerar uma cópia da série com imputação da mediana nos valores faltantes
serie_imputada <- miss10_ARMA1.100$Dado
serie_imputada[is.na(serie_imputada)] <- median(miss10_ARMA1.100$Coluna.controle, na.rm = TRUE)

# Criar um dataframe com as séries original e imputada
dados <- data.frame(
  Tempo = 1:length(miss10_ARMA1.100$Dado),
  Observacao = c(miss10_ARMA1.100$Dado, serie_imputada),
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
  geom_point(data = pontos_imputacao, aes(x = Tempo, y = Observacao), color = "blue", size = 2) +
  labs(title = "Imputação com a Mediana", x = "Tempo", y = "Observações") +
  scale_color_manual(values = c("Original" = "black", "Imputada" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank())

#Imputação usando LOCF (Down)

# Imputação LOCF (Last Observation Carried Forward) (Down)
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
