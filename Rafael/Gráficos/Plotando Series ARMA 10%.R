#Gráficos das Séries

#bibliotecas
library(ggplot2)
library(missMethods)
library(Metrics)
library(tidyr)

set.seed(2003)

## Gerando Modelo ARMA (0.4; 0.2)

ARMA11a.100 <- arima.sim(n = 100, list(ar = c(0.4), ma = c(0.2)))

## Imputando dados faltantes

mdataARMA1.100 <- data.frame(Dado = ARMA11a.100, Coluna.controle = ARMA11a.100)

miss05_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.05, "Dado", cols_ctrl = "Coluna.controle")
miss10_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")


write.csv(miss10_ARMA1.100, "Gráfico.deep.csv")

# ======================================
# Bibliotecas
# ======================================
library(ggplot2)
library(dplyr)
library(imputeTS)
library(Metrics)
library(missMethods)

# ======================================
# Função padronizada (formato aprovado no caso da média)
# ======================================
plot_imputacao <- function(original, imputada, controle, titulo, cor_imputada, original_only = FALSE) {
  
  if (original_only) {
    # Apenas série original com legenda "Original"
    dados_original <- data.frame(
      Tempo = 1:length(original),
      Observacao = original,
      Tipo = "Original"
    )
    
    return(
      ggplot(dados_original, aes(x = Tempo, y = Observacao, color = Tipo)) +
        geom_line(size = 1) +
        scale_color_manual(values = c("Original" = "black")) +
        labs(
          title = titulo,
          subtitle = "Sem imputação",
          x = "Tempo", y = "Observações"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.title = element_blank())
    )
  }
  
  # Calcular RMSE
  rmse_val <- rmse(controle, imputada)
  
  # Criar dataframe para plotagem
  dados <- data.frame(
    Tempo = 1:length(original),
    Observacao = c(original, imputada),
    Tipo = rep(c("Original", "Imputada"), each = length(original))
  )
  
  # Garantir que valores originais fiquem onde não há NA
  dados$Observacao[dados$Tipo == "Imputada" & !is.na(original)] <- original[!is.na(original)]
  
  # Pontos imputados
  pontos_imputacao <- data.frame(
    Tempo = which(is.na(original)),
    Observacao = imputada[is.na(original)]
  )
  
  # Pontos originais verdadeiros
  pontos_originais <- data.frame(
    Tempo = which(is.na(original)),
    Observacao = controle[is.na(original)]
  )
  
  # Linhas tracejadas verticais
  linhas_diferenca <- data.frame(
    Tempo = pontos_imputacao$Tempo,
    y1 = pontos_imputacao$Observacao,
    y2 = pontos_originais$Observacao
  )
  
  ggplot(dados, aes(x = Tempo, y = Observacao, color = Tipo)) +
    geom_line(size = 1) +
    geom_point(data = pontos_imputacao, aes(x = Tempo, y = Observacao),
               color = cor_imputada, size = 2) +
    geom_point(data = pontos_originais, aes(x = Tempo, y = Observacao),
               shape = 1, color = "black", size = 2, stroke = 1) +
    geom_segment(data = linhas_diferenca,
                 aes(x = Tempo, xend = Tempo, y = y1, yend = y2),
                 color = "black", linetype = "dashed", size = 0.6,
                 inherit.aes = FALSE) +
    labs(
      title = titulo,
      subtitle = paste("RMSE =", round(rmse_val, 4)),
      x = "Tempo", y = "Observações"
    ) +
    scale_color_manual(values = c("Original" = "black", "Imputada" = cor_imputada)) +
    theme_minimal(base_size = 12) +
    theme(legend.title = element_blank())
}

# ======================================
# Gerar dados de exemplo
# ======================================
set.seed(2003)
ARMA11a.100 <- arima.sim(n = 100, list(ar = c(0.4), ma = c(0.2)))
mdataARMA1.100 <- data.frame(Dado = ARMA11a.100, Coluna.controle = ARMA11a.100)
miss10_ARMA1.100 <- delete_MAR_censoring(mdataARMA1.100, 0.10, "Dado", cols_ctrl = "Coluna.controle")

# ======================================
# Aplicar para cada caso
# ======================================

# 1. Série original
plot_original <- plot_imputacao(
  original = ARMA11a.100,
  imputada = NULL,
  controle = NULL,
  titulo = "Série Original Simulada",
  cor_imputada = "black",
  original_only = TRUE
)

# 2. Média
serie_media <- miss10_ARMA1.100$Dado
serie_media[is.na(serie_media)] <- mean(miss10_ARMA1.100$Coluna.controle, na.rm = TRUE)
plot_media <- plot_imputacao(
  original = miss10_ARMA1.100$Dado,
  imputada = serie_media,
  controle = miss10_ARMA1.100$Coluna.controle,
  titulo = "Imputação com a Média",
  cor_imputada = "blue"
)

# 3. Mediana
serie_mediana <- miss10_ARMA1.100$Dado
serie_mediana[is.na(serie_mediana)] <- median(miss10_ARMA1.100$Coluna.controle, na.rm = TRUE)
plot_mediana <- plot_imputacao(
  original = miss10_ARMA1.100$Dado,
  imputada = serie_mediana,
  controle = miss10_ARMA1.100$Coluna.controle,
  titulo = "Imputação com a Mediana",
  cor_imputada = "purple"
)

# 4. LOCF
serie_locf <- na_locf(miss10_ARMA1.100$Dado, option = "locf", na_remaining = "rev")
plot_locf <- plot_imputacao(
  original = miss10_ARMA1.100$Dado,
  imputada = serie_locf,
  controle = miss10_ARMA1.100$Coluna.controle,
  titulo = "Imputação com LOCF",
  cor_imputada = "red"
)

# 5. NOCB
serie_nocb <- na_locf(miss10_ARMA1.100$Dado, option = "nocb", na_remaining = "rev")
plot_nocb <- plot_imputacao(
  original = miss10_ARMA1.100$Dado,
  imputada = serie_nocb,
  controle = miss10_ARMA1.100$Coluna.controle,
  titulo = "Imputação com NOCB",
  cor_imputada = "darkorange"
)

# 6. Spline
serie_spline <- na_interpolation(miss10_ARMA1.100$Dado, option = "spline")
plot_spline <- plot_imputacao(
  original = miss10_ARMA1.100$Dado,
  imputada = serie_spline,
  controle = miss10_ARMA1.100$Coluna.controle,
  titulo = "Imputação com Interpolação Spline",
  cor_imputada = "darkorange"
)

# ======================================
# Exibir todos
# ======================================
plot_original
plot_media
plot_mediana
plot_locf
plot_nocb
plot_spline


library(ggplot2)
library(Metrics)
library(readr)

plot_imputacao <- function(original, imputada, controle, titulo, cor_imputada, original_only = FALSE) {
  
  if (original_only) {
    dados_original <- data.frame(
      Tempo = 1:length(original),
      Observacao = original,
      Tipo = "Original"
    )
    
    return(
      ggplot(dados_original, aes(x = Tempo, y = Observacao, color = Tipo)) +
        geom_line(size = 1) +
        scale_color_manual(values = c("Original" = "black")) +
        labs(
          title = titulo,
          subtitle = "Sem imputação",
          x = "Tempo", y = "Observações"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.title = element_blank())
    )
  }
  
  # Criar vetor completo substituindo NAs pelo imputado
  imputacao_completa <- original
  imputacao_completa[is.na(imputacao_completa)] <- imputada[is.na(original)]
  
  # Calcular RMSE com controle
  rmse_val <- rmse(controle, imputacao_completa)
  
  # Dados para plotagem
  dados <- data.frame(
    Tempo = 1:length(original),
    Observacao = c(original, imputacao_completa),
    Tipo = rep(c("Original", "Imputada"), each = length(original))
  )
  
  # Pontos imputados
  pontos_imputacao <- data.frame(
    Tempo = which(is.na(original)),
    Observacao = imputada[is.na(original)]
  )
  
  # Pontos originais verdadeiros
  pontos_originais <- data.frame(
    Tempo = which(is.na(original)),
    Observacao = controle[is.na(original)]
  )
  
  # Linhas tracejadas verticais
  linhas_diferenca <- data.frame(
    Tempo = pontos_imputacao$Tempo,
    y1 = pontos_imputacao$Observacao,
    y2 = pontos_originais$Observacao
  )
  
  ggplot(dados, aes(x = Tempo, y = Observacao, color = Tipo)) +
    geom_line(size = 1) +
    geom_point(data = pontos_imputacao, aes(x = Tempo, y = Observacao),
               color = cor_imputada, size = 2) +
    geom_point(data = pontos_originais, aes(x = Tempo, y = Observacao),
               shape = 1, color = "black", size = 2, stroke = 1) +
    geom_segment(data = linhas_diferenca,
                 aes(x = Tempo, xend = Tempo, y = y1, yend = y2),
                 color = "black", linetype = "dashed", size = 0.6,
                 inherit.aes = FALSE) +
    labs(
      title = titulo,
      subtitle = paste("RMSE =", round(rmse_val, 4)),
      x = "Tempo", y = "Observações"
    ) +
    scale_color_manual(values = c("Original" = "black", "Imputada" = cor_imputada)) +
    theme_minimal(base_size = 12) +
    theme(legend.title = element_blank())
}

# ----- Ler dados -----
df_deep <- read_csv("imputdeepgraf.csv", col_types = cols(
  Dado_Original = col_double(),
  Imputados = col_double()
))

# ----- Criar controle -----
set.seed(2003)
ARMA11a.100 <- arima.sim(n = 100, list(ar = c(0.4), ma = c(0.2)))
mdataARMA1.100 <- data.frame(Dado = ARMA11a.100, Coluna.controle = ARMA11a.100)

# ----- Plot Deep Learning -----
plot_deep <- plot_imputacao(
  original = df_deep$Dado_Original,
  imputada = df_deep$Imputados,
  controle = mdataARMA1.100$Coluna.controle,
  titulo = "Imputação com Deep Learning",
  cor_imputada = "darkcyan"
)

print(plot_deep)

