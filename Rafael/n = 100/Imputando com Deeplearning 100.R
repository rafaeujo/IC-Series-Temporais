library(Metrics)

# Caminho base dos arquivos imputados
base_path <- "Imputados deep 100/"

# Função para calcular RMSE
metricas_deep <- function(alfa, data, coluna, antigo) {
  b <- rmse(antigo, data[[coluna]])
  return(list("Media para" = alfa, "RMSE" = b))
}

# -------------------------
# AR1 (phi = 0.4)
deep_ar1_04 <- read.csv(paste0(base_path, "deep.ar1(04).100.csv"))
rmse_ar1_04_5  <- metricas_deep(5,  deep_ar1_04, "Dado5",  deep_ar1_04$Coluna.controle)
rmse_ar1_04_10 <- metricas_deep(10, deep_ar1_04, "Dado10", deep_ar1_04$Coluna.controle)
rmse_ar1_04_20 <- metricas_deep(20, deep_ar1_04, "Dado20", deep_ar1_04$Coluna.controle)
rmse_ar1_04_40 <- metricas_deep(40, deep_ar1_04, "Dado40", deep_ar1_04$Coluna.controle)

# -------------------------
# AR1 (phi = 0.6)
deep_ar1_06 <- read.csv(paste0(base_path, "deep.ar1(06).100.csv"))
rmse_ar1_06_5  <- metricas_deep(5,  deep_ar1_06, "Dado5",  deep_ar1_06$Coluna.controle)
rmse_ar1_06_10 <- metricas_deep(10, deep_ar1_06, "Dado10", deep_ar1_06$Coluna.controle)
rmse_ar1_06_20 <- metricas_deep(20, deep_ar1_06, "Dado20", deep_ar1_06$Coluna.controle)
rmse_ar1_06_40 <- metricas_deep(40, deep_ar1_06, "Dado40", deep_ar1_06$Coluna.controle)

# -------------------------
# AR2 (0.4, 0.2)
deep_ar2_0402 <- read.csv(paste0(base_path, "deep.ar2(0402).100.csv"))
rmse_ar2_0402_5  <- metricas_deep(5,  deep_ar2_0402, "Dado5",  deep_ar2_0402$Coluna.controle)
rmse_ar2_0402_10 <- metricas_deep(10, deep_ar2_0402, "Dado10", deep_ar2_0402$Coluna.controle)
rmse_ar2_0402_20 <- metricas_deep(20, deep_ar2_0402, "Dado20", deep_ar2_0402$Coluna.controle)
rmse_ar2_0402_40 <- metricas_deep(40, deep_ar2_0402, "Dado40", deep_ar2_0402$Coluna.controle)

# -------------------------
# AR2 (0.4, 0.5)
deep_ar2_0405 <- read.csv(paste0(base_path, "deep.ar2(0405).100.csv"))
rmse_ar2_0405_5  <- metricas_deep(5,  deep_ar2_0405, "Dado5",  deep_ar2_0405$Coluna.controle)
rmse_ar2_0405_10 <- metricas_deep(10, deep_ar2_0405, "Dado10", deep_ar2_0405$Coluna.controle)
rmse_ar2_0405_20 <- metricas_deep(20, deep_ar2_0405, "Dado20", deep_ar2_0405$Coluna.controle)
rmse_ar2_0405_40 <- metricas_deep(40, deep_ar2_0405, "Dado40", deep_ar2_0405$Coluna.controle)

# -------------------------
# ARMA (0.4, 0.2)
deep_arma <- read.csv(paste0(base_path, "deep.arma.100.csv"))
rmse_arma_5  <- metricas_deep(5,  deep_arma, "Dado5",  deep_arma$Coluna.controle)
rmse_arma_10 <- metricas_deep(10, deep_arma, "Dado10", deep_arma$Coluna.controle)
rmse_arma_20 <- metricas_deep(20, deep_arma, "Dado20", deep_arma$Coluna.controle)
rmse_arma_40 <- metricas_deep(40, deep_arma, "Dado40", deep_arma$Coluna.controle)

# -------------------------
# Tabelando resultados
cemdeep <- data.frame(
  Porcentagem = c(5, 10, 20, 40),
  AR1_04 = c(rmse_ar1_04_5[[2]], rmse_ar1_04_10[[2]], rmse_ar1_04_20[[2]], rmse_ar1_04_40[[2]]),
  AR1_06 = c(rmse_ar1_06_5[[2]], rmse_ar1_06_10[[2]], rmse_ar1_06_20[[2]], rmse_ar1_06_40[[2]]),
  AR2_0402 = c(rmse_ar2_0402_5[[2]], rmse_ar2_0402_10[[2]], rmse_ar2_0402_20[[2]], rmse_ar2_0402_40[[2]]),
  AR2_0405 = c(rmse_ar2_0405_5[[2]], rmse_ar2_0405_10[[2]], rmse_ar2_0405_20[[2]], rmse_ar2_0405_40[[2]]),
  ARMA_0402 = c(rmse_arma_5[[2]], rmse_arma_10[[2]], rmse_arma_20[[2]], rmse_arma_40[[2]])
)

# Salvando o resultado
#write.csv(cemdeep, "cemdeep.csv", row.names = FALSE)
