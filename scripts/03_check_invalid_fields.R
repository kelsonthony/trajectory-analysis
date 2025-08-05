# ===============================================
# 03_check_invalid_fields.R
# Diagnóstico de estados inválidos, NAs ou inconsistências
# ===============================================

library(data.table)
library(stringr)

# Caminho do arquivo CSV mais recente
input_file <- list.files("data/output", pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)[1]
if (is.na(input_file) || input_file == "") stop("❌ Nenhum arquivo de trajetória encontrado.")

traj <- fread(input_file)

# Estados válidos esperados
estados_validos <- c("ALT", "ENF", "UCO", "UCA", "UTI", "OBI")
week_cols <- as.character(1:30)

# Padronização: converte tudo para maiúsculo e remove espaços
traj_check <- traj[, ..week_cols] |> lapply(function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x == ""] <- NA
  return(x)
}) |> as.data.table()

# Verifica linhas com NA ou valores fora do conjunto válido
linhas_invalidas <- traj[
  apply(traj_check, 1, function(x) any(is.na(x) | !x %in% estados_validos))
]

# Linhas e colunas com erro específico
erros_detalhados <- which(
  sapply(traj_check, function(col) is.na(col) | !col %in% estados_validos),
  arr.ind = TRUE
)

cat("🔎 Número total de linhas com erro:", nrow(linhas_invalidas), "\n")

if (nrow(linhas_invalidas) > 0) {
  # Salva CSV com as linhas problemáticas
  erro_csv <- file.path("data/output", paste0("diagnostico_linhas_invalidas_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))
  fwrite(linhas_invalidas, erro_csv)
  
  cat("📁 Linhas inválidas salvas em:", erro_csv, "\n")
  
  # Mostra algumas posições exatas do erro
  print("📌 Algumas posições com erro (linha, coluna):")
  print(head(erros_detalhados, 10))
} else {
  cat("✅ Nenhuma inconsistência detectada.\n")
}
