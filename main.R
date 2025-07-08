# main.R
source("scripts/01_generate_trajectory.R")

# Gera arquivos com timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

input_path <- "data/input/TABELAS_coleta_HRT.xlsx"
output_xlsx <- paste0("data/output/trajetoria_", timestamp, ".xlsx")
output_csv  <- paste0("data/output/trajetoria_", timestamp, ".csv")

gerar_trajetoria(
  input_path = input_path,
  output_xlsx = output_xlsx,
  output_csv = output_csv,
  limite_semanas = 30
)
