# main.R
source("scripts/00_mount_trajectory.R")
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


# Etapa 2 - Análise com TraMineR
source("scripts/02_analysis_tramine.R")

# Etapa 3 - Análise descritiva
source("scripts/03_analysis_descriptive.R")
run_descriptive_analysis()

# Etapa 4 - Análise de grupos
source("scripts/04_group_analysis.R")
run_group_analysis()

# Etapa 5 - Gerar Graficos
source("scripts/05_generate_graphics.R")
run_generate_graphics()

# Etapa 6 - Gerar perfil de tempo por cluster
source("scripts/06_cluster_time_profile.R")
run_cluster_time_profile()

