# =============================================
# 03 - ANÁLISE DESCRITIVA E GRÁFICOS DE TRAJETÓRIA
# =============================================

# Pacotes necessários
library(data.table)
library(dplyr)
library(ggplot2)

run_descriptive_analysis <- function() {
  # Diretórios
  input_dir <- "data/output"
  output_dir <- "data/output"
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

  # 1. Localiza o CSV de trajetória
  input_file <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)[1]

  if (is.na(input_file) || input_file == "") {
    stop("Nenhum arquivo CSV de trajetória encontrado em: ", input_dir)
  }

  # 2. Carregar a matriz de trajetória
  traj <- fread(input_file)

  # 3. Transformar para formato longo
  traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
  traj_long$semana <- as.numeric(as.character(traj_long$semana))

  # 4. Resumo por semana e estado
  resumo <- traj_long %>%
    group_by(semana, estado) %>%
    summarise(n = n(), .groups = "drop")

  # ============================================
  # 📈 GRÁFICO DE LINHAS – Evolução semanal
  # ============================================
  grafico_linhas <- ggplot(resumo, aes(x = semana, y = n, color = estado)) +
    geom_line(size = 1.2) +
    labs(
      title = "Evolução Semanal por Estado",
      x = "Semana",
      y = "Número de Pacientes",
      color = "Estado"
    ) +
    theme_minimal()

  ggsave(file.path(output_dir, paste0("grafico_linhas_trajetoria_", timestamp, ".png")),
         grafico_linhas, width = 10, height = 6, dpi = 300)

  # ============================================
  # 📊 GRÁFICO DE BARRAS EMPILHADAS
  # ============================================
  grafico_barras <- ggplot(resumo, aes(x = semana, y = n, fill = estado)) +
    geom_bar(stat = "identity") +
    labs(
      title = "Distribuição por Estado em Cada Semana",
      x = "Semana",
      y = "Número de Pacientes",
      fill = "Estado"
    ) +
    theme_minimal()

  ggsave(file.path(output_dir, paste0("grafico_barras_empilhadas_trajetoria_", timestamp, ".png")),
         grafico_barras, width = 10, height = 6, dpi = 300)

  # ============================================
  # 🔥 HEATMAP – Estado vs Semana
  # ============================================
  grafico_heatmap <- ggplot(resumo, aes(x = semana, y = estado, fill = n)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "darkblue") +
    labs(
      title = "Heatmap: Estado x Semana",
      x = "Semana",
      y = "Estado",
      fill = "Nº Pacientes"
    ) +
    theme_minimal()

  ggsave(file.path(output_dir, paste0("heatmap_estado_semana_trajetoria_", timestamp, ".png")),
         grafico_heatmap, width = 10, height = 6, dpi = 300)

  # Final
  message("✅ Gráficos gerados com sucesso:")
  message("- grafico_linhas_trajetoria_", timestamp, ".png")
  message("- grafico_barras_empilhadas_trajetoria_", timestamp, ".png")
  message("- heatmap_estado_semana_trajetoria_", timestamp, ".png")
}

# Executa automaticamente se chamado diretamente
if (sys.nframe() == 0) {
  run_descriptive_analysis()
}

run_descriptive_analysis()