# ============================================
# 05 - GERAÇÃO DE GRÁFICOS ESTILO EXCEL
# ============================================

library(data.table)
library(ggplot2)
library(dplyr)
library(reshape2)

run_generate_graphics <- function() {
  input_dir <- "data/output"
  output_dir <- "data/output"
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

  input_file <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)[1]
  if (is.na(input_file) || input_file == "") {
    stop("❌ Nenhum arquivo CSV de trajetória encontrado em: ", input_dir)
  }

  traj <- fread(input_file)
  traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
  traj_long$semana <- as.numeric(as.character(traj_long$semana))

  # Frequência total por estado
  freq_estado <- traj_long %>%
    group_by(estado) %>%
    summarise(qtd = n(), .groups = "drop")

  # Gráfico de barras
  gg_barras <- ggplot(freq_estado, aes(x = reorder(estado, -qtd), y = qtd, fill = estado)) +
    geom_bar(stat = "identity") +
    labs(title = "Frequência por Estado de Internação", x = "Estado", y = "Total de Semanas") +
    theme_minimal()

  ggsave(file.path(output_dir, paste0("grafico_barras_estado_", timestamp, ".png")),
         gg_barras, width = 8, height = 6, dpi = 300)

  # Gráfico de pizza do desfecho final
  traj_final <- traj_long %>%
    group_by(n_neonato) %>%
    arrange(semana) %>%
    summarise(estado_final = last(estado), .groups = "drop") %>%
    count(estado_final)

  gg_pizza <- ggplot(traj_final, aes(x = "", y = n, fill = estado_final)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Destino Final dos Neonatos", x = NULL, y = NULL, fill = "Desfecho") +
    theme_void()

  ggsave(file.path(output_dir, paste0("grafico_pizza_destino_", timestamp, ".png")),
         gg_pizza, width = 6, height = 6, dpi = 300)

  message("✅ Gráficos salvos com sucesso:")
  message("- grafico_barras_estado_", timestamp, ".png")
  message("- grafico_pizza_destino_", timestamp, ".png")
}

# 🔁 Executar automaticamente se chamado diretamente
if (sys.nframe() == 0) {
  run_generate_graphics()
}
