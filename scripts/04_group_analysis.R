# ============================================
# 04 - ANÁLISE DE TRAJETÓRIAS COM MÉTODO SILHOUETTE
# ============================================

library(TraMineR)
library(data.table)
library(cluster)
library(factoextra)
library(ggplot2)
library(reshape2)
library(dplyr)

run_group_analysis <- function() {
  input_dir <- "data/output"
  output_dir <- "data/output"
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

  input_file <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)[1]
  if (is.na(input_file) || input_file == "") {
    stop("Nenhum arquivo CSV de trajetória encontrado em: ", input_dir)
  }

  # 1. Leitura
  traj <- fread(input_file)
  week_cols <- as.character(1:30)
  estados <- c("ALT", "ENF", "UCO", "UCA", "UTI", "OBI")
  traj[, (week_cols) := lapply(.SD, factor, levels = estados), .SDcols = week_cols]

  # 2. Objeto de sequência
  seq_obj <- seqdef(traj[, ..week_cols], alphabet = estados, states = estados, right = "DEL")

  # 3. Plot: Distribuição geral
  png(file.path(output_dir, paste0("plot_distribuicao_geral_", timestamp, ".png")), width = 1000, height = 600)
  seqfplot(seq_obj, with.legend = "right", main = "Distribuição de Estados por Semana")
  dev.off()

  # 4. Matriz de distância
  dist_mat <- seqdist(seq_obj, method = "OM", indel = 1, sm = "CONSTANT")

  # 5. Escolher número de grupos (Silhouette)
  png(file.path(output_dir, paste0("escolha_numero_grupos_silhouette_", timestamp, ".png")), width = 1000, height = 600)
  fviz_nbclust(as.matrix(dist_mat), FUN = hcut, method = "silhouette", k.max = 6) +
    labs(title = "Número Ótimo de Grupos - Método Silhouette")
  dev.off()

  # 6. Clustering (fixado em k = 5 como exemplo)
  agnes_fit <- agnes(dist_mat, diss = TRUE, method = "ward")
  traj$grupo <- cutree(agnes_fit, k = 5)

  # 7. Dendrograma
  png(file.path(output_dir, paste0("dendrograma_trajetorias_", timestamp, ".png")), width = 1000, height = 600)
  plot(agnes_fit, which.plots = 2, main = "Dendrograma das Trajetórias")
  dev.off()

  # 8. Gráfico por grupo
  png(file.path(output_dir, paste0("trajetorias_por_grupo_", timestamp, ".png")), width = 1000, height = 600)
  seqdplot(seq_obj, group = traj$grupo, border = NA, main = "Trajetórias por Grupo")
  dev.off()

  # 9. Heatmap por grupo
  traj_melt <- melt(traj[, c("grupo", ..week_cols)], id.vars = "grupo", variable.name = "semana", value.name = "estado")
  traj_summary <- traj_melt %>%
    group_by(grupo, estado) %>%
    summarise(prop = n() / nrow(traj), .groups = "drop")

  png(file.path(output_dir, paste0("heatmap_por_grupo_", timestamp, ".png")), width = 1000, height = 600)
  ggplot(traj_summary, aes(x = grupo, y = estado, fill = prop)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = "Distribuição Média de Estados por Grupo", x = "Grupo", y = "Estado") +
    theme_minimal()
  dev.off()

  # 10. Curva acumulada dos estados
  traj_long <- melt(traj[, c("grupo", ..week_cols)], id.vars = "grupo", variable.name = "semana", value.name = "estado")
  traj_long$semana <- as.integer(as.character(traj_long$semana))

  curva_estados <- traj_long %>%
    group_by(semana, estado) %>%
    summarise(freq = n(), .groups = "drop") %>%
    group_by(semana) %>%
    mutate(prop = freq / sum(freq))

  png(file.path(output_dir, paste0("curva_acumulada_estados_", timestamp, ".png")), width = 1000, height = 600)
  ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
    geom_area(alpha = 0.7, color = "white") +
    labs(title = "Proporção Acumulada dos Estados por Semana", x = "Semana", y = "Proporção") +
    theme_minimal()
  dev.off()

  # 11. Salvar base com cluster
  output_csv <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_", timestamp, ".csv"))
  fwrite(traj, output_csv)

  message("✅ Script finalizado com sucesso.")
  message("📁 Cluster salvo em: ", output_csv)
  message("📊 Gráficos salvos em: ", output_dir)
}

# Executar automaticamente se chamado diretamente
if (sys.nframe() == 0) {
  run_group_analysis()
}

# Run
run_group_analysis()