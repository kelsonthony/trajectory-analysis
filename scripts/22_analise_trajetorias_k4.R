# ================================================================
# 05 - ANÁLISE COMPLETA DE TRAJETÓRIAS — VERSÃO FINAL (k = 4)
# ================================================================

packages_needed <- c(
  "TraMineR","data.table","cluster","factoextra","ggplot2",
  "reshape2","dplyr","readxl","dendextend","viridisLite","writexl"
)
to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages_needed, library, character.only = TRUE))

input_dir  <- "data/output"
output_dir <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
timestamp  <- format(Sys.time(), "%Y%m%d_%H%M")

input_files <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("❌ Nenhum arquivo CSV de trajetória encontrado.")
input_file <- tail(sort(input_files), 1)

estados_validos <- c("ALT","ENF","UCO","UCA","UTI","OBI")
week_cols <- as.character(1:30)
k <- 4

traj <- data.table::fread(input_file, sep = ";", encoding = "UTF-8")

traj_clean <- traj[, ..week_cols] |>
  lapply(function(col){
    x <- toupper(trimws(as.character(col)))
    x[!(x %in% estados_validos)] <- NA
    factor(x, levels = estados_validos)
  }) |>
  as.data.table()

traj_clean[, id := .I]
traj_clean <- traj_clean[complete.cases(traj_clean)]
cat("✅ Total de linhas válidas para análise:", nrow(traj_clean), "\n")

seq_obj <- TraMineR::seqdef(traj_clean[, ..week_cols],
                            alphabet = estados_validos, states = estados_validos)

png(file.path(output_dir, paste0("plot_distribuicao_geral_", timestamp, ".png")),
    width = 1000, height = 600)
TraMineR::seqfplot(seq_obj, with.legend = "right",
                   main = paste0("Distribuição de Estados por Semana (n=", nrow(seq_obj), ")"))
dev.off()

sm_const <- TraMineR::seqsubm(seq_obj, method = "CONSTANT", cval = 2)
dist_mat <- TraMineR::seqdist(seq_obj, method = "OM", indel = 1, sm = sm_const)

png(file.path(output_dir, paste0("diagnostico_silhouette_", timestamp, ".png")),
    width = 1000, height = 600)
factoextra::fviz_nbclust(as.matrix(dist_mat), FUN = hcut,
                          method = "silhouette", k.max = 6) +
  labs(title = "Diagnóstico: Silhouette vs k (Ward + OM)")
dev.off()

agnes_fit <- cluster::agnes(dist_mat, diss = TRUE, method = "ward")
hc   <- as.hclust(agnes_fit)
traj_clean$cluster <- paste0("type ", cutree(hc, k = k))

dend <- stats::as.dendrogram(hc)
dend <- dendextend::color_branches(dend, k = k, col = viridisLite::viridis(k))
png(file.path(output_dir, paste0("dendrograma_trajetorias_k4_", timestamp, ".png")),
    width = 1000, height = 600)
plot(dend, main = "Dendrograma das Trajetórias (k = 4)")
dendextend::rect.dendrogram(dend, k = k, border = "grey30", lty = 1)
dev.off()

png(file.path(output_dir, paste0("trajetorias_por_cluster_k4_", timestamp, ".png")),
    width = 1200, height = 750)
TraMineR::seqdplot(seq_obj, group = traj_clean$cluster, border = NA,
                   main = "Trajetórias por Cluster (k = 4)")
dev.off()

traj_melt <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_summary <- traj_melt |>
  dplyr::group_by(cluster, estado) |>
  dplyr::summarise(prop = dplyr::n() / nrow(traj_clean), .groups = "drop")

png(file.path(output_dir, paste0("heatmap_por_cluster_k4_", timestamp, ".png")),
    width = 1000, height = 600)
ggplot(traj_summary, aes(x = cluster, y = estado, fill = prop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Distribuição Média de Estados por Cluster (k = 4)",
       x = "Cluster", y = "Estado") +
  theme_minimal()
dev.off()

traj_long <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.integer(as.character(traj_long$semana))
curva_estados <- traj_long |>
  dplyr::group_by(semana, estado) |>
  dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(semana) |>
  dplyr::mutate(prop = freq / sum(freq))

png(file.path(output_dir, paste0("curva_acumulada_estados_k4_", timestamp, ".png")),
    width = 1000, height = 600)
ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
  geom_area(alpha = 0.7, color = "white") +
  labs(title = "Proporção Acumulada dos Estados por Semana (k = 4)",
       x = "Semana", y = "Proporção") +
  theme_minimal()
dev.off()

# ===================== Índices: entropia / turbulência / transições =====================
entropy_seq     <- TraMineR::seqient(seq_obj)        # Entropia de Shannon
turbulence_seq  <- TraMineR::seqST(seq_obj)          # <- CORRIGIDO: sem 'measure='
transitions_seq <- TraMineR::seqtransn(seq_obj)      # Nº de mudanças de estado

indices_seq <- data.frame(
  id          = traj_clean$id,
  cluster     = traj_clean$cluster,
  entropy     = as.numeric(entropy_seq),
  turbulence  = as.numeric(turbulence_seq),
  transitions = as.integer(transitions_seq)
)

indices_por_cluster <- indices_seq |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(
    n               = dplyr::n(),
    entropy_mean    = mean(entropy, na.rm = TRUE),
    entropy_sd      = sd(entropy, na.rm = TRUE),
    entropy_median  = median(entropy, na.rm = TRUE),
    turbulence_mean = mean(turbulence, na.rm = TRUE),
    turbulence_sd   = sd(turbulence, na.rm = TRUE),
    turbulence_median = median(turbulence, na.rm = TRUE),
    trans_mean      = mean(transitions, na.rm = TRUE),
    trans_sd        = sd(transitions, na.rm = TRUE),
    trans_median    = median(transitions, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(cluster)

png(file.path(output_dir, paste0("boxplot_entropia_por_cluster_k4_", timestamp, ".png")),
    width = 900, height = 600)
ggplot(indices_seq, aes(x = cluster, y = entropy, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Entropia por Cluster (k = 4)", x = "Cluster", y = "Entropia (Shannon)") +
  theme_minimal() + theme(legend.position = "none")
dev.off()

png(file.path(output_dir, paste0("boxplot_turbulencia_por_cluster_k4_", timestamp, ".png")),
    width = 900, height = 600)
ggplot(indices_seq, aes(x = cluster, y = turbulence, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Turbulência por Cluster (k = 4)", x = "Cluster", y = "Turbulência") +
  theme_minimal() + theme(legend.position = "none")
dev.off()

png(file.path(output_dir, paste0("boxplot_transicoes_por_cluster_k4_", timestamp, ".png")),
    width = 900, height = 600)
ggplot(indices_seq, aes(x = cluster, y = transitions, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Número de Transições por Cluster (k = 4)", x = "Cluster", y = "Transições") +
  theme_minimal() + theme(legend.position = "none")
dev.off()

output_csv  <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_k4_", timestamp, ".csv"))
output_xlsx <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_k4_", timestamp, ".xlsx"))

data.table::fwrite(traj_clean, output_csv)

writexl::write_xlsx(
  list(
    "traj_clusters"       = traj_clean,
    "heatmap_summary"     = traj_summary,
    "curva_estados"       = curva_estados,
    "indices_por_seq"     = indices_seq,
    "indices_por_cluster" = indices_por_cluster
  ),
  path = output_xlsx
)

data.table::fwrite(indices_seq,           file.path(output_dir, paste0("indices_por_seq_k4_", timestamp, ".csv")))
data.table::fwrite(indices_por_cluster,   file.path(output_dir, paste0("indices_por_cluster_k4_", timestamp, ".csv")))

cat("✅ Análise concluída com k = ", k, ".\n", sep = "")
cat("📁 Arquivos salvos em:\n -", output_csv, "\n -", output_xlsx, "\n")
cat("📊 Gráficos salvos em:", output_dir, "\n")
