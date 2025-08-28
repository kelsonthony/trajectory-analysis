# ================================================================
# 05 - ANÁLISE COMPLETA DE TRAJETÓRIAS — VERSÃO FINAL (k = 4)
# ================================================================

# Pacotes
packages_needed <- c(
  "TraMineR","data.table","cluster","factoextra","ggplot2",
  "reshape2","dplyr","readxl","dendextend","viridisLite","writexl"
)
to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages_needed, library, character.only = TRUE))

# Caminhos
input_dir  <- "data/output"
output_dir <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
timestamp  <- format(Sys.time(), "%Y%m%d_%H%M")

# Localiza o CSV de trajetória mais recente
input_files <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("❌ Nenhum arquivo CSV de trajetória encontrado.")
input_file <- tail(sort(input_files), 1)

# Estados e colunas
estados_validos <- c("ALT","ENF","UCO","UCA","UTI","OBI")
week_cols <- as.character(1:30)

# ========================= Leitura e limpeza =========================
traj <- data.table::fread(input_file, sep = ";", encoding = "UTF-8")

traj_clean <- traj[, ..week_cols] |>
  lapply(function(col){
    x <- toupper(trimws(as.character(col)))
    x[!(x %in% estados_validos)] <- NA
    factor(x, levels = estados_validos)
  }) |>
  as.data.table()

# Mantém id para rastreio e remove linhas com NA
traj_clean[, id := .I]
traj_clean <- traj_clean[complete.cases(traj_clean)]
cat("✅ Total de linhas válidas para análise:", nrow(traj_clean), "\n")

# ========================= Sequences & Distâncias =====================
seq_obj <- TraMineR::seqdef(traj_clean[, ..week_cols], alphabet = estados_validos, states = estados_validos)

# Distribuição geral
png(file.path(output_dir, paste0("plot_distribuicao_geral_", timestamp, ".png")), width = 1000, height = 600)
TraMineR::seqfplot(seq_obj, with.legend = "right",
                   main = paste0("Distribuição de Estados por Semana (n=", nrow(seq_obj), ")"))
dev.off()

# Distância OM com matriz de substituição CONSTANT (custo 2)
sm_const <- TraMineR::seqsubm(seq_obj, method = "CONSTANT", cval = 2)
dist_mat <- TraMineR::seqdist(seq_obj, method = "OM", indel = 1, sm = sm_const)

# (Opcional) Curva silhouette como diagnóstico (k variando)
png(file.path(output_dir, paste0("diagnostico_silhouette_", timestamp, ".png")), width = 1000, height = 600)
factoextra::fviz_nbclust(as.matrix(dist_mat), FUN = hcut, method = "silhouette", k.max = 6) +
  labs(title = "Diagnóstico: Silhouette vs k (Ward + OM)")
dev.off()

# ========================= Clusterização final (k=4) ==================
k <- 4
agnes_fit <- cluster::agnes(dist_mat, diss = TRUE, method = "ward")
traj_clean$cluster <- paste0("type ", cutree(as.hclust(agnes_fit), k = k))

# Dendrograma colorido
hc   <- as.hclust(agnes_fit)
dend <- stats::as.dendrogram(hc)
dend <- dendextend::color_branches(dend, k = k, col = viridisLite::viridis(k))
png(file.path(output_dir, paste0("dendrograma_trajetorias_k4_", timestamp, ".png")), width = 1000, height = 600)
plot(dend, main = "Dendrograma das Trajetórias (k = 4)")
dendextend::rect.dendrogram(dend, k = k, border = "grey30", lty = 1)
dev.off()

# Painel de trajetórias por cluster
png(file.path(output_dir, paste0("trajetorias_por_cluster_k4_", timestamp, ".png")), width = 1200, height = 750)
TraMineR::seqdplot(seq_obj, group = traj_clean$cluster, border = NA, main = "Trajetórias por Cluster (k = 4)")
dev.off()

# ========================= Resumos/Gráficos auxiliares ================
# Heatmap médio por cluster
traj_melt <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_summary <- traj_melt |>
  dplyr::group_by(cluster, estado) |>
  dplyr::summarise(prop = dplyr::n() / nrow(traj_clean), .groups = "drop")

png(file.path(output_dir, paste0("heatmap_por_cluster_k4_", timestamp, ".png")), width = 1000, height = 600)
ggplot(traj_summary, aes(x = cluster, y = estado, fill = prop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Distribuição Média de Estados por Cluster (k = 4)", x = "Cluster", y = "Estado") +
  theme_minimal()
dev.off()

# Curva acumulada por semana (toda a coorte)
traj_long <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.integer(as.character(traj_long$semana))
curva_estados <- traj_long |>
  dplyr::group_by(semana, estado) |>
  dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(semana) |>
  dplyr::mutate(prop = freq / sum(freq))

png(file.path(output_dir, paste0("curva_acumulada_estados_k4_", timestamp, ".png")), width = 1000, height = 600)
ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
  geom_area(alpha = 0.7, color = "white") +
  labs(title = "Proporção Acumulada dos Estados por Semana (k = 4)", x = "Semana", y = "Proporção") +
  theme_minimal()
dev.off()

# ========================= Exportações ================================
# Arquivos de saída
output_csv  <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_k4_", timestamp, ".csv"))
output_xlsx <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_k4_", timestamp, ".xlsx"))

# CSV
data.table::fwrite(traj_clean, output_csv)

# XLSX (múltiplas abas: dados, resumo heatmap, curva acumulada)
writexl::write_xlsx(
  list(
    "traj_clusters"  = traj_clean,
    "heatmap_summary" = traj_summary,
    "curva_estados"   = curva_estados
  ),
  path = output_xlsx
)

cat("✅ Análise concluída com k = 4.\n")
cat("📁 Arquivos com cluster salvos em:\n -", output_csv, "\n -", output_xlsx, "\n")
cat("📊 Gráficos salvos em:", output_dir, "\n")
