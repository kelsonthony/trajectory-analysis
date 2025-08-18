# ================================================================
# 05 - ANÁLISE COMPLETA DE TRAJETÓRIAS (COM AUDITORIA DE k) — corrigido
# ================================================================

# Pacotes
packages_needed <- c(
  "TraMineR","data.table","cluster","factoextra","ggplot2",
  "reshape2","dplyr","readxl","dendextend","viridisLite","NbClust"
)
to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages_needed, library, character.only = TRUE))

# Caminhos
input_dir  <- "data/output"
output_dir <- "data/output"
timestamp  <- format(Sys.time(), "%Y%m%d_%H%M")

# Localiza o CSV de trajetória mais recente
input_files <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("❌ Nenhum arquivo CSV de trajetória encontrado.")
input_file <- tail(sort(input_files), 1)

# Estados
estados_validos <- c("ALT","ENF","UCO","UCA","UTI","OBI")
week_cols <- as.character(1:30)

# Leitura
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

# Sequences
seq_obj <- TraMineR::seqdef(traj_clean[, ..week_cols], alphabet = estados_validos, states = estados_validos)

# Distribuição geral
png(file.path(output_dir, paste0("plot_distribuicao_geral_", timestamp, ".png")), width = 1000, height = 600)
TraMineR::seqfplot(seq_obj, with.legend = "right",
                   main = paste0("Distribuição de Estados por Semana (n=", nrow(seq_obj), ")"))
dev.off()

# ------------------ DISTÂNCIA OM (baseline) --------------------
# Cria matriz de substituição CONSTANT com custo 2
sm_const <- TraMineR::seqsubm(seq_obj, method = "CONSTANT", cval = 2)
# Agora calcula OM usando 'sm_const' e indel = 1
dist_mat <- TraMineR::seqdist(seq_obj, method = "OM", indel = 1, sm = sm_const)

# Silhouette: escolher k
png(file.path(output_dir, paste0("escolha_numero_grupos_silhouette_", timestamp, ".png")), width = 1000, height = 600)
factoextra::fviz_nbclust(as.matrix(dist_mat), FUN = hcut, method = "silhouette", k.max = 6) +
  labs(title = "Número Ótimo de Grupos - Silhouette (Ward + OM)")
dev.off()

# Clusterização final (k=5)
k <- 5
agnes_fit <- cluster::agnes(dist_mat, diss = TRUE, method = "ward")
traj_clean$cluster <- paste0("type ", cutree(as.hclust(agnes_fit), k = k))

# Dendrograma colorido
hc   <- as.hclust(agnes_fit)
dend <- stats::as.dendrogram(hc)
dend <- dendextend::color_branches(dend, k = k, col = viridisLite::viridis(k))
png(file.path(output_dir, paste0("dendrograma_trajetorias_", timestamp, ".png")), width = 1000, height = 600)
plot(dend, main = "Dendrograma das Trajetórias (colorido por cluster)")
dendextend::rect.dendrogram(dend, k = k, border = "grey30", lty = 1)
dev.off()

# Painel de trajetórias por cluster
png(file.path(output_dir, paste0("trajetorias_por_cluster_", timestamp, ".png")), width = 1000, height = 600)
TraMineR::seqdplot(seq_obj, group = traj_clean$cluster, border = NA, main = "Trajetórias por Cluster")
dev.off()

# Heatmap médio por cluster
traj_melt <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_summary <- traj_melt |>
  dplyr::group_by(cluster, estado) |>
  dplyr::summarise(prop = dplyr::n() / nrow(traj_clean), .groups = "drop")
png(file.path(output_dir, paste0("heatmap_por_cluster_", timestamp, ".png")), width = 1000, height = 600)
ggplot(traj_summary, aes(x = cluster, y = estado, fill = prop)) +
  geom_tile() + scale_fill_viridis_c() +
  labs(title = "Distribuição Média de Estados por Cluster", x = "Cluster", y = "Estado") +
  theme_minimal()
dev.off()

# Curva acumulada por semana
traj_long <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.integer(as.character(traj_long$semana))
curva_estados <- traj_long |>
  dplyr::group_by(semana, estado) |>
  dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(semana) |>
  dplyr::mutate(prop = freq / sum(freq))
png(file.path(output_dir, paste0("curva_acumulada_estados_", timestamp, ".png")), width = 1000, height = 600)
ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
  geom_area(alpha = 0.7, color = "white") +
  labs(title = "Proporção Acumulada dos Estados por Semana", x = "Semana", y = "Proporção") +
  theme_minimal()
dev.off()

# Exporta CSV e XLSX final com clusters
output_csv  <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_", timestamp, ".csv"))
output_xlsx <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_", timestamp, ".xlsx"))

# CSV
data.table::fwrite(traj_clean, output_csv)

# XLSX (usa writexl ou openxlsx)
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
writexl::write_xlsx(traj_clean, output_xlsx)

cat("✅ Análise principal (k=5) concluída.\n")
cat("📁 Arquivo com cluster salvo em:", output_csv, " e ", output_xlsx, "\n")

# ================================================================
# *********************  AUDITORIA DE k  *************************
# ================================================================

k_range <- 2:8

compute_silhouette_curve <- function(dist_obj, k_range) {
  hc <- hclust(as.dist(dist_obj), method = "ward.D2")
  res <- lapply(k_range, function(k){
    cl  <- cutree(hc, k = k)
    sil <- cluster::silhouette(cl, as.dist(dist_obj))
    data.frame(k = k, sil_mean = mean(sil[, "sil_width"]))
  })
  do.call(rbind, res)
}

# (1) Silhouette detalhado (baseline)
sil_curve <- compute_silhouette_curve(dist_mat, k_range)
png(file.path(output_dir, paste0("auditoria_silhouette_curve_", timestamp, ".png")), width = 900, height = 500)
ggplot(sil_curve, aes(k, sil_mean)) +
  geom_line() + geom_point() +
  labs(title = "Silhouette médio vs k (Ward + OM baseline)", x = "k", y = "Silhouette médio") +
  theme_minimal()
dev.off()

# (2) Grid de custos OM — criar sm para cada cval
indel_vals <- c(0.5, 1, 1.5)
cval_vals  <- c(1.5, 2, 3)
grid_results <- list()

for (indel_v in indel_vals) {
  for (cval_v in cval_vals) {
    smg <- TraMineR::seqsubm(seq_obj, method = "CONSTANT", cval = cval_v)
    dm  <- TraMineR::seqdist(seq_obj, method = "OM", indel = indel_v, sm = smg)
    sc  <- compute_silhouette_curve(dm, k_range)
    sc$indel <- indel_v
    sc$cval  <- cval_v
    sc$k_best <- sc$k[which.max(sc$sil_mean)]
    grid_results[[paste(indel_v, cval_v, sep = "_")]] <- sc
  }
}
grid_df <- dplyr::bind_rows(grid_results)
data.table::fwrite(grid_df, file.path(output_dir, paste0("auditoria_grid_OM_silhouette_", timestamp, ".csv")))

k_star <- grid_df |>
  dplyr::group_by(indel, cval) |>
  dplyr::summarise(k_best = k[which.max(sil_mean)], .groups = "drop")
png(file.path(output_dir, paste0("auditoria_grid_kbest_", timestamp, ".png")), width = 700, height = 480)
ggplot(k_star, aes(x = factor(indel), y = factor(cval), fill = factor(k_best))) +
  geom_tile(color = "white") +
  geom_text(aes(label = k_best)) +
  scale_fill_viridis_d() +
  labs(title = "k* (maior silhouette) por custos OM", x = "indel", y = "cval", fill = "k*") +
  theme_minimal()
dev.off()

# (3) MDS + Gap Statistic
mds_dim <- 5
coords <- cmdscale(as.matrix(dist_mat), k = mds_dim)
set.seed(123)
gap <- cluster::clusGap(coords, FUN = stats::kmeans, K.max = max(k_range), B = 50)
png(file.path(output_dir, paste0("auditoria_gap_stat_", timestamp, ".png")), width = 900, height = 500)
factoextra::fviz_gap_stat(gap) + labs(title = "Gap Statistic (k-means em MDS das trajetórias)")
dev.off()

# (4) MDS + NbClust
nb <- tryCatch({
  NbClust::NbClust(
    data = coords, distance = "euclidean",
    min.nc = min(k_range), max.nc = max(k_range),
    method = "kmeans", index = "alllong"
  )
}, error = function(e) NULL)

if (!is.null(nb)) {
  tab <- as.data.frame(nb$Best.nc)
  best_k <- as.integer(na.omit(tab[1, ]))
  nb_summary <- as.data.frame(table(best_k))
  names(nb_summary) <- c("k", "votes")
  nb_summary$k <- as.integer(as.character(nb_summary$k))
  data.table::fwrite(nb_summary, file.path(output_dir, paste0("auditoria_nbclust_votes_", timestamp, ".csv")))
  png(file.path(output_dir, paste0("auditoria_nbclust_votes_", timestamp, ".png")), width = 800, height = 480)
  ggplot(nb_summary, aes(x = k, y = votes)) +
    geom_col() + geom_text(aes(label = votes), vjust = -0.3) +
    labs(title = "NbClust: votos por k (kmeans em MDS)", x = "k", y = "nº de índices que sugerem k") +
    theme_minimal()
  dev.off()
} else {
  cat("⚠️ NbClust não rodou (possível falta de memória/tempo). Tente diminuir K.max ou MDS.\n")
}

cat("🔎 Auditoria concluída. Arquivos salvos em:", output_dir, "\n")
