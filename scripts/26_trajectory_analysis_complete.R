# ================================================================
# 26 - ANÁLISE COMPLETA DE TRAJETÓRIAS — VERSÃO TRATE + EVENTOS (k = 4)
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

# ---------- Entrada ----------
input_files <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("❌ Nenhum arquivo CSV de trajetória encontrado.")
input_file <- tail(sort(input_files), 1)

# ---------- Parâmetros ----------
estados_validos <- c("ALT","ENF","UCO","UCA","UTI","OBI")
week_cols <- as.character(1:30)
k <- 4

# ---------- Leitura e limpeza ----------
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

# ---------- Objeto de sequência ----------
seq_obj <- TraMineR::seqdef(traj_clean[, ..week_cols],
                            alphabet = estados_validos, states = estados_validos)

# ---------- Gráfico: distribuição geral ----------
png(file.path(output_dir, paste0("plot_distribuicao_geral_", timestamp, ".png")),
    width = 1000, height = 600)
TraMineR::seqfplot(seq_obj, with.legend = "right",
                   main = paste0("Distribuição de Estados por Semana (n=", nrow(seq_obj), ")"))
dev.off()

# ---------- Distâncias OM com TRATE ----------
# Matriz de substituição guiada pelas TAXAS DE TRANSIÇÃO observadas
sm_trate <- TraMineR::seqsubm(seq_obj, method = "TRATE")
# Custo de inserção/remoção (indel) = 1 (padrão; ajuste se necessário)
dist_mat <- TraMineR::seqdist(seq_obj, method = "OM", indel = 1, sm = sm_trate)

# ---------- Diagnóstico de k (silhouette) ----------
png(file.path(output_dir, paste0("diagnostico_silhouette_TRATE_", timestamp, ".png")),
    width = 1000, height = 600)
factoextra::fviz_nbclust(as.matrix(dist_mat), FUN = hcut,
                          method = "silhouette", k.max = 6) +
  labs(title = "Diagnóstico: Silhouette vs k (Ward + OM/TRATE)")
dev.off()

# ---------- Clusterização (Ward) com TRATE ----------
agnes_fit <- cluster::agnes(dist_mat, diss = TRUE, method = "ward")
hc   <- as.hclust(agnes_fit)
traj_clean$cluster <- paste0("type ", cutree(hc, k = k))

# ---------- Dendrograma ----------
dend <- stats::as.dendrogram(hc)
dend <- dendextend::color_branches(dend, k = k, col = viridisLite::viridis(k))
png(file.path(output_dir, paste0("dendrograma_trajetorias_k", k, "_TRATE_", timestamp, ".png")),
    width = 1000, height = 600)
plot(dend, main = paste0("Dendrograma das Trajetórias (k = ", k, ", TRATE)"))
dendextend::rect.dendrogram(dend, k = k, border = "grey30", lty = 1)
dev.off()

# ---------- Trajetórias por cluster ----------
png(file.path(output_dir, paste0("trajetorias_por_cluster_k", k, "_TRATE_", timestamp, ".png")),
    width = 1200, height = 750)
TraMineR::seqdplot(seq_obj, group = traj_clean$cluster, border = NA,
                   main = paste0("Trajetórias por Cluster (k = ", k, ", TRATE)"))
dev.off()

# ---------- Heatmap médio por cluster ----------
traj_melt <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_summary <- traj_melt |>
  dplyr::group_by(cluster, estado) |>
  dplyr::summarise(prop = dplyr::n() / nrow(traj_clean), .groups = "drop")

png(file.path(output_dir, paste0("heatmap_por_cluster_k", k, "_TRATE_", timestamp, ".png")),
    width = 1000, height = 600)
ggplot(traj_summary, aes(x = cluster, y = estado, fill = prop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = paste0("Distribuição Média de Estados por Cluster (k = ", k, ", TRATE)"),
       x = "Cluster", y = "Estado") +
  theme_minimal()
dev.off()

# ---------- Curva acumulada de estados ----------
traj_long <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.integer(as.character(traj_long$semana))
curva_estados <- traj_long |>
  dplyr::group_by(semana, estado) |>
  dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(semana) |>
  dplyr::mutate(prop = freq / sum(freq))

png(file.path(output_dir, paste0("curva_acumulada_estados_k", k, "_TRATE_", timestamp, ".png")),
    width = 1000, height = 600)
ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
  geom_area(alpha = 0.7, color = "white") +
  labs(title = paste0("Proporção Acumulada dos Estados por Semana (k = ", k, ", TRATE)"),
       x = "Semana", y = "Proporção") +
  theme_minimal()
dev.off()

# ---------- Entropia por sequência / Turbulência / Transições ----------
entropy_seq     <- TraMineR::seqient(seq_obj)        # Entropia de Shannon (por sequência)
turbulence_seq  <- TraMineR::seqST(seq_obj)          # Turbulência (por sequência)
transitions_seq <- TraMineR::seqtransn(seq_obj)      # Nº de mudanças de estado (por sequência)

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
    n                 = dplyr::n(),
    entropy_mean      = mean(entropy, na.rm = TRUE),
    entropy_sd        = sd(entropy, na.rm = TRUE),
    entropy_median    = median(entropy, na.rm = TRUE),
    turbulence_mean   = mean(turbulence, na.rm = TRUE),
    turbulence_sd     = sd(turbulence, na.rm = TRUE),
    turbulence_median = median(turbulence, na.rm = TRUE),
    trans_mean        = mean(transitions, na.rm = TRUE),
    trans_sd          = sd(transitions, na.rm = TRUE),
    trans_median      = median(transitions, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(cluster)

# ---------- Boxplots ----------
png(file.path(output_dir, paste0("boxplot_entropia_por_cluster_k", k, "_TRATE_", timestamp, ".png")),
    width = 900, height = 600)
ggplot(indices_seq, aes(x = cluster, y = entropy, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = paste0("Entropia por Cluster (k = ", k, ", TRATE)"),
       x = "Cluster", y = "Entropia (Shannon)") +
  theme_minimal() + theme(legend.position = "none")
dev.off()

png(file.path(output_dir, paste0("boxplot_turbulencia_por_cluster_k", k, "_TRATE_", timestamp, ".png")),
    width = 900, height = 600)
ggplot(indices_seq, aes(x = cluster, y = turbulence, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = paste0("Turbulência por Cluster (k = ", k, ", TRATE)"),
       x = "Cluster", y = "Turbulência") +
  theme_minimal() + theme(legend.position = "none")
dev.off()

png(file.path(output_dir, paste0("boxplot_transicoes_por_cluster_k", k, "_TRATE_", timestamp, ".png")),
    width = 900, height = 600)
ggplot(indices_seq, aes(x = cluster, y = transitions, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = paste0("Número de Transições por Cluster (k = ", k, ", TRATE)"),
       x = "Cluster", y = "Transições") +
  theme_minimal() + theme(legend.position = "none")
dev.off()

# ---------- Entropia por tempo (heterogeneidade semanal da coorte) ----------
png(file.path(output_dir, paste0("entropia_por_tempo_TRATE_", timestamp, ".png")),
    width = 1000, height = 600)
TraMineR::seqHtplot(seq_obj, main = "Entropia da distribuição de estados por semana (TRATE)")
dev.off()

# ===================== ANÁLISE DE EVENTOS =====================
# Foco em transições/subsequências (complementar à análise por estados)

# 1) Sequences of events
seqe <- TraMineR::seqecreate(seq_obj)

# 2) Subsequências frequentes (suporte mínimo = 5%; ajuste se necessário)
fsub <- TraMineR::seqefsub(seqe, pMinSupport = 0.05)

# Data frame com resumo das subsequências frequentes
fsub_summary <- summary(fsub)  # contém subseq, size, support, etc.

# Plot das 15 subsequências mais frequentes
png(file.path(output_dir, paste0("subsequencias_frequentes_top15_TRATE_", timestamp, ".png")),
    width = 1000, height = 600)
plot(fsub[1:min(15, length(fsub))])
dev.off()

# 3) Transições/subsequências mais discriminantes entre clusters
cl_fac <- factor(traj_clean$cluster)
discr <- TraMineR::seqecmpgroup(fsub, group = cl_fac)

# O objeto 'discr' tem resumo textual útil; salvamos num .txt
sink(file.path(output_dir, paste0("transicoes_discriminantes_sumario_TRATE_", timestamp, ".txt")))
cat("== RESUMO: transições/subsequências discriminantes por cluster ==\n")
print(summary(discr))
sink()

# Gráfico das 6 primeiras transições mais discriminantes
png(file.path(output_dir, paste0("transicoes_discriminantes_top6_TRATE_", timestamp, ".png")),
    width = 1000, height = 600)
plot(discr[1:min(6, length(discr))])
dev.off()

# -------------------- Exportações --------------------
output_csv  <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_k", k, "_TRATE_", timestamp, ".csv"))
output_xlsx <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_k", k, "_TRATE_", timestamp, ".xlsx"))

data.table::fwrite(traj_clean, output_csv)

# Exporta resumos principais em um único XLSX
writexl::write_xlsx(
  list(
    "traj_clusters"         = traj_clean,
    "heatmap_summary"       = traj_summary,
    "curva_estados"         = curva_estados,
    "indices_por_seq"       = indices_seq,
    "indices_por_cluster"   = indices_por_cluster,
    "subseq_frequentes"     = as.data.frame(fsub_summary)
    # Sumário detalhado das discriminantes foi salvo como .txt (mais completo)
  ),
  path = output_xlsx
)

# CSVs separados úteis
data.table::fwrite(indices_seq,           file.path(output_dir, paste0("indices_por_seq_k", k, "_TRATE_", timestamp, ".csv")))
data.table::fwrite(indices_por_cluster,   file.path(output_dir, paste0("indices_por_cluster_k", k, "_TRATE_", timestamp, ".csv")))
data.table::fwrite(as.data.frame(fsub_summary), file.path(output_dir, paste0("subseq_frequentes_TRATE_", timestamp, ".csv")))

cat("✅ Análise (TRATE + eventos) concluída com k = ", k, ".\n", sep = "")
cat("📁 Arquivos salvos em:\n -", output_csv, "\n -", output_xlsx, "\n")
cat("📊 Gráficos e sumários salvos em:", output_dir, "\n")
