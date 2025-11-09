# ================================================================
# 05 - ANÁLISE COMPLETA DE TRAJETÓRIAS — VERSÃO FINAL (k = 4)
# Painel 2x2 com letras A–D (halo) e legenda maior + melhorias
# ================================================================

packages_needed <- c(
  "TraMineR","data.table","cluster","factoextra","ggplot2",
  "reshape2","dplyr","readxl","dendextend","viridisLite","writexl"
)
to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages_needed, library, character.only = TRUE))

# --- IO ----------------------------------------------------------
input_dir  <- "data/output"
output_dir <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
timestamp  <- format(Sys.time(), "%Y%m%d_%H%M")

input_files <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("❌ Nenhum arquivo CSV de trajetória encontrado.")
input_file <- tail(sort(input_files), 1)

# --- Parâmetros --------------------------------------------------
estados_validos <- c("ALT","ENF","UCO","UCA","UTI","OBI")
week_cols <- as.character(1:30)
k <- 4
rotulos_paineis <- LETTERS[1:k]   # A, B, C, D
legend_cex      <- 1.6            # legenda maior
letter_cex      <- 2.0            # letras A–D
export_res      <- 300            # 300 DPI para pôster

# (opcional) preencher NAs finais com o último estado observado
preencher_na_finais <- FALSE

# --- Leitura e limpeza -------------------------------------------
traj <- data.table::fread(input_file, sep = ";", encoding = "UTF-8")

traj_clean <- traj[, ..week_cols] |>
  lapply(function(col){
    x <- toupper(trimws(as.character(col)))
    x[!(x %in% estados_validos)] <- NA
    factor(x, levels = estados_validos)
  }) |>
  as.data.table()

if (preencher_na_finais) {
  fill_last <- function(v) {
    last <- NA
    for (i in seq_along(v)) {
      if (!is.na(v[i])) last <- v[i] else v[i] <- last
    }
    v
  }
  traj_char <- as.data.frame(lapply(traj_clean, as.character))
  traj_char <- as.data.table(t(apply(traj_char, 1, fill_last)))
  setnames(traj_char, names(traj_clean))
  traj_clean <- as.data.table(lapply(traj_char, \(x) factor(x, levels = estados_validos)))
}

traj_clean[, id := .I]
traj_clean <- traj_clean[complete.cases(traj_clean)]
cat("✅ Total de linhas válidas para análise:", nrow(traj_clean), "\n")

# --- Definição da sequência -------------------------------------
seq_obj <- TraMineR::seqdef(traj_clean[, ..week_cols],
                            alphabet = estados_validos, states = estados_validos)

# --- Distribuição geral ------------------------------------------
png(file.path(output_dir, paste0("plot_distribuicao_geral_", timestamp, ".png")),
    width = 1200, height = 700, res = 120)
TraMineR::seqfplot(seq_obj, with.legend = "right",
                   main = paste0("Distribuição de Estados por Semana (n=", nrow(seq_obj), ")"))
dev.off()

# --- Distâncias (OM) e HC (Ward) ---------------------------------
sm_const <- TraMineR::seqsubm(seq_obj, method = "CONSTANT", cval = 2)
dist_mat <- TraMineR::seqdist(seq_obj, method = "OM", indel = 1, sm = sm_const)

agnes_fit <- cluster::agnes(dist_mat, diss = TRUE, method = "ward")
hc   <- as.hclust(agnes_fit)

# --- Diagnóstico k (silhouette manual) ---------------------------
ks <- 2:6
sil_values <- sapply(ks, function(kk) {
  cl <- cutree(hc, k = kk)
  sil <- cluster::silhouette(cl, as.dist(dist_mat))
  mean(sil[, "sil_width"])
})
sil_df <- data.frame(k = ks, silhouette = as.numeric(sil_values))

png(file.path(output_dir, paste0("diagnostico_silhouette_", timestamp, ".png")),
    width = 1200, height = 700, res = 120)
ggplot(sil_df, aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = ks) +
  labs(title = "Diagnóstico: Silhouette vs k (Ward + OM)",
       x = "k", y = "Silhouette médio") +
  theme_minimal(base_size = 14)
dev.off()

# --- Clusters 1..k em português ---------------------------------
cl_num <- cutree(hc, k = k)
traj_clean$cluster <- factor(paste0("tipo ", cl_num),
                             levels = paste0("tipo ", 1:k), ordered = TRUE)

# --- Dendrograma -------------------------------------------------
dend <- stats::as.dendrogram(hc)
dend <- dendextend::color_branches(dend, k = k, col = viridisLite::viridis(k))
png(file.path(output_dir, paste0("dendrograma_trajetorias_k4_", timestamp, ".png")),
    width = 1200, height = 700, res = 120)
plot(dend, main = "Dendrograma das Trajetórias (k = 4)")
dendextend::rect.dendrogram(dend, k = k, border = "grey30", lty = 1)
dev.off()

# --- PAINEL 2x2 + LEGENDA CENTRALIZADA (letras mais baixas) ---
png(file.path(output_dir, paste0("trajetorias_por_cluster_k4_rotuladas_", timestamp, ".png")),
    width = 3600, height = 3400, res = export_res)

# layout: 3 linhas (a 3ª é a faixa única da legenda)
lay <- matrix(c(1, 2,
                3, 4,
                5, 5), nrow = 3, byrow = TRUE)
layout(lay, heights = c(0.42, 0.42, 0.16))

# >>> margens maiores embaixo para caber as letras folgadas
mar_bottom   <- 6.8     # antes 5.6
letter_line  <- 4.4     # antes 3.0 -> letras mais abaixo

par(mar = c(mar_bottom, 3.2, 2.6, 1.0),
    xaxs = "i", yaxs = "i",
    cex.axis = 1.0, cex.lab = 1.0)

for (i in 1:k) {
  idx <- which(traj_clean$cluster == paste0("tipo ", i))
  seq_i <- TraMineR::seqdef(traj_clean[idx, ..week_cols],
                            alphabet = estados_validos, states = estados_validos)
  attr(seq_i, "cpal") <- attr(seq_obj, "cpal")

  TraMineR::seqdplot(seq_i, with.legend = FALSE, border = NA,
                     main = paste0("Trajetórias por Cluster (k = ", k, ") - tipo ", i))

  # Letras A–D no canto inferior direito, FORA do gráfico (na margem)
  mtext(rotulos_paineis[i], side = 1, line = letter_line, adj = 1,
        font = 2, cex = letter_cex * 1.25, col = "white")
  mtext(rotulos_paineis[i], side = 1, line = letter_line, adj = 1,
        font = 2, cex = letter_cex, col = "black")
}

# Faixa da LEGENDA – centralizada e longe dos painéis
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = estados_validos,
       fill   = attr(seq_obj, "cpal"),
       horiz  = TRUE, cex = legend_cex,
       box.lwd = 1, bty = "o", xpd = NA)

dev.off()




# --- Heatmap médio por cluster (proporção dentro do cluster) ----
traj_melt <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")

traj_summary <- traj_melt |>
  dplyr::group_by(cluster, estado) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
  dplyr::group_by(cluster) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

png(file.path(output_dir, paste0("heatmap_por_cluster_k4_", timestamp, ".png")),
    width = 1200, height = 700, res = 120)
ggplot(traj_summary, aes(x = cluster, y = estado, fill = prop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Distribuição Média de Estados por Cluster (k = 4)",
       x = "Cluster", y = "Estado") +
  theme_minimal(base_size = 14)
dev.off()

# --- Curva acumulada dos estados --------------------------------
traj_long <- reshape2::melt(traj_clean[, c("cluster", ..week_cols)],
                            id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.integer(as.character(traj_long$semana))
curva_estados <- traj_long |>
  dplyr::group_by(semana, estado) |>
  dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(semana) |>
  dplyr::mutate(prop = freq / sum(freq))

png(file.path(output_dir, paste0("curva_acumulada_estados_k4_", timestamp, ".png")),
    width = 1200, height = 700, res = 120)
ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
  geom_area(alpha = 0.75, color = "white", linewidth = 0.2) +
  labs(title = "Proporção Acumulada dos Estados por Semana (k = 4)",
       x = "Semana", y = "Proporção") +
  theme_minimal(base_size = 14)
dev.off()

# --- Índices por sequência e por cluster -------------------------
entropy_seq     <- TraMineR::seqient(seq_obj)       # Entropia de Shannon
turbulence_seq  <- TraMineR::seqST(seq_obj)         # Turbulência
transitions_seq <- TraMineR::seqtransn(seq_obj)     # Nº de mudanças de estado

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

# Boxplots (texto maior)
png(file.path(output_dir, paste0("boxplot_entropia_por_cluster_k4_", timestamp, ".png")),
    width = 1100, height = 700, res = 120)
ggplot(indices_seq, aes(x = cluster, y = entropy, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Entropia por Cluster (k = 4)", x = "Cluster", y = "Entropia (Shannon)") +
  theme_minimal(base_size = 14) + theme(legend.position = "none")
dev.off()

png(file.path(output_dir, paste0("boxplot_turbulencia_por_cluster_k4_", timestamp, ".png")),
    width = 1100, height = 700, res = 120)
ggplot(indices_seq, aes(x = cluster, y = turbulence, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Turbulência por Cluster (k = 4)", x = "Cluster", y = "Turbulência") +
  theme_minimal(base_size = 14) + theme(legend.position = "none")
dev.off()

png(file.path(output_dir, paste0("boxplot_transicoes_por_cluster_k4_", timestamp, ".png")),
    width = 1100, height = 700, res = 120)
ggplot(indices_seq, aes(x = cluster, y = transitions, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Número de Transições por Cluster (k = 4)", x = "Cluster", y = "Transições") +
  theme_minimal(base_size = 14) + theme(legend.position = "none")
dev.off()

# --- Saídas tabulares --------------------------------------------
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

data.table::fwrite(indices_seq,         file.path(output_dir, paste0("indices_por_seq_k4_", timestamp, ".csv")))
data.table::fwrite(indices_por_cluster, file.path(output_dir, paste0("indices_por_cluster_k4_", timestamp, ".csv")))

cat("✅ Análise concluída com k = ", k, ".\n", sep = "")
cat("📁 Arquivos salvos em:\n -", output_csv, "\n -", output_xlsx, "\n")
cat("🖼  Gráfico (painel A–D): trajetorias_por_cluster_k4_rotuladas_", timestamp, ".png\n", sep = "")
