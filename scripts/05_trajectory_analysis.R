# ================================================================
# 05 - ANÁLISE COMPLETA DE TRAJETÓRIAS (CORRIGIDO FINAL)
# ================================================================

library(TraMineR)
library(data.table)
library(cluster)
library(factoextra)
library(ggplot2)
library(reshape2)
library(dplyr)

# Caminhos
input_dir <- "data/output"
output_dir <- "data/output"
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

# Localiza o CSV de trajetória mais recente
input_files <- list.files(input_dir, pattern = "^trajetoria_.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) stop("❌ Nenhum arquivo CSV de trajetória encontrado.")
input_file <- tail(sort(input_files), 1)

# Estados válidos (sem DEL)
estados_validos <- c("ALT", "ENF", "UCO", "UCA", "UTI", "OBI")
week_cols <- as.character(1:30)

# Leitura do CSV
traj <- fread(input_file, sep = ";", encoding = "UTF-8")

# Pré-processamento: padroniza para maiúsculo, remove espaços, trata inválidos
traj_clean <- traj[, ..week_cols] |> 
  lapply(function(col) {
    x <- toupper(trimws(as.character(col)))
    x[!(x %in% estados_validos)] <- NA
    factor(x, levels = estados_validos)
  }) |> 
  as.data.table()

# Remove linhas com qualquer NA
traj_clean[, id := .I]
traj_clean <- traj_clean[complete.cases(traj_clean)]
cat("✅ Total de linhas válidas para análise:", nrow(traj_clean), "\n")

# Criação do objeto de sequência
seq_obj <- seqdef(traj_clean[, ..week_cols], alphabet = estados_validos, states = estados_validos)

# Gráfico geral da distribuição dos estados
png(file.path(output_dir, paste0("plot_distribuicao_geral_", timestamp, ".png")), width = 1000, height = 600)
seqfplot(seq_obj, with.legend = "right", main = paste0("Distribuição de Estados por Semana (n=", nrow(seq_obj), ")"))
dev.off()

# Distâncias entre sequências
dist_mat <- seqdist(seq_obj, method = "OM", indel = 1, sm = "CONSTANT")

# Silhouette para número ótimo de clusters
png(file.path(output_dir, paste0("escolha_numero_grupos_silhouette_", timestamp, ".png")), width = 1000, height = 600)
fviz_nbclust(as.matrix(dist_mat), FUN = hcut, method = "silhouette", k.max = 6) +
  labs(title = "Número Ótimo de Grupos - Método Silhouette")
dev.off()

# Clusterização hierárquica
k <- 5
agnes_fit <- agnes(dist_mat, diss = TRUE, method = "ward")
traj_clean$cluster <- paste0("type ", cutree(agnes_fit, k = k))

# Dendrograma
png(file.path(output_dir, paste0("dendrograma_trajetorias_", timestamp, ".png")), width = 1000, height = 600)
plot(agnes_fit, which.plots = 2, main = "Dendrograma das Trajetórias")
dev.off()

# Gráfico de trajetórias por cluster
png(file.path(output_dir, paste0("trajetorias_por_cluster_", timestamp, ".png")), width = 1000, height = 600)
seqdplot(seq_obj, group = traj_clean$cluster, border = NA, main = "Trajetórias por Cluster")
dev.off()

# Heatmap por cluster
traj_melt <- melt(traj_clean[, c("cluster", ..week_cols)], id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_summary <- traj_melt |>
  group_by(cluster, estado) |>
  summarise(prop = n() / nrow(traj_clean), .groups = "drop")

png(file.path(output_dir, paste0("heatmap_por_cluster_", timestamp, ".png")), width = 1000, height = 600)
ggplot(traj_summary, aes(x = cluster, y = estado, fill = prop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Distribuição Média de Estados por Cluster", x = "Cluster", y = "Estado") +
  theme_minimal()
dev.off()

# Curva acumulada por semana
traj_long <- melt(traj_clean[, c("cluster", ..week_cols)], id.vars = "cluster", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.integer(as.character(traj_long$semana))

curva_estados <- traj_long |>
  group_by(semana, estado) |>
  summarise(freq = n(), .groups = "drop") |>
  group_by(semana) |>
  mutate(prop = freq / sum(freq))

png(file.path(output_dir, paste0("curva_acumulada_estados_", timestamp, ".png")), width = 1000, height = 600)
ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
  geom_area(alpha = 0.7, color = "white") +
  labs(title = "Proporção Acumulada dos Estados por Semana", x = "Semana", y = "Proporção") +
  theme_minimal()
dev.off()

# Exporta CSV final com cluster
output_csv <- file.path(output_dir, paste0("Trajetoria_neonatos_cluster_", timestamp, ".csv"))
fwrite(traj_clean, output_csv)

cat("✅ Análise finalizada com sucesso.\n")
cat("📁 Arquivo com cluster salvo em:", output_csv, "\n")
cat("📊 Gráficos salvos em:", output_dir, "\n")
