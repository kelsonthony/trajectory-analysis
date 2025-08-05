# ==========================================
# 07 - CLUSTERING BASEADO NO TEMPO EM CADA ESTADO (CORRIGIDO)
# ==========================================

# Pacotes
library(dplyr)
library(data.table)
library(cluster)
library(ggplot2)
library(tidyr)

# Criar diretório de saída
dir_out <- "data/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Detectar o CSV de trajetória mais recente
csv_mais_recente <- list.files(
  dir_out,
  pattern = "^Trajetoria_neonatos.*\\.csv$",
  full.names = TRUE
) %>%
  sort(decreasing = TRUE) %>%
  .[1]

if (is.na(csv_mais_recente)) {
  stop("❌ Nenhum arquivo de trajetória encontrado em 'data/output'.")
}

message("✅ Arquivo carregado: ", csv_mais_recente)

# Carregar a matriz de trajetória
traj <- fread(csv_mais_recente)

# Detectar coluna identificadora
col_id <- grep("^(id|n_neonato)$", colnames(traj), value = TRUE)

if (length(col_id) == 0) {
  stop("❌ Nenhuma coluna identificadora encontrada ('id' ou 'n_neonato').")
} else {
  message("🔎 Coluna identificadora usada: ", col_id)
}

# Remover coluna 'cluster' anterior se existir
if ("cluster" %in% colnames(traj)) {
  traj <- traj %>% select(-cluster)
}

# Converter para formato longo
traj_long <- traj %>%
  pivot_longer(cols = -all_of(col_id), names_to = "semana", values_to = "estado") %>%
  mutate(semana = as.integer(semana))

# Contar tempo total em cada estado por paciente
tempo_por_estado <- traj_long %>%
  group_by(across(all_of(col_id)), estado) %>%
  summarise(tempo = n(), .groups = "drop") %>%
  pivot_wider(names_from = estado, values_from = tempo, values_fill = 0)

# Criar matriz para cluster (sem o identificador)
matriz_cluster <- tempo_por_estado %>% select(-all_of(col_id))

# Clustering hierárquico
dist_tempo <- dist(matriz_cluster)
agrupamento <- agnes(dist_tempo, method = "ward")
grupo_tempo <- cutree(agrupamento, k = 3)

# Adicionar coluna de grupo
tempo_por_estado$grupo <- as.factor(grupo_tempo)

# Estatísticas por grupo
summary_por_grupo <- tempo_por_estado %>%
  group_by(grupo) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

print(summary_por_grupo)

# Preparar gráfico
summary_por_grupo_long <- summary_por_grupo %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "estado",
    values_to = "tempo_medio"
  )

plot_file <- file.path(
  dir_out,
  paste0("grafico_tempo_medio_estado_grupo_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
)

grafico <- ggplot(summary_por_grupo_long, aes(x = estado, y = tempo_medio, fill = grupo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tempo médio em cada estado por grupo de similaridade temporal",
    x = "Estado", y = "Tempo médio (semanas)"
  ) +
  theme_minimal()

ggsave(plot_file, plot = grafico, width = 10, height = 6, dpi = 300)

# Salvar resultado com grupos
output_csv <- file.path(
  dir_out,
  paste0("agrupamento_por_tempo_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
)

# Renomear coluna identificadora para 'id' ao exportar (padronização)
tempo_por_estado_export <- tempo_por_estado
colnames(tempo_por_estado_export)[1] <- "id"

fwrite(tempo_por_estado_export, output_csv)

# Mensagem final
message("✅ Script 07 finalizado com sucesso.")
message("📊 Arquivo salvo: ", output_csv)
message("📈 Gráfico salvo: ", plot_file)
