# ==========================================
# 06 - CLUSTERING BASEADO NO TEMPO EM CADA ESTADO
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

# Caminho do CSV
csv_path <- file.path(dir_out, list.files(dir_out, pattern = "^Trajetoria_neonatos.*\\.csv$", full.names = FALSE)[1])
csv_path <- file.path(dir_out, csv_path)

# Carrega a matriz de trajetória
traj <- fread(csv_path)

# Derreter o formato largo para longo
traj_long <- traj %>%
  pivot_longer(-n_neonato, names_to = "semana", values_to = "estado") %>%
  mutate(semana = as.integer(semana))

# Contar tempo total de cada paciente em cada estado
tempo_por_estado <- traj_long %>%
  group_by(n_neonato, estado) %>%
  summarise(tempo = n(), .groups = "drop") %>%
  pivot_wider(names_from = estado, values_from = tempo, values_fill = 0)

# Remover coluna ID para clustering
matriz_cluster <- tempo_por_estado %>% select(-n_neonato)

# Clustering hierárquico
dist_tempo <- dist(matriz_cluster)
agrupamento <- agnes(dist_tempo, method = "ward")
grupo_tempo <- cutree(agrupamento, k = 3)

# Anexar grupo ao dataframe
tempo_por_estado$grupo <- as.factor(grupo_tempo)

# 📊 Estatísticas por grupo
summary_por_grupo <- tempo_por_estado %>%
  group_by(grupo) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

print(summary_por_grupo)

# 📈 Gráfico de barras médias por grupo
summary_por_grupo_long <- summary_por_grupo %>%
  pivot_longer(-grupo, names_to = "estado", values_to = "tempo_medio")

plot_file <- file.path(dir_out, paste0("grafico_tempo_medio_estado_grupo_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"))

grafico <- ggplot(summary_por_grupo_long, aes(x = estado, y = tempo_medio, fill = grupo)) +
  geom_col(position = "dodge") +
  labs(title = "Tempo médio em cada estado por grupo de similaridade temporal",
       x = "Estado", y = "Tempo médio (semanas)") +
  theme_minimal()

ggsave(plot_file, plot = grafico, width = 10, height = 6, dpi = 300)

# ✅ Salvar resultado
output_csv <- file.path(dir_out, paste0("agrupamento_por_tempo_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))
fwrite(tempo_por_estado, output_csv)

message("✅ Script 06 finalizado com sucesso.")
message("📊 Arquivo salvo: ", output_csv)
message("📈 Gráfico salvo: ", plot_file)
