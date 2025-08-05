# ==========================================
# Curva de Kaplan-Meier - Tempo de Internação Neonatal (Dias e Semanas)
# ==========================================

# Instalar pacotes se necessário
if (!require(readxl)) install.packages("readxl")
if (!require(survival)) install.packages("survival")
if (!require(survminer)) install.packages("survminer")

# Carregar pacotes
library(readxl)
library(survival)
library(survminer)

# Diretórios
dir_input <- "data/input"
dir_output <- "data/output"

if (!dir.exists(dir_input)) dir.create(dir_input, recursive = TRUE)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# Caminho do arquivo Excel
arquivo <- file.path(dir_input, "AdmAltDatas.xlsx")

# Ler dados da aba "AdmissaoAlta"
dados <- read_excel(arquivo, sheet = "AdmissaoAlta")

# ✅ Conversão robusta para Date (corrige o erro POSIXt x Date)
dados$data_adm <- as.Date(dados$data_adm)
dados$data_alta <- as.Date(dados$data_alta)
dados$data_óbito <- as.Date(dados$data_óbito)

# Data de saída: óbito se houver, senão alta
dados$data_saida <- ifelse(is.na(dados$data_óbito), dados$data_alta, dados$data_óbito)
dados$data_saida <- as.Date(dados$data_saida)

# Cálculo dos dias de internação
dados$dias_internacao <- as.numeric(dados$data_saida - dados$data_adm)

# Evento: 1 = óbito, 0 = alta
dados$evento <- ifelse(is.na(dados$data_óbito), 0, 1)

# Filtrar apenas casos válidos
dados_validos <- subset(dados, dias_internacao >= 0)

# ===============================
# Gráfico em Dias
# ===============================
fit_dias <- survfit(Surv(dias_internacao, evento) ~ 1, data = dados_validos)

grafico_km_dias <- ggsurvplot(
  fit_dias,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Dias de internação",
  ylab = "Probabilidade de permanência hospitalar",
  title = "Curva de Kaplan-Meier - Tempo de internação (dias)",
  palette = "Dark2"
)

# Caminho do PNG - dias
file_name_dias <- paste0("curva_kaplan_meier_dias_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
plot_path_dias <- file.path(dir_output, file_name_dias)

ggsave(
  filename = plot_path_dias,
  plot = grafico_km_dias$plot,
  width = 10,
  height = 6,
  dpi = 300
)

message("✅ Gráfico em dias salvo em: ", plot_path_dias)

# ===============================
# Gráfico em Semanas
# ===============================
dados_validos$semanas_internacao <- dados_validos$dias_internacao / 7

fit_semanas <- survfit(Surv(semanas_internacao, evento) ~ 1, data = dados_validos)

grafico_km_semanas <- ggsurvplot(
  fit_semanas,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Semanas de internação",
  ylab = "Probabilidade de permanência hospitalar",
  title = "Curva de Kaplan-Meier - Tempo de internação (semanas)",
  palette = "Dark2"
)

# Caminho do PNG - semanas
file_name_semanas <- paste0("curva_kaplan_meier_semanas_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
plot_path_semanas <- file.path(dir_output, file_name_semanas)

ggsave(
  filename = plot_path_semanas,
  plot = grafico_km_semanas$plot,
  width = 10,
  height = 6,
  dpi = 300
)

message("✅ Gráfico em semanas salvo em: ", plot_path_semanas)
