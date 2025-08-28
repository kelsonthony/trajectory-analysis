# ================================================================
# Identificar células vazias (NA, "", espaços, "NA"/"N/A", "-")
# e gerar:
#   1) lista de células vazias (linha, coluna)
#   2) resumo por coluna (n e % vazios)
#   3) resumo por linha (quantos campos vazios por registro)
# Saída: data/output/celulas_vazias.xlsx
# ================================================================

# Pacotes
pkgs <- c("readxl","readr","dplyr","tidyr","writexl","janitor","stringr","tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config de entrada --------
use_excel <- TRUE  # <- mude para FALSE se quiser ler do CSV

out_dir <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (use_excel) {
  input_file <- "data/input/dados_regressao.xlsx"   # ajuste o nome exato (acentos!)
  sheet_name <- "Planilha1"
  df <- readxl::read_excel(input_file, sheet = sheet_name) %>% janitor::clean_names()
} else {
  input_csv <- "data/output/dados_modelo_20250827_1953.csv"
  df <- readr::read_csv(input_csv, show_col_types = FALSE) %>% janitor::clean_names()
}

# -------- Funções utilitárias --------
as_chr_norm <- function(x) {
  out <- if (is.character(x)) x else as.character(x)
  stringr::str_trim(out)
}

is_empty_val <- function(v) {
  v0 <- v
  is_na <- is.na(v0)
  v1 <- tolower(trimws(replace(v0, is.na(v0), "")))
  is_blank <- v1 == "" | v1 %in% c("na","n/a","-")
  is_na | is_blank
}

# -------- Preparar: forçar tudo a texto --------
df_chr <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as_chr_norm))

# -------- Long + marcação de vazios --------
long <- df_chr %>%
  dplyr::mutate(linha = dplyr::row_number()) %>%
  tidyr::pivot_longer(
    cols = -linha,
    names_to = "coluna",
    values_to = "valor",
    values_transform = list(valor = as.character),
    values_ptypes    = list(valor = character())
  ) %>%
  dplyr::mutate(vazio = is_empty_val(valor))

# -------- Saídas --------
# 1) Lista de células vazias
celulas_vazias <- long %>%
  dplyr::filter(vazio) %>%
  dplyr::select(linha, coluna, valor)

# 2) Resumo por coluna
resumo_por_coluna <- long %>%
  dplyr::group_by(coluna) %>%
  dplyr::summarise(
    n_total  = dplyr::n(),
    n_vazio  = sum(vazio, na.rm = TRUE),
    pct_vazio = round(100 * n_vazio / n_total, 1),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(pct_vazio), coluna)

# 3) Resumo por linha
resumo_por_linha <- long %>%
  dplyr::group_by(linha) %>%
  dplyr::summarise(
    n_campos  = dplyr::n(),
    n_vazios  = sum(vazio, na.rm = TRUE),
    pct_vazios = round(100 * n_vazios / n_campos, 1),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(n_vazios), linha)

# -------- Exportar --------
out_xlsx <- file.path(out_dir, "celulas_vazias.xlsx")
writexl::write_xlsx(
  list(
    "Celulas_vazias"   = celulas_vazias,
    "Resumo_por_coluna"= resumo_por_coluna,
    "Resumo_por_linha" = resumo_por_linha
  ),
  out_xlsx
)

message("✅ Relatório gerado em: ", out_xlsx)
print(head(celulas_vazias, 15))
print(head(resumo_por_coluna, 15))
print(head(resumo_por_linha, 15))
