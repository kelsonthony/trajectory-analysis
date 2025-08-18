# ================================================================
# RESUMO POR CLUSTER: IG (WW+DD), APGAR 1/5, PESO, RCP
# Lê a aba "GERAL" e gera uma tabela final (CSV + XLSX)
# ================================================================

# --------- Pacotes ---------
packages_needed <- c("readxl", "dplyr", "stringr", "writexl", "readr", "tidyr")
to_install <- setdiff(packages_needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages_needed, library, character.only = TRUE))

# --------- Config ---------
input_xlsx  <- "data/input/Banco_de_dados_final_0708_2.xlsx"  # ajuste se necessário
sheet_name  <- "GERAL"
output_dir  <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
timestamp   <- format(Sys.time(), "%Y%m%d_%H%M")
out_csv     <- file.path(output_dir, paste0("resumo_clusters_IG_Apgar_Peso_RCP_", timestamp, ".csv"))
out_xlsx    <- file.path(output_dir, paste0("resumo_clusters_IG_Apgar_Peso_RCP_", timestamp, ".xlsx"))

# --------- Helpers ---------
# Converte "WW+DD" -> semanas decimais
ig_to_weeks <- function(x) {
  if (is.na(x)) return(NA_real_)
  x <- gsub("\\s+", "", as.character(x))     # remove espaços
  if (x == "") return(NA_real_)
  parts <- strsplit(x, "\\+", fixed = FALSE)[[1]]
  w <- suppressWarnings(as.numeric(parts[1]))
  d <- if (length(parts) > 1) suppressWarnings(as.numeric(parts[2])) else 0
  if (is.na(w)) return(NA_real_)
  if (is.na(d)) d <- 0
  w + d/7
}

# Converte semanas decimais -> "WW+DD"
weeks_to_fmt <- function(w) {
  if (is.na(w)) return(NA_character_)
  ww <- floor(w)
  dd <- round((w - ww) * 7)
  if (dd == 7) { ww <- ww + 1; dd <- 0 }
  sprintf("%d+%d", ww, dd)
}

# --------- Leitura ---------
if (!file.exists(input_xlsx)) stop(paste0("❌ Arquivo não encontrado: ", input_xlsx))
df <- readxl::read_excel(input_xlsx, sheet = sheet_name)

# --------- Preparação ---------
# Normaliza nomes de colunas para facilitar detecção
names(df) <- trimws(names(df))

# Garante coluna de CLUSTER
if (!"cluster" %in% names(df)) {
  stop("❌ Não encontrei a coluna 'cluster'. Certifique-se de que o arquivo possui essa coluna.")
}

# IG -> semanas decimais
if (!"IG" %in% names(df)) stop("❌ Não encontrei a coluna 'IG'.")
df$IG_weeks <- vapply(df$IG, ig_to_weeks, numeric(1))

# Apgar 1/5 -> numéricos
apgar1_col <- "APGAR 1"
apgar5_col <- "APGAR 5"
if (!(apgar1_col %in% names(df))) stop("❌ Não encontrei a coluna 'APGAR 1'.")
if (!(apgar5_col %in% names(df))) stop("❌ Não encontrei a coluna 'APGAR 5'.")
df[[apgar1_col]] <- suppressWarnings(as.numeric(df[[apgar1_col]]))
df[[apgar5_col]] <- suppressWarnings(as.numeric(df[[apgar5_col]]))

# Peso -> numérico (aceita nomes alternativos)
peso_candidates <- c("P_NAS", "PESO", "PESO_NASC", "PESO_NASCIMENTO")
peso_name <- peso_candidates[peso_candidates %in% names(df)]
if (length(peso_name) == 0) {
  stop("❌ Não encontrei a coluna de peso (tente: 'P_NAS', 'PESO', 'PESO_NASC', 'PESO_NASCIMENTO').")
}
peso_name <- peso_name[1]
df$P_NAS <- suppressWarnings(as.numeric(df[[peso_name]]))

# RCP -> normaliza para maiúsculas e sem espaços; conta "SIM"
rcp_col <- if ("RCP" %in% names(df)) "RCP" else NA_character_
if (!is.na(rcp_col)) {
  rcp_norm <- toupper(trimws(as.character(df[[rcp_col]])))
  # opcional: tratar variações "SIM " / " Sim" etc já cobertas por toupper/trimws
  df$RCP_norm <- rcp_norm
} else {
  df$RCP_norm <- NA_character_
}

# Ordena cluster naturalmente (type 1, type 2, ...)
df$cluster <- as.character(df$cluster)
df$cluster <- factor(df$cluster,
                     levels = sort(unique(df$cluster),
                                   method = "radix"))

# --------- Agregação por cluster ---------
summary_tbl <- df |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(
    n               = dplyr::n(),
    IG_mean_weeks   = mean(IG_weeks, na.rm = TRUE),
    IG_sd_weeks     = sd(IG_weeks, na.rm = TRUE),
    Apgar1_mean     = mean(.data[[apgar1_col]], na.rm = TRUE),
    Apgar1_sd       = sd(.data[[apgar1_col]], na.rm = TRUE),
    Apgar5_mean     = mean(.data[[apgar5_col]], na.rm = TRUE),
    Apgar5_sd       = sd(.data[[apgar5_col]], na.rm = TRUE),
    Peso_mean       = mean(P_NAS, na.rm = TRUE),
    Peso_sd         = sd(P_NAS, na.rm = TRUE),
    RCP_SIM         = sum(RCP_norm == "SIM", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    `IG_mean_fmt(WW+DD)` = vapply(IG_mean_weeks, weeks_to_fmt, character(1)),
    RCP_SIM_pct = round(100 * RCP_SIM / n, 2)
  )

# --------- Arredondamentos e ordenação ---------
summary_tbl <- summary_tbl |>
  dplyr::mutate(
    IG_mean_weeks = round(IG_mean_weeks, 2),
    IG_sd_weeks   = round(IG_sd_weeks, 2),
    Apgar1_mean   = round(Apgar1_mean, 2),
    Apgar1_sd     = round(Apgar1_sd, 2),
    Apgar5_mean   = round(Apgar5_mean, 2),
    Apgar5_sd     = round(Apgar5_sd, 2),
    Peso_mean     = round(Peso_mean, 1),
    Peso_sd       = round(Peso_sd, 1)
  ) |>
  dplyr::select(
    cluster, n,
    IG_mean_weeks, `IG_mean_fmt(WW+DD)`, IG_sd_weeks,
    Apgar1_mean, Apgar1_sd, Apgar5_mean, Apgar5_sd,
    Peso_mean, Peso_sd,
    RCP_SIM, RCP_SIM_pct
  ) |>
  dplyr::arrange(cluster)

# --------- Cabeçalhos em português ---------
summary_tbl_pt <- summary_tbl |>
  dplyr::rename(
    `Cluster`                 = cluster,
    `N`                       = n,
    `IG média (semanas)`      = IG_mean_weeks,
    `IG média (WW+DD)`        = `IG_mean_fmt(WW+DD)`,
    `IG desvio-padrão`        = IG_sd_weeks,
    `Apgar 1 - média`         = Apgar1_mean,
    `Apgar 1 - desvio-padrão` = Apgar1_sd,
    `Apgar 5 - média`         = Apgar5_mean,
    `Apgar 5 - desvio-padrão` = Apgar5_sd,
    `Peso médio (g)`          = Peso_mean,
    `Peso desvio-padrão`      = Peso_sd,
    `Reanimação (SIM)`        = RCP_SIM,
    `% Reanimação (SIM)`      = RCP_SIM_pct
  )

# --------- Exportações ---------
# CSV (com cabeçalhos em português)
readr::write_csv(summary_tbl_pt, out_csv)

# XLSX
writexl::write_xlsx(summary_tbl_pt, path = out_xlsx)

cat("✅ Resumo por cluster gerado com sucesso.\n")
cat("📁 Tabelas:\n -", out_csv, "\n -", out_xlsx, "\n")
