# ================================================================
# ESCOLARIZAÇÃO (ESC_MAT) — tabela final no formato "n (p%)"
# - Coluna Total: p% sobre o total geral
# - Colunas de grupo (CU/cluster/TYPE...): p% dentro de cada grupo
# - Saída: data/output/escolarizacao_por_grupo_<timestamp>.xlsx
# ================================================================

# -------- Pacotes --------
pkgs <- c("readxl","dplyr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_xlsx <- "data/input/Banco_de_dados_final_0708_2_Escolarizacao.xlsx"  # ajuste se necessário
sheet_name <- "GERAL"
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts         <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx   <- file.path(out_dir, paste0("escolarizacao_por_grupo_", ts, ".xlsx"))

# (opcional) usar vírgula como separador decimal (ex.: 15,0%)
use_ptBR_decimal <- FALSE
old_locale <- NULL
if (use_ptBR_decimal) {
  old_locale <- try(Sys.getlocale("LC_NUMERIC"), silent = TRUE)
  try(Sys.setlocale("LC_NUMERIC", "pt_BR.UTF-8"), silent = TRUE)  # macOS/Linux
  # Windows: try(Sys.setlocale("LC_NUMERIC", "Portuguese_Brazil.1252"), silent = TRUE)
}
fmt_pct <- function(x) sprintf("%.1f%%", x)

# -------- Leitura --------
df <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df) <- trimws(names(df))

if (!"ESC_MAT" %in% names(df)) stop("❌ Coluna ESC_MAT não encontrada.")

# Detecta coluna de grupo (CU/cluster/TYPE...)
group_candidates <- c("CU","cluster","CLUSTER","Grupo","GRUPO","Group","TYPE","TIPO")
group_col <- group_candidates[group_candidates %in% names(df)]
group_col <- if (length(group_col)) group_col[1] else NULL

# -------- Normalização de valores --------
# Mapeia variantes vazias/NA para "S/INFO"
norm_esc <- function(x){
  v <- toupper(trimws(as.character(x)))
  v[v %in% c("", "NA", "N/A", "SEM INFO", "SEM INFORMAÇÃO", "SEM INFORMACAO",
             "S/ INFORMACAO", "S/ INFORMAÇÃO", "S/INFO.", "S/ INF0")] <- "S/INFO"
  v
}

levels_out <- c("SUPERIOR COMP","SUPERIOR INC",
                "MÉDIO COMP","MÉDIO INC",
                "FUND COMP","FUND INC",
                "S/INFO")

df$ESC_MAT_CLEAN <- norm_esc(df$ESC_MAT)

# -------- Helpers --------
# retorna a coluna formatada "n (p%)" para um vetor (total geral ou subset por grupo)
col_n_pct <- function(vec) {
  fac <- factor(vec, levels = levels_out)
  n   <- as.integer(table(fac, useNA = "no"))
  den <- sum(n)  # denominador = total da coluna
  p   <- if (den == 0) rep(0, length(n)) else 100 * n / den
  sprintf("%d (%s)", n, fmt_pct(p))
}

# -------- Montagem da tabela --------
res <- data.frame(Categoria = levels_out, check.names = FALSE)

# Coluna Total (n (%)) sobre o total geral
res[["Total (n (%))"]] <- col_n_pct(df$ESC_MAT_CLEAN)

# Colunas por grupo (se existir)
if (!is.null(group_col)) {
  grupos <- unique(df[[group_col]])
  for (g in grupos) {
    sub_vec <- df$ESC_MAT_CLEAN[df[[group_col]] == g]
    res[[as.character(g)]] <- col_n_pct(sub_vec)
  }
}

# -------- Exporta --------
writexl::write_xlsx(list("Escolarizacao_n(%)" = res), path = out_xlsx)

if (use_ptBR_decimal && !inherits(old_locale, "try-error")) {
  try(Sys.setlocale("LC_NUMERIC", old_locale), silent = TRUE)
}

cat("✅ Arquivo gerado em:\n", out_xlsx, "\n")
