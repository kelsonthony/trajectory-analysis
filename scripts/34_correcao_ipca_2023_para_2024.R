# 34_correcao_ipca_2023_para_2024.R
# ------------------------------------------------------------
# Corrige uma planilha de custos (ano-base 2023) para 2024
# usando um ÚNICO fator IPCA acumulado: 1,07326210.
# - Mantém abas, nomes e ordem das colunas
# - Aplica em todas as células que forem numéricas
# - Formata como moeda BR: "R$ 1.234.567"
# ------------------------------------------------------------

required_pkgs <- c("readxl","writexl","stringr")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# Caminhos (ajuste se necessário)
INPUT_XLSX  <- "data/input/dados_de_custos_2023.xlsx"
OUTPUT_XLSX <- "data/output/dados_de_custos_2023_corrigido_2024.xlsx"

# Fator único IPCA (2023 -> 2024)
FATOR_IPCA <- 1.07326210

# Se TRUE, grava como texto "R$ 1.234.567". Se FALSE, deixa número puro.
FORMAT_AS_CURRENCY <- TRUE

# -------- utils --------

# Converte strings "R$ 1.234,56" / "12.345" / "1.234" em numeric (ponto decimal)
to_num <- function(v) {
  if (is.numeric(v)) return(v)
  s <- as.character(v)
  # se não tem dígitos, vira NA
  no_digits <- !grepl("\\d", s)
  s[no_digits] <- NA_character_
  # remove "R$ ", espaços, pontos de milhar e troca vírgula decimal por ponto
  s <- gsub("R\\$\\s*", "", s)
  s <- gsub("\\.", "", s)   # milhar
  s <- gsub(",", ".", s)    # decimal
  as.numeric(suppressWarnings(s))
}

# Formata numeric -> "R$ 1.234.567" (sem casas decimais)
fmt_brl <- function(x) {
  out <- ifelse(is.na(x), "", paste0("R$ ", formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")))
  out
}

# Corrige todas as células numéricas de um data.frame-like
corrigir_planilha <- function(df, fator, format_currency = TRUE) {
  out <- df
  nms <- names(df)
  for (j in seq_along(nms)) {
    col <- df[[j]]
    num_col <- to_num(col)
    # só corrige posições que realmente viraram número
    idx <- !is.na(num_col)
    if (!any(idx)) next
    corrigidos <- num_col[idx] * fator
    if (format_currency) {
      out[[j]][idx] <- fmt_brl(corrigidos)
    } else {
      out[[j]][idx] <- corrigidos
    }
  }
  out
}

# -------- execução --------

if (!file.exists(INPUT_XLSX)) stop("Arquivo de entrada não encontrado: ", INPUT_XLSX)
abas <- readxl::excel_sheets(INPUT_XLSX)

saida <- list()
dir.create(dirname(OUTPUT_XLSX), showWarnings = FALSE, recursive = TRUE)

for (sh in abas) {
  df_raw <- suppressMessages(readxl::read_excel(INPUT_XLSX, sheet = sh, col_names = TRUE))
  if (nrow(df_raw) == 0 || ncol(df_raw) == 0) {
    message(sprintf("[Aviso] Aba '%s' vazia, mantida.", sh))
    saida[[sh]] <- df_raw
    next
  }
  df_corr <- tryCatch(
    corrigir_planilha(df_raw, fator = FATOR_IPCA, format_currency = FORMAT_AS_CURRENCY),
    error = function(e) { message("[Erro] ", sh, ": ", e$message); df_raw }
  )
  # preserva nomes e ordem
  df_corr <- df_corr[, names(df_raw), drop = FALSE]
  names(df_corr) <- names(df_raw)
  saida[[sh]] <- df_corr
}

writexl::write_xlsx(saida, path = OUTPUT_XLSX)
message("Arquivo gerado: ", OUTPUT_XLSX)
