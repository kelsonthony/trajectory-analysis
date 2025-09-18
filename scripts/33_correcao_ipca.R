# 33_correcao_ipca.R
# ------------------------------------------------------------
# Corrige custos para 2024 (IPCA) preservando layout do Excel e
# formata as células numéricas como moeda BR (ex.: R$ 27.462).
# ------------------------------------------------------------

required_pkgs <- c("readxl","writexl","stringr","dplyr","purrr")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# Caminhos
INPUT_XLSX  <- "data/input/planilhas_custos_ipca.xlsx"
OUTPUT_XLSX <- "data/output/planilhas_custos_ipca_corrigido.xlsx"

# Fatores IPCA para levar a preços de 2024
FATORES <- c(
  "2018"=1.39417140, "2019"=1.34996230, "2020"=1.29416950,
  "2021"=1.16867180, "2022"=1.10355660, "2023"=1.05418350,
  "2024"=1.00000000
)

# Se não achar ano por linha/coluna, usa:
DEFAULT_BASE_YEAR <- "2021"  # ajuste se quiser

# -------- utils --------

`%||%` <- function(a, b) if (is.null(a) || (length(a)==1 && is.na(a))) b else a

# Extrai primeiro ano (2018-2024) encontrado num vetor de strings
extrair_ano <- function(x) {
  if (is.null(x)) return(NA_character_)
  s <- suppressWarnings(as.character(x))
  m <- stringr::str_extract(s, "(?<!\\d)(2018|2019|2020|2021|2022|2023|2024)(?!\\d)")
  m[!is.na(m)][1] %||% NA_character_
}

# Converte "1.234,56", "12.345", "R$ 1.234" -> numeric (ponto decimal)
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
  # NA fica vazio para não sujar a planilha
  out <- ifelse(is.na(x), "", paste0("R$ ", formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")))
  # Remover "R$ NA" caso algo escape
  out <- gsub("^R\\$ NA$", "", out)
  out
}

# Aplica correção célula a célula (numéricas) e FORMATA moeda BR
corrigir_tabela <- function(df, header_year_guess = NA_character_) {
  nms <- names(df)

  # Ano por coluna (busca nos nomes)
  ano_col <- vapply(nms, extrair_ano, FUN.VALUE = character(1))
  if (all(is.na(ano_col)) && !is.na(header_year_guess)) {
    ano_col[] <- header_year_guess
  }

  # Ano por linha (busca em todo o conteúdo da linha)
  linhas_ano <- apply(df, 1, function(row) extrair_ano(row))

  out <- df
  for (j in seq_along(nms)) {
    col <- df[[j]]
    # tenta interpretar como número/custo
    num_col <- to_num(col)

    for (i in seq_len(nrow(df))) {
      val <- num_col[i]
      if (is.na(val)) next  # mantém como está se não for número/custo

      # prioridade 1: ano na LINHA; 2: ano na COLUNA; 3: default
      ano_here <- linhas_ano[i]
      if (is.na(ano_here)) ano_here <- ano_col[j]
      if (is.na(ano_here)) ano_here <- DEFAULT_BASE_YEAR

      if (!is.na(ano_here) && ano_here %in% names(FATORES)) {
        fator <- as.numeric(FATORES[[ano_here]])
        val_corr <- val * fator
        # formata como moeda BR e grava como TEXTO (mantém layout visual)
        out[[j]][i] <- fmt_brl(val_corr)
      } else {
        # se não encontrou ano válido, mantém original
        out[[j]][i] <- col[i]
      }
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

  # “pista” de ano vinda de nomes de coluna e primeira linha
  header_hint <- extrair_ano(c(names(df_raw), as.character(df_raw[1, , drop = TRUE])))

  df_corr <- tryCatch(
    corrigir_tabela(df_raw, header_year_guess = header_hint),
    error = function(e) { message("[Erro] ", sh, ": ", e$message); df_raw }
  )

  # preserva nomes/ordem
  df_corr <- df_corr[, names(df_raw), drop = FALSE]
  names(df_corr) <- names(df_raw)

  saida[[sh]] <- df_corr
}

writexl::write_xlsx(saida, path = OUTPUT_XLSX)
message("Arquivo gerado: ", OUTPUT_XLSX)
