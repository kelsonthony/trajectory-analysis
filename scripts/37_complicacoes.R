# ================================================================
# Complicações: n (%) e média (DP) [dias] por variável
# Arquivo: data/input/Banco_de_dados_final_0708_Complicacoes.xlsx
# Aba:     CondicoesAssociadas
# Variáveis alvo:
# HEMOR_PULM, HEMOR_IC (GRAU), HIPERT_PULM, PNEU_CONG, PNEU_ADQ,
# ENT_NEC, INFEC_PRES, INFEC_TAR, SEPSE, ATB, T_ATB, TIPO_ATB,
# SÍFILIS, CUV, NPT, DIETA NA ALTA
#
# Regras:
# - n (%) = proporção de linhas onde a condição "ocorreu" (lógicas abaixo).
# - Média (DP) [dias]:
#    * Se a variável tem coluna de dias pareada (aqui só ATB -> T_ATB):
#        => média/DP calculada ENTRE ocorridos com dias > 0.
#    * Se NÃO tem coluna de dias e USE_PROXY_FOR_NO_DAYS == TRUE:
#        => proxy = T_ATB ENTRE ocorridos (inclui zeros).
#    * Caso contrário: "—".
# ================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tibble)
  library(tidyr);  library(writexl); library(purrr)
})

# -------- Parâmetros --------
in_file <- "data/input/Banco_de_dados_final_0708_Complicacoes.xlsx"
sheet   <- "CondicoesAssociadas"
out_dir <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Use proxy (T_ATB) para variáveis SEM dias próprios?
USE_PROXY_FOR_NO_DAYS <- TRUE
PROXY_COL <- "T_ATB"

NA_STRINGS <- c("", "NA", "N/A", "S/INFO", "SEM/INFO", "SEM INFO", "SEM-INFORMACAO")

# -------- Leitura robusta --------
df <- readxl::read_excel(in_file, sheet = sheet, na = NA_STRINGS)
# Remove colunas totalmente vazias
all_na_cols <- vapply(df, function(x) all(is.na(x)), logical(1))
if (any(all_na_cols)) df <- df[, !all_na_cols, drop = FALSE]
# Normaliza nomes (trim)
names(df) <- trimws(names(df))
# Converte tudo para texto
df <- df %>% mutate(across(everything(), ~ as.character(.)))

N_total <- nrow(df)

# -------- Utilitários --------
is_yes <- function(x, N = N_total) {
  if (is.null(x)) return(rep(FALSE, N))
  v <- tolower(trimws(as.character(x)))
  v %in% c("sim","s","yes","y","1","true","verdadeiro")
}
is_present_not_nao <- function(x, N = N_total) {
  if (is.null(x)) return(rep(FALSE, N))
  v <- trimws(as.character(x))
  ok <- !is.na(v) & v != ""
  ok & !(tolower(v) %in% c("nao","não","no","none"))
}
to_days_num <- function(x, N = N_total) {
  if (is.null(x)) return(rep(NA_real_, N))
  v <- as.character(x)
  v <- trimws(v)
  v <- gsub(",", ".", v)
  v[!grepl("^[+-]?[0-9]*\\.?[0-9]+$", v)] <- NA
  suppressWarnings(as.numeric(v))
}
fmt_mean_sd <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return("—")
  m <- mean(x); s <- ifelse(length(x) > 1, stats::sd(x), 0)
  paste0(format(round(m, digits), nsmall = digits), " (",
         format(round(s, digits), nsmall = digits), ")")
}
fmt_npct <- function(n, N) sprintf("%d (%.1f%%)", n, ifelse(N > 0, 100*n/N, 0))

# -------- Mapeamento das variáveis --------
targets <- tibble::tribble(
  ~variavel,          ~flag_type,          ~days_col,
  "HEMOR_PULM",       "yes",               NA_character_,
  "HEMOR_IC (GRAU)",  "present_not_nao",   NA_character_,
  "HIPERT_PULM",      "yes",               NA_character_,
  "PNEU_CONG",        "yes",               NA_character_,
  "PNEU_ADQ",         "yes",               NA_character_,
  "ENT_NEC",          "yes",               NA_character_,
  "INFEC_PRES",       "yes",               NA_character_,
  "INFEC_TAR",        "yes",               NA_character_,
  "SEPSE",            "yes",               NA_character_,
  "ATB",              "yes",               "T_ATB",       # tem dias
  "SÍFILIS",          "yes",               NA_character_,
  "CUV",              "yes",               NA_character_,
  "NPT",              "yes",               NA_character_,
  "DIETA NA ALTA",    "present_not_nao",   NA_character_
)

# Mantém apenas as variáveis que existem na planilha
targets$exists <- targets$variavel %in% names(df)
if (any(!targets$exists)) {
  message("⚠️ Colunas ausentes: ", paste(targets$variavel[!targets$exists], collapse = ", "))
}
targets <- targets %>% filter(exists) %>% select(-exists)

# Proxy numérico (T_ATB) se existir
proxy_vec <- if (PROXY_COL %in% names(df)) to_days_num(df[[PROXY_COL]]) else rep(NA_real_, N_total)

# -------- Cálculo --------
rows <- vector("list", nrow(targets))

for (i in seq_len(nrow(targets))) {
  var  <- targets$variavel[i]
  ftyp <- targets$flag_type[i]
  dcol <- targets$days_col[i]

  occurred <- switch(
    ftyp,
    "yes"             = is_yes(df[[var]]),
    "present_not_nao" = is_present_not_nao(df[[var]]),
    rep(FALSE, N_total)
  )

  n_occ <- sum(occurred, na.rm = TRUE)
  npct  <- fmt_npct(n_occ, N_total)

  mean_sd <- "—"
  if (!is.na(dcol) && dcol %in% names(df)) {
    # variável com dias próprios (ATB -> T_ATB)
    v <- to_days_num(df[[dcol]])
    mean_sd <- fmt_mean_sd(v[occurred & !is.na(v) & v > 0])
  } else if (USE_PROXY_FOR_NO_DAYS && all(!is.na(proxy_vec))) {
    # usar proxy T_ATB entre os que tiveram a variável
    # (inclui zeros para mostrar média mesmo se não houve dias de ATB)
    mean_sd <- fmt_mean_sd(ifelse(occurred, ifelse(is.na(proxy_vec), 0, pmax(proxy_vec, 0)), NA))
  }

  rows[[i]] <- tibble(
    Variavel = var,
    `n (%)` = npct,
    `Média (DP) [dias]` = mean_sd
  )
}

resultado <- bind_rows(rows) %>%
  mutate(ord = dplyr::case_when(
    Variavel == "HEMOR_PULM"       ~ 1,
    Variavel == "HEMOR_IC (GRAU)"  ~ 2,
    Variavel == "HIPERT_PULM"      ~ 3,
    Variavel == "PNEU_CONG"        ~ 4,
    Variavel == "PNEU_ADQ"         ~ 5,
    Variavel == "ENT_NEC"          ~ 6,
    Variavel == "INFEC_PRES"       ~ 7,
    Variavel == "INFEC_TAR"        ~ 8,
    Variavel == "SEPSE"            ~ 9,
    Variavel == "ATB"              ~ 10,
    Variavel == "SÍFILIS"          ~ 11,
    Variavel == "CUV"              ~ 12,
    Variavel == "NPT"              ~ 13,
    Variavel == "DIETA NA ALTA"    ~ 14,
    TRUE ~ 99
  )) %>%
  arrange(ord) %>% select(-ord)

print(resultado, n = Inf)

# -------- Salvar --------
out_xlsx <- file.path(out_dir, "complicacoes_resumo.xlsx")
writexl::write_xlsx(list("Resumo complicacoes" = resultado), out_xlsx)
message("✅ Arquivo salvo em: ", out_xlsx)

# ---- DICA ----
# Se NÃO quiser proxy (e prefere "—"), altere:
# USE_PROXY_FOR_NO_DAYS <- FALSE
