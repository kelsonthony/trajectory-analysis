# ================================================================
# Suportes ventilatórios: n (%), média (DP) [dias] + TOTAL (suportes)
# Arquivo: data/input/Banco_de_dados_final_0708_suporte_ventilatorio.xlsx
# Aba:     CondClinicas
# Colunas mínimas esperadas:
#   SUP.O2 VM DIAS VM VNI DIAS VNI CNAF DIAS CNAF CPAP DIAS CPAP CNO2 HOOD
# Observação:
#  - Para SUP.O2, CNO2 e HOOD (sem colunas de dias), usamos como proxy a
#    soma (DIAS VM + DIAS VNI + DIAS CNAF + DIAS CPAP) entre os usuários
#    do suporte. Aqui **incluímos zeros** para evitar “—”.
#  - Para VM, VNI, CNAF, CPAP (com dias), média/DP **apenas entre dias > 0**.
# ================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tibble)
  library(tidyr);  library(writexl); library(purrr)
})

# ---------- Entrada ----------
in_file  <- "data/input/Banco_de_dados_final_0708_suporte_ventilatorio.xlsx"
sheet    <- "CondClinicas"
out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

NA_STRINGS <- c("", "NA", "N/A", "S/INFO", "SEM/INFO", "SEM INFO", "SEM-INFORMACAO")

# 1) Ler 1 linha para obter número de colunas
tmp <- readxl::read_excel(in_file, sheet = sheet, na = NA_STRINGS, n_max = 1, guess_max = 1)
ncols <- ncol(tmp)

# 2) Ler tudo como TEXTO (evita warnings de tipo); NA_STRINGS viram NA
df <- readxl::read_excel(
  in_file,
  sheet = sheet,
  na = NA_STRINGS,
  col_types = rep("text", ncols)
)

N_total <- nrow(df)

# ---------- Utilitários ----------
is_yes_vec <- function(x, N = N_total) {
  if (is.null(x)) return(rep(FALSE, N))
  v <- tolower(trimws(as.character(x)))
  v %in% c("sim","s","yes","y","1","true","verdadeiro")
}

to_days_num <- function(x, N = N_total) {
  if (is.null(x)) return(rep(NA_real_, N))
  v <- as.character(x)
  v <- trimws(v)
  v <- gsub(",", ".", v)                                 # vírgula -> ponto
  v[!grepl("^[+-]?[0-9]*\\.?[0-9]+$", v)] <- NA          # não numéricos -> NA
  suppressWarnings(as.numeric(v))
}

fmt_mean_sd <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return("—")
  m <- mean(x)
  s <- ifelse(length(x) > 1, stats::sd(x), 0)
  paste0(format(round(m, digits), nsmall = digits), " (",
         format(round(s, digits), nsmall = digits), ")")
}

fmt_npct <- function(n, N) sprintf("%d (%.1f%%)", n, ifelse(N > 0, 100*n/N, 0))

exists_col <- function(nm) !is.na(nm) && nm %in% names(df)

# ---------- Mapeamento dos suportes ----------
supports <- tibble::tribble(
  ~unidade, ~flag,     ~days,
  "SUP.O2", "SUP.O2",  NA_character_,
  "VM",     "VM",      "DIAS VM",
  "VNI",    "VNI",     "DIAS VNI",
  "CNAF",   "CNAF",    "DIAS CNAF",
  "CPAP",   "CPAP",    "DIAS CPAP",
  "CNO2",   "CNO2",    NA_character_,
  "HOOD",   "HOOD",    NA_character_
) %>%
  mutate(flag_exists = vapply(flag, exists_col, logical(1)),
         days_exists = vapply(days, exists_col, logical(1)))

# Vetor com as colunas de dias conhecidas (para o proxy)
known_days_cols <- c("DIAS VM","DIAS VNI","DIAS CNAF","DIAS CPAP")
known_days_cols <- known_days_cols[known_days_cols %in% names(df)]

# Parse para numérico das colunas de dias conhecidas
days_known <- NULL
if (length(known_days_cols) > 0) {
  days_known <- lapply(known_days_cols, function(cn) to_days_num(df[[cn]], N_total))
  names(days_known) <- known_days_cols
  days_known <- as_tibble(days_known)
} else {
  # se não houver nenhuma coluna de dias, cria um tibble vazio
  days_known <- tibble()
}

# Soma total de dias (apenas colunas com dias)
total_days_known <- if (ncol(days_known) > 0) {
  days_known %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, pmax(., 0)))) %>%
    transmute(TOTAL_DIAS = rowSums(across(everything()), na.rm = TRUE)) %>%
    pull(TOTAL_DIAS)
} else {
  rep(0, N_total)
}

# ---------- Cálculo por suporte ----------
rows <- list()
used_any   <- rep(FALSE, N_total)  # usou qualquer suporte?
# total para TOTAL(suportes) é sempre a soma dos suportes com dias
total_days_for_total <- total_days_known

for (i in seq_len(nrow(supports))) {
  u     <- supports$unidade[i]
  f     <- supports$flag[i]
  d     <- supports$days[i]
  has_f <- supports$flag_exists[i]
  has_d <- supports$days_exists[i]

  used_flag <- if (has_f) is_yes_vec(df[[f]], N_total) else rep(FALSE, N_total)
  days_vec  <- if (has_d) to_days_num(df[[d]], N_total) else rep(NA_real_, N_total)

  # Usou = flag SIM OU (dias > 0, quando houver dias)
  used <- used_flag | (if (has_d) (days_vec > 0 & !is.na(days_vec)) else FALSE)

  # n (%)
  n_used <- sum(used, na.rm = TRUE)
  npct   <- fmt_npct(n_used, N_total)

  # Média/DP:
  # - Se o suporte TEM coluna de dias -> média/DP entre dias > 0 (entre usados)
  # - Se o suporte NÃO TEM coluna de dias -> proxy = total_days_known ENTRE USADOS (incluindo zeros)
  #   (zeros indicam que a planilha não traz dias para esse suporte específico)
  if (has_d) {
    mean_sd <- fmt_mean_sd(days_vec[used & days_vec > 0])
  } else {
    mean_sd <- fmt_mean_sd(total_days_known[used])    # inclui zeros aqui (proxy)
  }

  rows[[length(rows)+1]] <- tibble(
    Unidade = u,
    `n (%)` = npct,
    `Média (DP) [dias]` = mean_sd
  )

  # acumula "usou qualquer suporte"
  used_any <- used_any | used
}

# ---------- TOTAL (suportes) ----------
row_total <- tibble(
  Unidade = "TOTAL (suportes)",
  `n (%)` = fmt_npct(sum(used_any, na.rm = TRUE), N_total),
  # média/DP do SOMATÓRIO de dias dos suportes com DIAS, entre quem teve total > 0
  `Média (DP) [dias]` = fmt_mean_sd(total_days_for_total[total_days_for_total > 0])
  # Para incluir zeros no total, troque por: fmt_mean_sd(total_days_for_total)
)

resultado <- bind_rows(bind_rows(rows), row_total) %>%
  mutate(ord = case_when(
    Unidade == "SUP.O2" ~ 1,
    Unidade == "VM"     ~ 2,
    Unidade == "VNI"    ~ 3,
    Unidade == "CNAF"   ~ 4,
    Unidade == "CPAP"   ~ 5,
    Unidade == "CNO2"   ~ 6,
    Unidade == "HOOD"   ~ 7,
    Unidade == "TOTAL (suportes)" ~ 8,
    TRUE ~ 9
  )) %>%
  arrange(ord) %>% select(-ord)

print(resultado, n = Inf)

# ---------- Salvar ----------
out_xlsx <- file.path(out_dir, "suportes_ventilatorios_resumo.xlsx")
writexl::write_xlsx(list("Resumo suportes" = resultado), out_xlsx)
message("✅ Arquivo salvo em: ", out_xlsx)
