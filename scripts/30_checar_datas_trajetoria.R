# ================================================================
# Auditoria de datas da Trajetoria
# - Parsing robusto (vários formatos + serial Excel)
# - Formato suspeito, fora da faixa, coerência temporal por linha
# - Durações por leito, deltas, total > 210 (30 semanas)
# - Saída: data/output/relatorio_qualidade_datas.xlsx
# ================================================================

# ---------- Pacotes ----------
pkgs <- c("readxl","dplyr","tidyr","stringr","janitor",
          "writexl","lubridate","purrr","tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------- Configurações ----------
input_file <- "data/input/Banco de dados_final_0708_Trajetoria.xlsx"
sheet_name <- "Trajetoria"
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

MIN_DATE  <- as.Date("2000-01-01")  # limite inferior plausível
MAX_DATE  <- Sys.Date() + 1         # pequena tolerância futura
CUT_TOTAL <- 210                    # 30 semanas

# ---------- Leitura ----------
raw0 <- readxl::read_excel(input_file, sheet = sheet_name, col_types = "text")
raw  <- janitor::clean_names(raw0)
n_rows <- nrow(raw)

# ---------- Helpers ----------
pick_cols <- function(regex) grep(regex, names(raw), ignore.case = TRUE, value = TRUE)

# Parser permissivo + serial Excel (datas sem horário viram 00:00)
parse_date_any <- function(x) {
  x <- trimws(as.character(x)); x[x==""] <- NA
  d <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c(
      "Ymd HMS","Y-m-d H:M:S","dmY HMS","d/m/Y H:M:S","m/d/Y H:M:S",
      "Ymd","Y-m-d","dmY","d/m/Y","mdY","m/d/Y","Ymd HM","Y-m-d H:M"
    ),
    tz = "UTC", quiet = TRUE
  ))
  idx <- is.na(d) & !is.na(x)
  if (any(idx)) {
    num <- suppressWarnings(as.numeric(x[idx]))
    ok  <- !is.na(num)
    if (any(ok)) d[idx][ok] <- as.POSIXct(as.Date(num[ok], origin = "1899-12-30"))
  }
  d
}

# Formato suspeito: YYYY-DD-MM (meio > 12) OU não parseável e não vazio
is_suspect_format <- function(x) {
  x <- trimws(as.character(x)); x[is.na(x)] <- ""
  m <- regexec("^([0-9]{4})[-/]([0-9]{2})[-/]([0-9]{2})$", x)
  reg <- regmatches(x, m)
  sus_mid_gt_12 <- vapply(reg, function(grp) {
    if (length(grp) == 4) suppressWarnings(as.numeric(grp[3])) > 12 else FALSE
  }, logical(1))
  d <- suppressWarnings(parse_date_any(x))
  not_parseable <- is.na(d) & nzchar(x)
  sus_mid_gt_12 | not_parseable
}

# Tentar identificar uma coluna de ID (opcional)
guess_id_col <- function() {
  cands <- c("id","id_paciente","rn","prontuario","prontuário","patient_id","subject_id")
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}
id_col <- guess_id_col()

# ---------- Mapas de colunas ----------
leitos <- list(
  UTIN   = c("^utin_1$", "^utin_2$", "^utin_3$"),
  UCINCO = c("^ucinco_1$", "^ucinco_2$", "^ucinco_3$"),
  UCINCA = c("^ucinca_1$", "^ucinca_2$", "^ucinca_3$"),
  ENF    = c("^enf_alcon(?:.*)?$", "^enfermaria(?:.*)?$") # ENF pode ser 1+ colunas
)

ALTA_REGEX  <- "^d_?alta_?h$"
OBITO_REGEX <- "^d_?o[bB]ito$|^d_?.*bito$"
NASC_REGEX  <- "^d_?nasc$|^data_?nascimento$|^dn$"  # se existir

# Resolver nomes reais das colunas
col_map <- list()
for (nm in names(leitos)) {
  cols <- unique(unlist(lapply(leitos[[nm]], pick_cols)))
  col_map[[nm]] <- cols
}
alta_col  <- pick_cols(ALTA_REGEX)
obito_col <- pick_cols(OBITO_REGEX)
nasc_col  <- pick_cols(NASC_REGEX)

# ---------- Parsing + métricas por coluna ----------
parsed <- list()
col_summ <- list()
aux_cols <- unique(c(unlist(col_map), alta_col, obito_col, nasc_col))

for (cn in aux_cols) {
  v_raw <- raw[[cn]]
  v_par <- parse_date_any(v_raw)
  parsed[[cn]] <- v_par

  non_empty <- sum(nzchar(trimws(as.character(v_raw))), na.rm = TRUE)
  valid     <- sum(!is.na(v_par))
  invalid   <- non_empty - valid
  suspect   <- sum(is_suspect_format(v_raw), na.rm = TRUE)
  out_range <- sum(!is.na(v_par) & (as.Date(v_par) < MIN_DATE | as.Date(v_par) > MAX_DATE))

  col_summ[[cn]] <- tibble::tibble(
    coluna = cn,
    n_total = n_rows,
    n_na_ou_vazio = n_rows - non_empty,
    n_nao_vazio = non_empty,
    n_validas = valid,
    n_invalidas = invalid,
    n_suspeitas_formato = suspect,
    n_fora_da_faixa = out_range
  )
}
resumo_colunas <- dplyr::bind_rows(col_summ) %>% dplyr::arrange(coluna)

# ---------- Listas detalhadas ----------
# 1) Não parseadas mas não vazias
invalid_cells <- purrr::map_dfr(aux_cols, function(cn) {
  v_raw <- raw[[cn]]; v_par <- parsed[[cn]]
  idx <- is.na(v_par) & nzchar(trimws(as.character(v_raw)))
  if (!any(idx)) return(NULL)
  tibble::tibble(
    linha = which(idx),
    coluna = cn,
    valor  = v_raw[idx],
    !!(if (!is.na(id_col)) id_col else "id") := if (!is.na(id_col)) raw[[id_col]][idx] else NA_character_
  )
})

# 2) Fora da faixa
oor_cells <- purrr::map_dfr(aux_cols, function(cn) {
  v_par <- parsed[[cn]]
  idx <- !is.na(v_par) & (as.Date(v_par) < MIN_DATE | as.Date(v_par) > MAX_DATE)
  if (!any(idx)) return(NULL)
  tibble::tibble(
    linha = which(idx),
    coluna = cn,
    data   = as.Date(v_par[idx]),
    !!(if (!is.na(id_col)) id_col else "id") := if (!is.na(id_col)) raw[[id_col]][idx] else NA_character_
  )
})

# 3) Suspeitas de formato
suspect_cells <- purrr::map_dfr(aux_cols, function(cn) {
  v_raw <- raw[[cn]]
  idx <- is_suspect_format(v_raw)
  if (!any(idx)) return(NULL)
  tibble::tibble(
    linha = which(idx),
    coluna = cn,
    valor  = v_raw[idx],
    !!(if (!is.na(id_col)) id_col else "id") := if (!is.na(id_col)) raw[[id_col]][idx] else NA_character_
  )
})

# ---------- Coerência por linha (eventos ordenados + deltas) ----------
calc_row_issues <- function(i) {
  ev <- tibble::tibble(date = as.POSIXct(character()), unit = character(), col = character())

  add_ev <- function(cols, label) {
    if (!length(cols)) return()
    for (cn in cols) {
      v <- parsed[[cn]][i]
      if (!is.na(v)) ev <<- dplyr::bind_rows(ev, tibble::tibble(date = v, unit = label, col = cn))
    }
  }
  for (nm in names(col_map)) add_ev(col_map[[nm]], nm)

  end_date <- NA
  if (length(alta_col)  && !is.na(parsed[[alta_col]][i]))  end_date <- parsed[[alta_col]][i]
  if (is.na(end_date) && length(obito_col) && !is.na(parsed[[obito_col]][i])) end_date <- parsed[[obito_col]][i]

  ev <- dplyr::arrange(ev, .data$date)

  durs <- c(UTIN=0, UCINCO=0, UCINCA=0, ENF=0)
  issues <- character()

  if (nrow(ev) > 0) {
    for (k in seq_len(nrow(ev))) {
      next_date <- if (k < nrow(ev)) ev$date[k+1] else end_date
      if (!is.na(next_date)) {
        delta <- as.numeric(difftime(next_date, ev$date[k], units = "days"))
        if (is.finite(delta)) {
          if (delta < 0) issues <- c(issues, sprintf("delta_negativo(%s→%s)=%g", ev$col[k], if (k<nrow(ev)) ev$col[k+1] else "fim", delta))
          if (delta == 0) issues <- c(issues, sprintf("delta_zero(%s)", ev$col[k]))
          if (delta > CUT_TOTAL) issues <- c(issues, sprintf("delta_maior_%d(%s→%s)=%g", CUT_TOTAL, ev$col[k], if (k<nrow(ev)) ev$col[k+1] else "fim", round(delta,1)))
          if (delta >= 0) durs[ev$unit[k]] <- durs[ev$unit[k]] + delta
        }
      } else {
        issues <- c(issues, sprintf("sem_fim_apos_%s", ev$col[k]))
      }
    }
  } else {
    if (!is.na(end_date)) issues <- c(issues, "fim_sem_evento")
  }

  tot <- sum(durs)
  if (is.finite(tot) && tot > CUT_TOTAL) issues <- c(issues, sprintf("total_maior_%d=%g", CUT_TOTAL, round(tot,1)))

  tibble::tibble(
    linha = i,
    !!(if (!is.na(id_col)) id_col else "id") := if (!is.na(id_col)) raw[[id_col]][i] else NA_character_,
    problemas = if (length(issues)) paste(issues, collapse=" | ") else NA_character_,
    dur_utin = durs["UTIN"], dur_ucinco = durs["UCINCO"], dur_ucinca = durs["UCINCA"], dur_enf = durs["ENF"],
    total = as.numeric(tot)
  )
}

row_issues <- purrr::map_dfr(seq_len(n_rows), calc_row_issues)

series_problemas <- row_issues %>% dplyr::filter(!is.na(problemas))

duracoes_gt210 <- row_issues %>%
  dplyr::filter(
    (!is.na(dur_utin)   & dur_utin   > CUT_TOTAL) |
    (!is.na(dur_ucinco) & dur_ucinco > CUT_TOTAL) |
    (!is.na(dur_ucinca) & dur_ucinca > CUT_TOTAL) |
    (!is.na(dur_enf)    & dur_enf    > CUT_TOTAL)
  )

tot_gt210 <- row_issues %>% dplyr::filter(!is.na(total) & total > CUT_TOTAL)

sem_fim <- row_issues %>%
  dplyr::mutate(.prob = ifelse(is.na(problemas), "", problemas)) %>%
  dplyr::filter(grepl("sem_fim", .prob, fixed = TRUE)) %>%
  dplyr::select(-.prob)

# ---------- Duplicidades detalhadas ----------
dup_detalhe <- purrr::map_dfr(seq_len(n_rows), function(i) {
  ev <- tibble::tibble(date = as.POSIXct(character()), unit = character(), col = character())
  add_ev <- function(cols, label) {
    if (length(cols)) {
      for (cn in cols) {
        v <- parsed[[cn]][i]
        if (!is.na(v)) ev <<- dplyr::bind_rows(ev, tibble::tibble(date = v, unit = label, col = cn))
      }
    }
  }
  for (nm in names(col_map)) add_ev(col_map[[nm]], nm)
  ev <- dplyr::arrange(ev, date)
  dups <- ev %>% dplyr::count(unit, date, name="freq") %>% dplyr::filter(freq > 1)
  if (nrow(dups) == 0) return(NULL)
  dups$linha <- i
  if (!is.na(id_col)) dups[[id_col]] <- raw[[id_col]][i]
  dups
})

if (is.null(dup_detalhe) || nrow(dup_detalhe) == 0) {
  dup_detalhe <- tibble::tibble(
    linha = integer(),
    unit  = character(),
    date  = as.POSIXct(character()),
    freq  = integer(),
    !!(if (!is.na(id_col)) id_col else "id") := character()
  )
} else {
  cols_order <- c("linha","unit","date","freq", if (!is.na(id_col)) id_col else "id")
  dup_detalhe <- dplyr::select(dup_detalhe, dplyr::all_of(cols_order))
}

# ---------- Exportar ----------
out_xlsx <- file.path(out_dir, "relatorio_qualidade_datas.xlsx")
writexl::write_xlsx(
  list(
    "Resumo por coluna"      = resumo_colunas,
    "Celulas invalidas"      = invalid_cells,
    "Datas fora da faixa"    = oor_cells,
    "Formato suspeito"       = suspect_cells,
    "Serie por linha (prob)" = series_problemas,
    "Duracoes >210"          = duracoes_gt210,
    "TOT >210"               = tot_gt210,
    "Sem alta ou obito"      = sem_fim,
    "Duplicidades"           = dup_detalhe
  ),
  out_xlsx
)

message("✅ Relatório salvo em: ", out_xlsx)
print(resumo_colunas, n = nrow(resumo_colunas))
