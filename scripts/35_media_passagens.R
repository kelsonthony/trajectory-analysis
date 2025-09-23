# ================================================================
# Ocupação por unidade em dias (UTIN, UCINCO, UCINCA, ENF)
# - Constrói timeline por RN a partir das colunas de ENTRADA:
#     "UTIN 1/2/3", "UCINCO 1/2/3", "UCINCA 1/2/3", "ENF/ALCON"
#   e dos eventos de término: "D.ALTA.H", "D.ÓBITO"
# - Duração = diferença até o próximo evento; atribuída à unidade de origem
# - Saída: n (%), Média (DP) [dias] por unidade (entre usuários da unidade)
#          + tabela individual (dias totais por RN em cada unidade)
# ================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(janitor)
  library(writexl)
  library(glue)
  library(purrr)
  library(lubridate)
})

# ---------------------- Entrada ----------------------
input_file <- "data/input/Banco_de_dados_final_0708_Trajetoria.xlsx"  # (nome com espaço)
sheet_name <- "Trajetoria"
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

raw <- readxl::read_excel(input_file, sheet = sheet_name)

# Nome da coluna de identificação do RN (ajuste se necessário)
id_col <- "ID RN"
stopifnot(id_col %in% names(raw))

# Trabalhamos com nomes originais (sem clean_names) para casar 100% com o Excel
N_total <- nrow(raw)

# Colunas candidatas (algumas podem não existir na planilha; trataremos com segurança)
cols_utin   <- intersect(c("UTIN 1","UTIN 2","UTIN 3"), names(raw))
cols_ucinco <- intersect(c("UCINCO 1","UCINCO 2","UCINCO 3"), names(raw))
cols_ucinca <- intersect(c("UCINCA 1","UCINCA 2","UCINCA 3"), names(raw))
col_enf     <- intersect("ENF/ALCON", names(raw))
col_alta    <- intersect("D.ALTA.H",  names(raw))
col_obito   <- intersect("D.ÓBITO",   names(raw))

# Se nada disso existir, aborta com mensagem clara
if (length(c(cols_utin, cols_ucinco, cols_ucinca, col_enf, col_alta, col_obito)) == 0) {
  stop("Nenhuma das colunas esperadas foi encontrada. Verifique nomes no Excel.")
}

# ---------------------- Timeline por RN ----------------------
# Monta um data.frame longo com (RN, unidade/evento, data)
build_events_long <- function(df) {
  # Helper para empilhar um conjunto de colunas sob um rótulo de unidade
  stack_unit <- function(cols, unit_name) {
    if (length(cols) == 0) return(NULL)
    df %>%
      select(all_of(c(id_col, cols))) %>%
      pivot_longer(cols = all_of(cols),
                   names_to = "colname", values_to = "date") %>%
      mutate(unidade = unit_name)
  }
  # Unidades
  ev_utin   <- stack_unit(cols_utin,   "UTIN")
  ev_ucinco <- stack_unit(cols_ucinco, "UCINCO")
  ev_ucinca <- stack_unit(cols_ucinca, "UCINCA")
  ev_enf    <- stack_unit(col_enf,     "ENF")         # pode ser 0 ou 1 coluna

  # Eventos de término (não são unidades, só marcam fim do período)
  ev_alta <- if (length(col_alta) == 1) {
    df %>%
      select(all_of(c(id_col, col_alta))) %>%
      rename(date = all_of(col_alta)) %>%
      mutate(colname = col_alta, unidade = "ALTA")
  } else NULL

  ev_obito <- if (length(col_obito) == 1) {
    df %>%
      select(all_of(c(id_col, col_obito))) %>%
      rename(date = all_of(col_obito)) %>%
      mutate(colname = col_obito, unidade = "OBITO")
  } else NULL

  events <- bind_rows(ev_utin, ev_ucinco, ev_ucinca, ev_enf, ev_alta, ev_obito) %>%
    filter(!is.na(date)) %>%                         # só eventos com data
    mutate(date = as_datetime(date)) %>%             # força POSIXct (seguro p/ ordenar)
    arrange(.data[[id_col]], date)
  events
}

events_long <- build_events_long(raw)

# Se algum RN não tem nenhum evento (linha vazia), garantimos presença com 0 dias
rn_sem_evento <- setdiff(raw[[id_col]], unique(events_long[[id_col]]))
if (length(rn_sem_evento) > 0) {
  warning(glue("Há {length(rn_sem_evento)} RN(s) sem qualquer evento datado; ficarão com 0 dias em todas as unidades."))
}

# ---------------------- Intervalos e atribuição ----------------------
# Para cada RN, ordenar por data e calcular duração até o próximo evento.
# Atribuímos a duração à unidade do evento atual **se** ela for uma unidade clínica (não ALTA/ÓBITO).
valid_units <- c("UTIN","UCINCO","UCINCA","ENF")

events_with_next <- events_long %>%
  group_by(.data[[id_col]]) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(next_date = lead(date),
         next_unit = lead(unidade)) %>%
  ungroup()

# Calcula duração em dias (não negativa) e remove linhas sem próximo evento
intervals <- events_with_next %>%
  filter(!is.na(next_date)) %>%
  mutate(days = as.numeric(difftime(next_date, date, units = "days")),
         days = ifelse(is.finite(days) & days > 0, days, 0)) %>%
  # mantém somente intervalos cuja unidade de origem é clínica (não ALTA/ÓBITO)
  filter(unidade %in% valid_units)

# ---------------------- Dias por RN e unidade ----------------------
days_by_rn_unit <- intervals %>%
  group_by(.data[[id_col]], unidade) %>%
  summarise(dias = sum(days, na.rm = TRUE), .groups = "drop")

# Garante presença de 0 para RN que não usaram alguma unidade
# (wide para facilitar leitura e export)
days_by_rn_wide <- days_by_rn_unit %>%
  tidyr::pivot_wider(names_from = unidade, values_from = dias, values_fill = 0) %>%
  # garante colunas para todas as unidades
  mutate(
    UTIN   = ifelse(is.na(UTIN),   0, UTIN),
    UCINCO = ifelse(is.na(UCINCO), 0, UCINCO),
    UCINCA = ifelse(is.na(UCINCA), 0, UCINCA),
    ENF    = ifelse(is.na(ENF),    0, ENF)
  ) %>%
  # reanexar RNs sem qualquer evento
  right_join(raw %>% select(all_of(id_col)), by = setNames(id_col, id_col)) %>%
  mutate(across(all_of(valid_units), ~replace_na(., 0))) %>%
  arrange(.data[[id_col]])

# ---------------------- Resumo (n (%), Média (DP)) ----------------------
fmt_mean_sd <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return("0.00 (0.00)")
  m <- mean(x); s <- stats::sd(x)
  paste0(format(round(m, digits), nsmall = digits), " (",
         format(round(s, digits), nsmall = digits), ")")
}

fmt_npct <- function(n, N) sprintf("%d (%.1f%%)", n, ifelse(N > 0, 100*n/N, 0))

summ_unit <- function(unit_name) {
  v <- days_by_rn_wide[[unit_name]]
  used <- v > 0
  n_used <- sum(used, na.rm = TRUE)
  tibble(
    Unidade = unit_name,
    `n (%)` = fmt_npct(n_used, N_total),
    `Média (DP) [dias]` = if (n_used > 0) fmt_mean_sd(v[used]) else "0.00 (0.00)"
  )
}

resumo <- bind_rows(lapply(valid_units, summ_unit)) %>%
  mutate(ord = match(Unidade, valid_units)) %>%
  arrange(ord) %>%
  select(-ord)

# ---------------------- Saída ----------------------
print(resumo, n = Inf)

out_xlsx <- file.path(out_dir, "ocupacao_unidades_resumo_e_individual.xlsx")
writexl::write_xlsx(
  list(
    "Resumo por unidade"      = resumo,
    "Dias por indivíduo (wide)" = days_by_rn_wide
  ),
  out_xlsx
)
message("✅ Arquivo salvo em: ", out_xlsx)

# ---------------------- QA opcional ----------------------
# Para auditoria, você pode inspecionar alguns casos manualmente:
# exemplo_id <- days_by_rn_wide[[id_col]][1]
# events_long %>% filter(`ID RN` == exemplo_id) %>% arrange(date)
# intervals   %>% filter(`ID RN` == exemplo_id) %>% arrange(date)
