# ================================================================
# Uso de leitos (n, %) e dias (média, DP) por leito e cluster
# Robusto a variações de nome e formato de data (inclui serial Excel)
# Saída: data/output/uso_e_dias_por_leito.xlsx
# ================================================================

pkgs <- c("readxl","dplyr","tidyr","stringr","janitor","writexl","lubridate","tibble","purrr","glue")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------- Config ----------------
input_file <- "data/input/Banco de dados_final_0708_Trajetoria.xlsx"
sheet_name <- "Trajetoria"
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------- Leitura (texto) ----------------
raw0 <- readxl::read_excel(input_file, sheet = sheet_name, col_types = "text")
raw  <- janitor::clean_names(raw0)

# ---------------- Helpers ----------------
pick_cols <- function(regex) grep(regex, names(raw), ignore.case = TRUE, value = TRUE)

# parser de data bem permissivo + número serial do Excel
parse_date_any <- function(x) {
  x0 <- x
  x  <- trimws(as.character(x)); x[x==""] <- NA
  # 1) tentar ymd_hms, ymd, dmy, mdy, etc.
  d <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("Ymd HMS","Y-m-d H:M:S","dmY HMS","d/m/Y H:M:S","m/d/Y H:M:S",
               "Ymd","Y-m-d","dmY","d/m/Y","mdY","m/d/Y","Ymd HM","Y-m-d H:M"),
    tz = "UTC",
    quiet = TRUE
  ))
  # 2) onde falhou, tentar como número serial do Excel (dias desde 1899-12-30)
  idx <- is.na(d) & !is.na(x)
  if (any(idx)) {
    num <- suppressWarnings(as.numeric(x[idx]))
    ok  <- !is.na(num)
    if (any(ok)) {
      d[idx][ok] <- as.Date(num[ok], origin = "1899-12-30")
      # se quiser horário zero
      d[idx][ok] <- as.POSIXct(d[idx][ok])
    }
  }
  d
}

fmt_mean_sd <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return("NaN (NA)")
  m <- mean(x, na.rm = TRUE); s <- stats::sd(x, na.rm = TRUE)
  paste0(format(round(m, digits), nsmall = digits), " (",
         format(round(s, digits), nsmall = digits), ")")
}

# ---------------- Colunas de eventos ----------------
# aceita com/sem underscore e sufixos "...n" (duplicatas)
utin1   <- pick_cols("^utin_?1(?:\\.\\.\\.[0-9]+)?$")
utin2   <- pick_cols("^utin_?2(?:\\.\\.\\.[0-9]+)?$")
utin3   <- pick_cols("^utin_?3(?:\\.\\.\\.[0-9]+)?$")
ucinco1 <- pick_cols("^ucinco_?1(?:\\.\\.\\.[0-9]+)?$")
ucinco2 <- pick_cols("^ucinco_?2(?:\\.\\.\\.[0-9]+)?$")
ucinco3 <- pick_cols("^ucinco_?3(?:\\.\\.\\.[0-9]+)?$")
ucinca1 <- pick_cols("^ucinca_?1(?:\\.\\.\\.[0-9]+)?$")
ucinca2 <- pick_cols("^ucinca_?2(?:\\.\\.\\.[0-9]+)?$")
ucinca3 <- pick_cols("^ucinca_?3(?:\\.\\.\\.[0-9]+)?$")
# ENF pode duplicar
enf_cands <- pick_cols("^enf_?alcon(?:\\.\\.\\.[0-9]+)?$|^enfermaria(?:\\.\\.\\.[0-9]+)?$")
alta_col  <- pick_cols("^d_?alta_?h$")
obito_col <- pick_cols("^d_?o[bB]ito$|^d_?.*bito$")

if (length(alta_col)==0 && length(obito_col)==0)
  stop("❌ Não encontrei 'D.ALTA.H' nem 'D.ÓBITO' (após clean_names).")

# escolher coluna ENF com mais datas válidas
choose_best_enf <- function(cands) {
  if (!length(cands)) return(character(0))
  counts <- sapply(cands, function(cn) sum(!is.na(parse_date_any(raw[[cn]]))))
  cands[which.max(counts)]
}
enf_col <- choose_best_enf(enf_cands)

# ---------------- Parse datas + diagnóstico ----------------
df <- raw
to_parse <- c(utin1, utin2, utin3, ucinco1, ucinco2, ucinco3, ucinca1, ucinca2, ucinca3, enf_col, alta_col, obito_col)
for (cn in to_parse) df[[cn]] <- parse_date_any(df[[cn]])

diag_count <- function(cols) if (length(cols)) sum(rowSums(!is.na(df[, cols, drop=FALSE]))>0) else 0
message("Diagnóstico (linhas com ALGUMA data no grupo):")
message("  UTIN(1/2/3): ", diag_count(c(utin1,utin2,utin3)))
message("  UCINCO:      ", diag_count(c(ucinco1,ucinco2,ucinco3)))
message("  UCINCA:      ", diag_count(c(ucinca1,ucinca2,ucinca3)))
message("  ENF:         ", if (length(enf_col)) sum(!is.na(df[[enf_col]])) else 0)
message("  Alta:        ", if (length(alta_col)) sum(!is.na(df[[alta_col]])) else 0)
message("  Óbito:       ", if (length(obito_col)) sum(!is.na(df[[obito_col]])) else 0)

# ---------------- Cálculo de dias por linha ----------------
calc_dias_row <- function(i) {
  r <- df[i, , drop = FALSE]
  events <- tibble(date = as.POSIXct(character()), unit = character())

  add_ev <- function(cols, label) {
    if (length(cols)) {
      for (cn in cols) if (!is.na(r[[cn]])) events <- add_row(events, date = r[[cn]], unit = label)
    }
    events
  }

  events <- add_ev(c(utin1,utin2,utin3), "UTIN")
  events <- add_ev(c(ucinco1,ucinco2,ucinco3), "UCINCO")
  events <- add_ev(c(ucinca1,ucinca2,ucinca3), "UCINCA")
  if (length(enf_col)) events <- add_ev(enf_col, "ENF")

  end_date <- NA
  if (length(alta_col) && !is.na(r[[alta_col]])) end_date <- r[[alta_col]]
  if (is.na(end_date) && length(obito_col) && !is.na(r[[obito_col]])) end_date <- r[[obito_col]]

  events <- arrange(events, date)

  durs <- c(UTIN=0, UCINCO=0, UCINCA=0, ENF=0)
  if (nrow(events)>0) {
    for (k in seq_len(nrow(events))) {
      next_date <- if (k < nrow(events)) events$date[k+1] else end_date
      if (!is.na(next_date)) {
        delta <- as.numeric(difftime(next_date, events$date[k], units="days"))
        if (is.finite(delta) && delta >= 0) durs[events$unit[k]] <- durs[events$unit[k]] + delta
      }
    }
  }
  durs
}

mat <- t(vapply(seq_len(nrow(df)), calc_dias_row, numeric(4)))
colnames(mat) <- c("UTIN","UCINCO","UCINCA","ENF")
dias <- as.data.frame(mat)

# ---------------- Quem USOU (contagem por conteúdo, não por parse) ----------------
used_from_content <- function(cols) {
  if (!length(cols)) return(rep(FALSE, nrow(raw)))
  sub <- raw[, cols, drop=FALSE]
  rowSums(nchar(trimws(apply(sub, 2, as.character))) > 0, na.rm = TRUE) > 0
}
used_UTIN   <- used_from_content(c(utin1,utin2,utin3))
used_UCINCO <- used_from_content(c(ucinco1,ucinco2,ucinco3))
used_UCINCA <- used_from_content(c(ucinca1,ucinca2,ucinca3))
used_ENF    <- if (length(enf_col)) used_from_content(enf_col) else rep(FALSE, nrow(raw))

# ---------------- Cluster ----------------
cluster_col <- if ("type" %in% names(raw)) "type" else if ("cluster" %in% names(raw)) "cluster" else if ("tipo" %in% names(raw)) "tipo" else stop("Sem coluna de cluster.")
clusters <- tolower(trimws(raw[[cluster_col]]))
clusters <- case_when(
  clusters %in% c("type 1","1") ~ "Tipo 1",
  clusters %in% c("type 2","2") ~ "Tipo 2",
  clusters %in% c("type 3","3") ~ "Tipo 3",
  clusters %in% c("type 4","4") ~ "Tipo 4",
  TRUE ~ raw[[cluster_col]]
)

base <- tibble(
  used_UTIN, used_UCINCO, used_UCINCA, used_ENF,
  UTIN = dias$UTIN, UCINCO = dias$UCINCO, UCINCA = dias$UCINCA, ENF = dias$ENF,
  Cluster = clusters
)

# ---------------- Tabela A: uso (n, %) ----------------
N_total   <- nrow(base)
N_cluster <- base %>% count(Cluster) %>% tidyr::complete(Cluster = paste("Tipo",1:4), fill = list(n=0))
n_in_clu  <- setNames(N_cluster$n, N_cluster$Cluster)

mk_line_uso <- function(leito, used_col) {
  n_tot <- sum(base[[used_col]], na.rm = TRUE); p_tot <- ifelse(N_total>0, 100*n_tot/N_total, NA)
  out <- tibble(Leito = leito, Total = glue("{n_tot} ({sprintf('%.1f', p_tot)}%)"))
  for (tp in paste("Tipo",1:4)) {
    n_c <- sum(base$Cluster==tp & base[[used_col]], na.rm=TRUE)
    p_c <- ifelse(n_in_clu[[tp]]>0, 100*n_c/n_in_clu[[tp]], NA)
    out[[tp]] <- glue("{n_c} ({sprintf('%.1f', p_c)}%)")
  }
  out
}
tabela_uso <- bind_rows(
  mk_line_uso("UTIN","used_UTIN"),
  mk_line_uso("UCINCO","used_UCINCO"),
  mk_line_uso("UCINCA","used_UCINCA"),
  mk_line_uso("ENFERMARIA","used_ENF")
)
linha_n <- tibble(Leito="N*", Total=as.character(N_total),
                  `Tipo 1`=as.character(n_in_clu[["Tipo 1"]]),
                  `Tipo 2`=as.character(n_in_clu[["Tipo 2"]]),
                  `Tipo 3`=as.character(n_in_clu[["Tipo 3"]]),
                  `Tipo 4`=as.character(n_in_clu[["Tipo 4"]]))
tabela_uso <- bind_rows(linha_n, tabela_uso)

# ---------------- Tabela B: dias (média, DP) entre QUEM USOU ----------------
mk_line_dias <- function(leito, used_col, dias_col) {
  sel_tot <- base[[used_col]]
  line <- tibble(Leito = leito, Total = fmt_mean_sd(base[[dias_col]][sel_tot]))
  for (tp in paste("Tipo",1:4)) {
    sel <- base$Cluster==tp & base[[used_col]]
    line[[tp]] <- fmt_mean_sd(base[[dias_col]][sel])
  }
  line
}
tabela_dias <- bind_rows(
  mk_line_dias("UTIN","used_UTIN","UTIN"),
  mk_line_dias("UCINCO","used_UCINCO","UCINCO"),
  mk_line_dias("UCINCA","used_UCINCA","UCINCA"),
  mk_line_dias("ENFERMARIA","used_ENF","ENF")
)

# ---------------- Saída ----------------
out_xlsx <- file.path(out_dir, "uso_e_dias_por_leito.xlsx")
writexl::write_xlsx(
  list(
    "Tabela A - Uso do leito (n, %)" = tabela_uso,
    "Tabela B - Dias entre quem usou" = tabela_dias
  ),
  out_xlsx
)
message("✅ Arquivo gerado: ", out_xlsx)

print(tabela_uso, n = nrow(tabela_uso))
print(tabela_dias, n = nrow(tabela_dias))
