# ================================================================
# Uso de leitos (n, %), estatísticas de dias
# - Tabela 1: sem corte
# - Tabela 2: com corte de 30 semanas (≤210 dias)
# ================================================================

pkgs <- c("readxl","dplyr","tidyr","stringr","janitor","writexl","lubridate","tibble","purrr","glue")
invisible(lapply(pkgs, library, character.only = TRUE))

# Entrada / Saída
input_file <- "data/input/Banco_de_dados_final_0708_Trajetoria.xlsx"
sheet_name <- "Trajetoria"
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# >>> aqui você reutiliza toda a parte anterior do script que calcula:
# base = tibble com colunas:
#   - used_UTIN, used_UCINCO, used_UCINCA, used_ENF
#   - UTIN, UCINCO, UCINCA, ENF (dias)
#   - Cluster
# ================================================================

fmt_mean_sd <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return("NaN (NA)")
  m <- mean(x, na.rm = TRUE); s <- stats::sd(x, na.rm = TRUE)
  paste0(format(round(m, digits), nsmall = digits), " (",
         format(round(s, digits), nsmall = digits), ")")
}

# função que monta a tabela completa (n (%) + média (DP)) com ou sem filtro
mk_table <- function(data, cutoff = NULL) {
  N_total   <- nrow(data)
  N_cluster <- data %>% count(Cluster) %>%
    tidyr::complete(Cluster = paste("Tipo",1:4), fill = list(n=0))
  n_in_clu  <- setNames(N_cluster$n, N_cluster$Cluster)
  
  mk_line_full <- function(leito, used_col, dias_col) {
    x_all <- data[[dias_col]][data[[used_col]]]
    if (!is.null(cutoff)) x_all <- x_all[x_all <= cutoff]
    
    n_tot <- sum(data[[used_col]], na.rm = TRUE)
    p_tot <- ifelse(N_total>0, 100*n_tot/N_total, NA)
    mean_sd_tot <- fmt_mean_sd(x_all)
    
    out <- tibble(
      Leito = leito,
      Total = glue("{n_tot} ({sprintf('%.1f', p_tot)}%) — {mean_sd_tot}")
    )
    
    for (tp in paste("Tipo",1:4)) {
      sel <- data$Cluster==tp & data[[used_col]]
      x <- data[[dias_col]][sel]
      if (!is.null(cutoff)) x <- x[x <= cutoff]
      n_c <- sum(sel, na.rm=TRUE)
      p_c <- ifelse(n_in_clu[[tp]]>0, 100*n_c/n_in_clu[[tp]], NA)
      mean_sd_c <- fmt_mean_sd(x)
      out[[tp]] <- glue("{n_c} ({sprintf('%.1f', p_c)}%) — {mean_sd_c}")
    }
    out
  }
  
  tabela <- dplyr::bind_rows(
    mk_line_full("UTIN","used_UTIN","UTIN"),
    mk_line_full("UCINCO","used_UCINCO","UCINCO"),
    mk_line_full("UCINCA","used_UCINCA","UCINCA"),
    mk_line_full("ENFERMARIA","used_ENF","ENF")
  )
  
  linha_n <- tibble(
    Leito="N*",
    Total   = as.character(N_total),
    `Tipo 1`= as.character(n_in_clu[["Tipo 1"]]),
    `Tipo 2`= as.character(n_in_clu[["Tipo 2"]]),
    `Tipo 3`= as.character(n_in_clu[["Tipo 3"]]),
    `Tipo 4`= as.character(n_in_clu[["Tipo 4"]])
  )
  
  bind_rows(linha_n, tabela)
}

# ---------------- gerar tabelas ----------------
tabela_sem_corte <- mk_table(base, cutoff = NULL)
tabela_com_corte <- mk_table(base, cutoff = 210)

# ---------------- salvar ----------------
out_xlsx <- file.path(out_dir, "uso_dias_por_leito_duas_tabelas.xlsx")
writexl::write_xlsx(
  list(
    "Sem corte (todos)"   = tabela_sem_corte,
    "Com corte ≤210 dias" = tabela_com_corte
  ),
  out_xlsx
)
message("✅ Arquivo salvo em: ", out_xlsx)

print(tabela_sem_corte, n = nrow(tabela_sem_corte))
print(tabela_com_corte, n = nrow(tabela_com_corte))
