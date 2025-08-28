# ================================================================
# Uso de leitos (n, %), média e DP de dias — por leito e cluster
# ================================================================

pkgs <- c("readxl","dplyr","tidyr","stringr","janitor","writexl","lubridate","tibble","purrr","glue")
invisible(lapply(pkgs, library, character.only = TRUE))

# Entrada / Saída
input_file <- "data/input/Banco de dados_final_0708_Trajetoria.xlsx"
sheet_name <- "Trajetoria"
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Leitura já processada (como no script anterior)
raw0 <- readxl::read_excel(input_file, sheet = sheet_name, col_types = "text")
raw  <- janitor::clean_names(raw0)

# >>> Aqui você reutiliza a parte anterior do código (parse de datas,
#     cálculo de dias, flags de uso, montagem do objeto base).
# Para simplificar, vou assumir que você já tem `base` montado com:
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

N_total   <- nrow(base)
N_cluster <- base %>% count(Cluster) %>% tidyr::complete(Cluster = paste("Tipo",1:4), fill = list(n=0))
n_in_clu  <- setNames(N_cluster$n, N_cluster$Cluster)

# -------- Tabela única (uso + média/DP) --------
mk_line_full <- function(leito, used_col, dias_col) {
  # total
  n_tot <- sum(base[[used_col]], na.rm = TRUE); p_tot <- ifelse(N_total>0, 100*n_tot/N_total, NA)
  mean_sd_tot <- fmt_mean_sd(base[[dias_col]][base[[used_col]]])
  out <- tibble(
    Leito = leito,
    Total = glue("{n_tot} ({sprintf('%.1f', p_tot)}%) — {mean_sd_tot}")
  )
  # clusters
  for (tp in paste("Tipo",1:4)) {
    n_c <- sum(base$Cluster==tp & base[[used_col]], na.rm=TRUE)
    p_c <- ifelse(n_in_clu[[tp]]>0, 100*n_c/n_in_clu[[tp]], NA)
    mean_sd_c <- fmt_mean_sd(base[[dias_col]][base$Cluster==tp & base[[used_col]]])
    out[[tp]] <- glue("{n_c} ({sprintf('%.1f', p_c)}%) — {mean_sd_c}")
  }
  out
}

tabela_full <- dplyr::bind_rows(
  mk_line_full("UTIN","used_UTIN","UTIN"),
  mk_line_full("UCINCO","used_UCINCO","UCINCO"),
  mk_line_full("UCINCA","used_UCINCA","UCINCA"),
  mk_line_full("ENFERMARIA","used_ENF","ENF")
)

# Linha N* (apenas n)
linha_n <- tibble(
  Leito="N*",
  Total   = as.character(N_total),
  `Tipo 1`= as.character(n_in_clu[["Tipo 1"]]),
  `Tipo 2`= as.character(n_in_clu[["Tipo 2"]]),
  `Tipo 3`= as.character(n_in_clu[["Tipo 3"]]),
  `Tipo 4`= as.character(n_in_clu[["Tipo 4"]])
)

tabela_final <- bind_rows(linha_n, tabela_full)

# -------- Salvar --------
out_xlsx <- file.path(out_dir, "uso_dias_por_leito_completo.xlsx")
writexl::write_xlsx(list("Uso + Média(DP)" = tabela_final), out_xlsx)

print(tabela_final, n = nrow(tabela_final))
message("✅ Arquivo salvo em: ", out_xlsx)
