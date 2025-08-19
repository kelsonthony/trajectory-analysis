# ================================================================
# ESCOLARIZAÇÃO (ESC_MAT) -> % por Total e por Grupo (CU/cluster)
# Saída: XLSX com a tabela final
# ================================================================

# -------- Pacotes --------
pkgs <- c("readxl","dplyr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_xlsx <- "data/input/Banco_de_dados_final_0708_2.xlsx"  # ajuste caminho
sheet_name <- "GERAL"                                        # ajuste se necessário
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts         <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx   <- file.path(out_dir, paste0("escolarizacao_por_grupo_", ts, ".xlsx"))

# -------- Leitura --------
df <- read_excel(input_xlsx, sheet = sheet_name)
names(df) <- trimws(names(df))

# Garante que a coluna se chama "ESC_MAT"
if (!"ESC_MAT" %in% names(df)) stop("❌ Coluna ESC_MAT não encontrada.")

# Detecta coluna de grupo (se existir)
group_candidates <- c("CU","cluster","CLUSTER","Grupo","GRUPO","Group","TYPE","TIPO")
group_col <- group_candidates[group_candidates %in% names(df)]
group_col <- if (length(group_col)) group_col[1] else NULL

# -------- Tabela --------
levels_out <- c("SUPERIOR COMP","SUPERIOR INC",
                "MÉDIO COMP","MÉDIO INC",
                "FUND COMP","FUND INC",
                "S/INFO")

# função de % por nível
pct_tab <- function(vec) {
  vec <- factor(vec, levels = levels_out)
  tab <- table(vec, useNA = "ifany")
  pct <- round(100 * tab / sum(tab), 1)
  sprintf("%s%%", pct)
}

res <- data.frame(Categoria = levels_out, check.names = FALSE)

# total
res[["Total (%)"]] <- pct_tab(df$ESC_MAT)

# por grupo (se existir)
if (!is.null(group_col)) {
  for (g in unique(df[[group_col]])) {
    sub <- df[df[[group_col]] == g, , drop = FALSE]
    res[[as.character(g)]] <- pct_tab(sub$ESC_MAT)
  }
}

# -------- Exporta --------
write_xlsx(list("Escolarizacao" = res), path = out_xlsx)
cat("✅ Arquivo gerado em:\n", out_xlsx, "\n")
