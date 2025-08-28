# ================================================================
# 24_checar_dados_regressao_v2.R
# Mapeia células em branco e destaca no Excel
# ================================================================

# --------- 0) Pacotes ---------
required <- c("readxl","openxlsx","dplyr","tidyr","stringr","purrr","tibble")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) {
  message("Instalando pacotes: ", paste(to_install, collapse = ", "))
  install.packages(to_install, dependencies = TRUE)
}
invisible(lapply(required, library, character.only = TRUE))

# --------- 1) Parâmetros ---------
input_file  <- "data/input/dados_regressão.xlsx"  # ajuste se necessário
sheet_name  <- "Planilha1"                        # ajuste se necessário
output_dir  <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
timestamp   <- format(Sys.time(), "%Y%m%d_%H%M")

out_report  <- file.path(output_dir, sprintf("celulas_em_branco_%s.xlsx", timestamp))
out_copy    <- file.path(output_dir, sprintf("planilha_destacada_%s.xlsx", timestamp))

# Trate também como "em branco" estes padrões (opcional)
treat_as_blank_patterns <- c(
  "^$", "^\\s+$",               # vazio / só espaços
  "^(NA|N/A|null|NULL)$",       # marcadores comuns
  "^(sem\\s*info|sem\\s*informac[aã]o)$"  # "Sem informação"
)

# --------- 2) Leitura ---------
df <- readxl::read_excel(input_file, sheet = sheet_name)

# --------- 3) Funções utilitárias ---------
is_blank_scalar <- function(x) {
  if (is.na(x)) return(TRUE)
  x_chr <- trimws(as.character(x))
  if (identical(x_chr, "")) return(TRUE)
  # padrões extras
  any(stringr::str_detect(tolower(x_chr), treat_as_blank_patterns))
}

# Aplica a detecção por coluna e devolve um data.frame lógico
blank_logical_df <- as.data.frame(lapply(df, function(col) vapply(col, is_blank_scalar, logical(1))))

# Índices (lin, col) das células em branco
idx <- which(as.matrix(blank_logical_df), arr.ind = TRUE)

# --------- 4) Relatório (linha/coluna/valor_original) ---------
if (nrow(idx) > 0) {
  # Valor original com mapeamento seguro
  valores <- purrr::map2_chr(idx[,1], idx[,2], ~ {
    colname <- colnames(df)[.y]
    val <- df[[colname]][.x]
    if (is.na(val)) return(NA_character_)
    as.character(val)
  })

  na_tbl <- tibble::tibble(
    linha_excel = idx[,1] + 1,          # +1 porque a linha 1 é o cabeçalho no Excel exportado
    coluna      = colnames(df)[idx[,2]],
    valor_original = valores
  )

  # Também um resumo por coluna, útil para priorizar limpeza
  resumo <- na_tbl %>%
    count(coluna, name = "qtd_celulas_em_branco") %>%
    arrange(desc(qtd_celulas_em_branco))

  openxlsx::write.xlsx(
    x = list(
      "células_em_branco" = na_tbl,
      "resumo_por_coluna" = resumo
    ),
    file = out_report
  )
  message("Relatório salvo em: ", out_report)
} else {
  message("Nenhuma célula em branco encontrada de acordo com as regras atuais.")
}

# --------- 5) Gerar uma cópia do Excel com células destacadas ---------
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, sheet_name)

# Escreve a tabela completa (com cabeçalho na linha 1)
openxlsx::writeData(wb, sheet = sheet_name, x = df, withFilter = TRUE)

if (nrow(idx) > 0) {
  style_blank <- openxlsx::createStyle(bgFill = "yellow")
  # Atenção: dados começam na linha 2 (linha 1 é cabeçalho)
  # idx[,1] é a linha dentro do df -> Excel row = idx[,1] + 1
  for (k in seq_len(nrow(idx))) {
    excel_row <- idx[k, 1] + 1  # cabeçalho ocupa a 1ª linha
    excel_col <- idx[k, 2]
    openxlsx::addStyle(
      wb, sheet = sheet_name, style = style_blank,
      rows = excel_row + 1 - 1, cols = excel_col, gridExpand = TRUE, stack = TRUE
    )
    # Observação: excel_row +1 -1 mantém +1. (deixado assim pra leitura clara)
  }
}
openxlsx::saveWorkbook(wb, out_copy, overwrite = TRUE)
message("Planilha destacada salva em: ", out_copy)

# --------- 6) Resumo no console ---------
total_blanks <- if (nrow(idx) > 0) nrow(idx) else 0
message("Resumo:")
message(sprintf("* Linhas (dados): %d | Colunas: %d", nrow(df), ncol(df)))
message(sprintf("* Células em branco detectadas: %d", total_blanks))
if (total_blanks > 0) {
  col_counts <- colSums(blank_logical_df)
  msg_cols <- paste(names(col_counts)[col_counts > 0], col_counts[col_counts > 0], sep=":", collapse = " | ")
  message("* Por coluna -> ", msg_cols)
}
