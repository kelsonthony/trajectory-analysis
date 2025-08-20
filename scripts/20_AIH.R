# ================================================================
# Somatório do valor Total_AIH — Total e por cluster
# Saída: data/output/aih_somatorio_YYYYMMDD_HHMM.xlsx
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c(
  "data/input/dados_gerais_AIH.xlsx",
  "data/input/Banco_de_dados_final_0708_2.xlsx"
)
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")
sheet_name <- readxl::excel_sheets(input_xlsx)[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("aih_somatorio_", ts, ".xlsx"))

# -------- Helpers --------
find_col <- function(nms, patterns){
  for (pat in patterns){
    hit <- which(grepl(pat, nms, ignore.case = TRUE, perl = TRUE))
    if (length(hit)) return(nms[hit[1]])
  }
  NULL
}

pick_group_col <- function(df){
  cands <- c("CU","cluster","CLUSTER","Grupo","GRUPO","Group","TYPE","TIPO")
  hit <- intersect(cands, names(df))
  if (length(hit)) hit[1] else NULL
}

# Converte valores monetários/numéricos em número (pt-BR, com R$, ponto, vírgula etc.)
as_num_br <- function(v){
  if (is.numeric(v)) return(as.numeric(v))
  s <- trimws(as.character(v))
  s[s == ""] <- NA
  # remove tudo que não seja dígito, ponto, vírgula, sinal
  s <- gsub("[^0-9,.-]", "", s)
  # regra: se tem vírgula e ponto, vírgula é decimal -> remove pontos e troca vírgula por ponto
  has_comma <- grepl(",", s, fixed = TRUE)
  has_dot   <- grepl("\\.", s)
  s[has_comma & has_dot] <- gsub("\\.", "", s[has_comma & has_dot])
  s[has_comma] <- gsub(",", ".", s[has_comma], fixed = TRUE)
  suppressWarnings(as.numeric(s))
}

# -------- Leitura --------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

aih_col <- find_col(names(df0), c("^TOTAL[_ ]?AIH$", "VALOR.*AIH", "TOTAL.*AIH", "\\bAIH\\b"))
if (is.null(aih_col)) stop("❌ Coluna 'Total_AIH' (ou similar) não encontrada.")

grp_col <- pick_group_col(df0)  # pode ser NULL

# -------- Prepara e soma --------
df <- df0 |>
  dplyr::transmute(
    Grupo_raw = if (!is.null(grp_col)) as.character(.data[[grp_col]]) else NA_character_,
    Valor_AIH = as_num_br(.data[[aih_col]])
  ) |>
  dplyr::mutate(
    # se quiser padronizar para TYPE 1..4, extrai o dígito; caso contrário, mantém o texto original
    Grupo = if (!is.null(grp_col)) {
      gsub("^\\s+|\\s+$", "", Grupo_raw)
    } else {
      "Total"
    },
    .keep = "unused"
  )

# Total
res_total <- df |>
  dplyr::summarise(
    Grupo = "Total",
    `Somatório Total_AIH` = round(sum(Valor_AIH, na.rm = TRUE), 2),
    N = dplyr::n(),
    `N com valor` = sum(!is.na(Valor_AIH))
  )

# Por grupo (se existir coluna de grupo)
res_grp <- if (!is.null(grp_col)) {
  df |>
    dplyr::group_by(Grupo) |>
    dplyr::summarise(
      `Somatório Total_AIH` = round(sum(Valor_AIH, na.rm = TRUE), 2),
      N = dplyr::n(),
      `N com valor` = sum(!is.na(Valor_AIH)),
      .groups = "drop"
    ) |>
    dplyr::arrange(Grupo)
} else {
  NULL
}

# Junta total + grupos
res_final <- dplyr::bind_rows(res_total, res_grp)

# -------- Exporta --------
writexl::write_xlsx(list("AIH_Sum" = res_final), path = out_xlsx)
cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
