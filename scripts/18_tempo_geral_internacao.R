# ================================================================
# Tempo total de internação (dias): Média e DP — Total e por cluster
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","lubridate","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c(
  "data/input/dados_gerais_INTERNACAO.xlsx",
  "data/input/Banco_de_dados_final_0708_2.xlsx"
)
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")
sheet_name <- readxl::excel_sheets(input_xlsx)[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("internacao_stats_", ts, ".xlsx"))

# -------- Helpers --------
find_col <- function(nms, patterns){
  for (pat in patterns){
    hit <- which(grepl(pat, nms, ignore.case = TRUE, perl = TRUE))
    if (length(hit)) return(nms[hit[1]])
  }
  NULL
}

as_date_any <- function(x){
  # Date/POSIXct
  if (inherits(x, "Date"))    return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  # Números (serial Excel)
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  # Texto
  s <- trimws(as.character(x))
  s[s == ""] <- NA
  parsed <- suppressWarnings(lubridate::parse_date_time(
    s,
    orders = c("dmy","dmy HMS","dmY","Ymd","ymd","mdy",
               "Y-m-d H:M:S","Y-m-d","d/m/Y H:M:S","d/m/Y","d-m-Y","d.m.Y",
               "m/d/Y H:M:S","m/d/Y"),
    tz = "UTC"
  ))
  as.Date(parsed)
}

pick_group_col <- function(df){
  cands <- c("CU","cluster","CLUSTER","Grupo","GRUPO","Group","TYPE","TIPO")
  hit <- intersect(cands, names(df))
  if (length(hit)) hit[1] else NULL
}

# -------- Leitura --------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

col_adm   <- find_col(names(df0), c("^data[_ ]?adm", "^adm(i(s){0,1})?"))
col_alta  <- find_col(names(df0), c("^data[_ ]?alta", "alta"))
col_obito <- find_col(names(df0), c("^data[_ ]?obito", "obito"))
if (is.null(col_adm))   stop("❌ Coluna de admissão não encontrada (ex.: Data_adm).")
if (is.null(col_alta) & is.null(col_obito))
  stop("❌ Nenhuma coluna de saída encontrada (ex.: Data_alta ou data_obito).")

grp_col <- pick_group_col(df0)  # opcional

# -------- Calcula LOS --------
df <- df0 |>
  dplyr::transmute(
    adm   = as_date_any(.data[[col_adm]]),
    alta  = if (!is.null(col_alta))  as_date_any(.data[[col_alta]])  else as.Date(NA),
    obito = if (!is.null(col_obito)) as_date_any(.data[[col_obito]]) else as.Date(NA),
    grupo = if (!is.null(grp_col)) as.character(.data[[grp_col]]) else NA_character_
  ) |>
  dplyr::mutate(
    saida = dplyr::coalesce(obito, alta),                # prioriza óbito se existir
    los_dias = as.numeric(difftime(saida, adm, units = "days")),
    los_dias = ifelse(los_dias < 0, NA, los_dias)        # descarta inconsistências
  )

# Mantém apenas LOS válido
df <- dplyr::filter(df, !is.na(los_dias))

# -------- Estatísticas (M e DP) --------
res_total <- df |>
  dplyr::summarise(
    `Média (dias)` = round(mean(los_dias, na.rm = TRUE), 2),
    `DP (dias)`    = round(sd(los_dias,   na.rm = TRUE), 2),
    N = dplyr::n()
  ) |>
  dplyr::mutate(Grupo = "Total", .before = 1)

res_grp <- if (!all(is.na(df$grupo))) {
  df |>
    dplyr::group_by(Grupo = grupo) |>
    dplyr::summarise(
      `Média (dias)` = round(mean(los_dias, na.rm = TRUE), 2),
      `DP (dias)`    = round(sd(los_dias,   na.rm = TRUE), 2),
      N = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(Grupo)
} else {
  NULL
}

res_final <- dplyr::bind_rows(res_total, res_grp)

# -------- Exporta --------
writexl::write_xlsx(
  list("Internacao_M_DP" = res_final),
  path = out_xlsx
)

cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
