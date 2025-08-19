# ================================================================
# TIPO DE PARTO (Cesárea / Vaginal) — Total e por cluster
# Saída: XLSX com 3 abas: n/% ; p/DP/EP (num) ; p/DP/EP (em %)
# ================================================================

# -------- Pacotes --------
pkgs <- c("readxl","dplyr","tidyr","writexl","stringi")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c(
  "data/input/Banco_de_dados_final_0708_2_T_PARTO.xlsx",
  "data/input/Banco_de_dados_final_0708_2.xlsx"
)
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("Arquivo de entrada não encontrado.")

sheets <- readxl::excel_sheets(input_xlsx)
sheet_name <- if ("GERAL" %in% sheets) "GERAL" else sheets[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("parto_por_grupo_", ts, ".xlsx"))

# -------- Helpers --------
norm_ascii <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

find_col <- function(nms, patterns) {
  for (pat in patterns) {
    idx <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(idx)) return(nms[which(idx)[1]])
  }
  NULL
}

# Mapeamento robusto: Cesárea / Vaginal (o restante => NA)
cat_parto <- function(x) {
  # tenta numérico (1=Cesárea, 2=Vaginal)
  xv <- suppressWarnings(as.numeric(x))
  if (!is.na(xv)) {
    if (xv == 1) return("Cesárea")
    if (xv == 2) return("Vaginal")
  }
  s <- toupper(trimws(norm_ascii(as.character(x))))
  s <- gsub("\\s+", " ", s)

  # Cesárea: qualquer menção a "CES" (cesarea, cesariana, cesareo)
  if (grepl("(^|[^A-Z])CES", s)) return("Cesárea")

  # Vaginal: "VAGIN" OU "NORMAL" OU "FORCEP"
  if (grepl("VAGIN", s) || grepl("NORMAL", s) || grepl("FORCEP", s)) return("Vaginal")

  # não identificado
  return(NA_character_)
}

levels_out <- c("Cesárea","Vaginal")

counts_table <- function(vec) {
  cat <- vapply(vec, cat_parto, FUN.VALUE = character(1))
  data.frame(
    Categoria = c("Cesárea","Vaginal","Sem informação"),
    n = c(sum(cat == "Cesárea", na.rm = TRUE),
          sum(cat == "Vaginal", na.rm = TRUE),
          sum(is.na(cat))),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(Total_pct = sprintf("%.1f%%", 100 * n / sum(n)))
}

# p = média Bernoulli (Cesárea=1); DP = sqrt(p*(1-p)); EP = sqrt(p*(1-p)/N)
bern_stats <- function(vec) {
  v <- vapply(vec, cat_parto, FUN.VALUE = character(1))
  v <- v[!is.na(v)]
  N <- length(v)
  if (N == 0) return(c(p = NA, dp = NA, ep = NA, N = 0))
  p  <- mean(v == "Cesárea")
  dp <- sqrt(p * (1 - p))
  ep <- sqrt(p * (1 - p) / N)
  c(p = p, dp = dp, ep = ep, N = N)
}

# -------- Leitura --------
df <- read_excel(input_xlsx, sheet = sheet_name)
names(df) <- trimws(names(df))
NMS <- names(df)

parto_col   <- find_col(NMS, c("^T[\\._]?PARTO$", "^PARTO$"))
cluster_col <- find_col(NMS, c("^cluster$", "^cu$", "grupo", "group", "type"))
if (is.null(parto_col))   stop("Coluna T_PARTO/T.PARTO/PARTO não encontrada.")
if (is.null(cluster_col)) stop("Coluna de cluster/grupo não encontrada.")

df <- df |>
  dplyr::rename(T_PARTO = !!parto_col, cluster = !!cluster_col) |>
  dplyr::mutate(cluster = as.character(cluster))

# -------- (1) n e % --------
tab_np <- counts_table(df$T_PARTO) |>
  dplyr::rename(Total_n = n)

# acrescenta n/% por cluster
for (g in sort(unique(df$cluster))) {
  sub <- df[df$cluster == g, , drop = FALSE]
  gp  <- counts_table(sub$T_PARTO)
  tab_np[[paste0(g, "_n")]]   <- gp$n
  tab_np[[paste0(g, "_pct")]] <- gp$Total_pct
}

tab_np <- tab_np |>
  dplyr::mutate(.ord = dplyr::case_when(
    Categoria == "Cesárea" ~ 1L,
    Categoria == "Vaginal" ~ 2L,
    TRUE ~ 99L
  )) |>
  dplyr::arrange(.ord) |>
  dplyr::select(-.ord)

# -------- (2) proporção p, DP, EP --------
tot <- bern_stats(df$T_PARTO)
tab_psd <- tibble::tibble(
  Medida = c("p (Cesárea)", "DP", "EP", "N"),
  Total  = c(tot["p"], tot["dp"], tot["ep"], tot["N"])
)
for (g in sort(unique(df$cluster))) {
  s <- bern_stats(df$T_PARTO[df$cluster == g])
  tab_psd[[as.character(g)]] <- c(s["p"], s["dp"], s["ep"], s["N"])
}

tab_psd_pct <- tab_psd
num_cols <- setdiff(names(tab_psd_pct), "Medida")
tab_psd_pct[num_cols] <- lapply(tab_psd_pct[num_cols], function(col) {
  ifelse(is.na(col), NA, sprintf("%.1f%%", as.numeric(col) * 100))
})

# -------- Exporta --------
writexl::write_xlsx(
  list(
    "Parto_n_pct"        = tab_np,
    "Parto_prop_sd_num"  = tab_psd,
    "Parto_prop_sd_pct"  = tab_psd_pct
  ),
  path = out_xlsx
)

cat("OK! Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
