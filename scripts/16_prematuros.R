# ================================================================
# Prematuridade por TYPE — Tabela final no formato "n (p%)"
# - Cada coluna TYPE: p% dentro do TYPE (colunas somam 100%)
# - Coluna Total: p% sobre o N geral
# - Saída: data/output/prematuridade_por_TYPE_<timestamp>.xlsx
# ================================================================

# ---------------- Pacotes ----------------
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------- Config ----------------
input_candidates <- c("data/input/Banco_de_dados_final_0708_2_PREMATURO.xlsx")
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")

sheets <- readxl::excel_sheets(input_xlsx)
sheet_name <- if ("GERAL" %in% sheets) "GERAL" else sheets[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("prematuridade_por_TYPE_", ts, ".xlsx"))

# (opcional) vírgula como separador decimal (ex.: 3,3%)
use_ptBR_decimal <- FALSE
old_locale <- NULL
if (use_ptBR_decimal) {
  old_locale <- try(Sys.getlocale("LC_NUMERIC"), silent = TRUE)
  try(Sys.setlocale("LC_NUMERIC", "pt_BR.UTF-8"), silent = TRUE) # macOS/Linux
  # Windows: try(Sys.setlocale("LC_NUMERIC", "Portuguese_Brazil.1252"), silent = TRUE)
}
fmt_pct <- function(x) sprintf("%.1f%%", x)

# ---------------- Helpers ----------------
find_col <- function(nms, patterns){
  for (pat in patterns){
    idx <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(idx)) return(nms[which(idx)[1]])
  }
  NULL
}

# 34+8 -> 35+1
parse_IG <- function(x){
  s <- toupper(trimws(as.character(x)))
  s <- gsub("\\s+", "", s)
  m <- stringr::str_match(s, "^(\\d{1,2})(?:\\+(\\d{1,2}))?$")
  if (is.na(m[1])) return(c(sem=NA_real_, dia=NA_real_, total_dias=NA_real_))
  w <- as.numeric(m[2]); d <- as.numeric(ifelse(is.na(m[3]), 0, m[3]))
  if (is.na(w) || is.na(d)) return(c(sem=NA_real_, dia=NA_real_, total_dias=NA_real_))
  if (d >= 7) { w <- w + floor(d/7); d <- d %% 7 }
  c(sem=w, dia=d, total_dias = w*7 + d)
}

cat_premat <- function(total_dias){
  if (is.na(total_dias)) return(NA_character_)
  if (total_dias <= (27*7 + 6)) return("Extremamente prematuro")
  if (total_dias <= (31*7 + 6)) return("Muito prematuro")
  if (total_dias <= (33*7 + 6)) return("Prematuro moderado")
  if (total_dias <= (36*7 + 6)) return("Prematuro tardio")
  NA_character_
}

cat_levels  <- c("Extremamente prematuro","Muito prematuro","Prematuro moderado","Prematuro tardio")
type_levels <- c("1","2","3","4")

# ---------------- Leitura ----------------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

ig_col   <- find_col(names(df0), c("^IG$", "IG.*", "IDADE\\s*GEST", "GEST.*SEMAN"))
type_col <- find_col(names(df0), c("^type$", "^tipo$", "^cluster$", "grupo", "group"))
if (is.null(ig_col))   stop("❌ Coluna IG não encontrada.")
if (is.null(type_col)) stop("❌ Coluna TYPE/cluster/grupo não encontrada.")

df <- df0[, c(ig_col, type_col)]
names(df) <- c("IG", "TYPE_raw")

df$TYPE <- as.character(df$TYPE_raw)
df$TYPE <- stringr::str_extract(df$TYPE, "[1-4]")

pars <- t(vapply(df$IG, parse_IG, FUN.VALUE = c(sem=NA_real_, dia=NA_real_, total_dias=NA_real_)))
df$total_dias <- pars[, "total_dias"]
df$Categoria  <- vapply(df$total_dias, cat_premat, character(1))

df_clean <- subset(df, TYPE %in% type_levels & !is.na(Categoria))
df_clean$Categoria <- factor(df_clean$Categoria, levels = cat_levels)
df_clean$TYPE      <- factor(df_clean$TYPE,      levels = type_levels)

# ---------------- Contagens e percentuais ----------------
tab_counts <- with(df_clean, table(Categoria, TYPE))
counts_df  <- as.data.frame.matrix(tab_counts)[cat_levels, , drop = FALSE]
colnames(counts_df) <- paste0("TYPE ", colnames(counts_df))

col_sums <- colSums(counts_df)
pct_cols <- sweep(counts_df, 2, ifelse(col_sums==0, 1, col_sums), "/") * 100

total_n    <- rowSums(counts_df)
N_geral    <- sum(total_n)
total_str  <- sprintf("%d (%s)", total_n, fmt_pct(100 * total_n / N_geral))

# monta "n (p%)" para cada TYPE
n_pct_df <- counts_df
for (j in seq_len(ncol(counts_df))) {
  ncol_vals <- as.integer(counts_df[, j])
  pcol_vals <- as.numeric(pct_cols[, j])
  n_pct_df[, j] <- sprintf("%d (%s)", ncol_vals, fmt_pct(pcol_vals))
}

# ---------------- Tabela final ----------------
sheet_n_pct <- data.frame(
  Categoria = cat_levels,
  `Total (n (%))` = total_str,
  n_pct_df[cat_levels, , drop = FALSE],
  check.names = FALSE
)

# ---------------- Exporta ----------------
writexl::write_xlsx(
  list("Prematuridade_n(%)" = sheet_n_pct),
  path = out_xlsx
)

if (use_ptBR_decimal && !inherits(old_locale, "try-error")) {
  try(Sys.setlocale("LC_NUMERIC", old_locale), silent = TRUE)
}
cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
