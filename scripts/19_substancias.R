# ================================================================
# Uso de substâncias (Álcool, Drogas, Tabaco) — "n (p%)"
# - Linhas: Sim / Não / Sem informação, por substância
# - Colunas: Total (n (%))  [% no total geral da substância]
#            TYPE 1..TYPE 4 [cada coluna soma 100%]
# - Saída: data/output/substancias_por_TYPE_<timestamp>.xlsx
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c("data/input/dados_gerais_SUBSTANCIAS.xlsx")
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")
sheet_name <- readxl::excel_sheets(input_xlsx)[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("substancias_por_TYPE_", ts, ".xlsx"))

# (opcional) vírgula como separador decimal: 33,3%
use_ptBR_decimal <- FALSE
old_locale <- NULL
if (use_ptBR_decimal) {
  old_locale <- try(Sys.getlocale("LC_NUMERIC"), silent = TRUE)
  try(Sys.setlocale("LC_NUMERIC", "pt_BR.UTF-8"), silent = TRUE)  # macOS/Linux
  # Windows: try(Sys.setlocale("LC_NUMERIC", "Portuguese_Brazil.1252"), silent = TRUE)
}
fmt_pct <- function(x) sprintf("%.1f%%", x)

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

# normaliza texto
norm <- function(x){
  if (length(x) != 1L) x <- x[1]
  if (is.na(x)) return(NA_character_)
  s <- toupper(trimws(as.character(x)))
  s <- iconv(s, to = "ASCII//TRANSLIT", sub = "")  # remove acentos/bytes ruins
  if (is.na(s)) return(NA_character_)
  s <- gsub("\\s+", " ", s)
  if (s == "") return(NA_character_) else s
}

# classifica em Sim / Não / Sem informação
class3 <- function(x){
  s <- norm(x)
  if (is.na(s) || s %in% c("NA","N/A") ||
      grepl("(^| )S\\s*/\\s*INFO( |$)|SEM\\s*INFO|SEM\\s*INFORMA", s, perl = TRUE))
    return("Sem informação")
  if (grepl("(^|\\b)(SIM|YES|Y|1|USA|USO|FUMANTE|TABAGISTA|ALCOOLISTA)(\\b|$)", s, perl = TRUE))
    return("Sim")
  if (grepl("(^|\\b)(NAO|NO|N|0|NAO USA|NAO USO|NUNCA|NEG)(\\b|$)", s, perl = TRUE))
    return("Não")
  "Sem informação"
}

levels_type <- c("1","2","3","4")
levels_cat  <- c("Sim","Não","Sem informação")

# -------- Leitura --------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

col_ALC <- find_col(names(df0), c("^ALC\\b","ALCO","ALCOOL"))
col_TAB <- find_col(names(df0), c("^TAB\\b","TABAC","FUMO"))
col_DRO <- find_col(names(df0), c("^DRO\\b","DROG"))
grp_col <- pick_group_col(df0)

if (is.null(col_ALC) & is.null(col_TAB) & is.null(col_DRO))
  stop("❌ Não encontrei ALC/TAB/DRO na planilha.")
if (is.null(grp_col))
  stop("❌ Não encontrei coluna de cluster/TYPE (CU/cluster/Grupo/TYPE...).")

df <- df0 |>
  dplyr::transmute(
    TYPE = stringr::str_extract(as.character(.data[[grp_col]]), "[1-4]"),
    ALC  = if (!is.null(col_ALC)) .data[[col_ALC]] else NA,
    TAB  = if (!is.null(col_TAB)) .data[[col_TAB]] else NA,
    DRO  = if (!is.null(col_DRO)) .data[[col_DRO]] else NA
  ) |>
  dplyr::mutate(TYPE = factor(TYPE, levels = levels_type))

# -------- Função que monta bloco "n (p%)" por substância --------
build_block <- function(vec, substancia_nome){
  tmp <- data.frame(TYPE = df$TYPE, val = vec)
  tmp$Categoria <- vapply(tmp$val, class3, character(1))
  tmp$Categoria <- factor(tmp$Categoria, levels = levels_cat)

  # Contagens Categoria x TYPE (6? não; aqui 3 x 4)
  tab <- with(tmp, table(Categoria, TYPE))
  counts <- as.data.frame.matrix(tab)
  # garante colunas 1..4 e ordem
  for (lvl in levels_type) if (!lvl %in% colnames(counts)) counts[[lvl]] <- 0L
  counts <- counts[, levels_type, drop = FALSE]

  # Percentuais por TYPE (cada coluna soma 100%)
  colN <- colSums(counts)
  denom <- ifelse(colN == 0, 1, colN)
  pct_cols <- sweep(counts, 2, denom, "/") * 100  # matrix 3x4

  # Coluna Total: n (p%) no total geral da substância
  total_n  <- rowSums(counts)               # 3x1
  N_total  <- sum(total_n)
  total_str <- sprintf("%d (%s)", total_n, fmt_pct(100 * total_n / N_total))

  # Formata cada TYPE como "n (p%)"
  n_pct_df <- counts
  for (j in seq_len(ncol(counts))) {
    ncol_vals <- as.integer(counts[, j])
    pcol_vals <- as.numeric(pct_cols[, j])
    n_pct_df[, j] <- sprintf("%d (%s)", ncol_vals, fmt_pct(pcol_vals))
  }
  colnames(n_pct_df) <- paste0("TYPE ", levels_type)

  data.frame(
    `Substância`   = substancia_nome,
    `Categoria`    = levels_cat,
    `Total (n (%))`= total_str,
    n_pct_df,
    check.names = FALSE
  )
}

# -------- Monta resultado para ALC, DRO, TAB --------
blocks <- list()
if (!is.null(col_ALC)) blocks[["alcool"]] <- build_block(df$ALC, "Álcool")
if (!is.null(col_DRO)) blocks[["drogas"]] <- build_block(df$DRO, "Drogas")
if (!is.null(col_TAB)) blocks[["tabaco"]] <- build_block(df$TAB, "Tabaco")

res <- dplyr::bind_rows(blocks)

# Ordena substâncias e categorias
ord_subst <- c("Álcool","Drogas","Tabaco")
res <- res |>
  dplyr::mutate(Substância = factor(Substância, levels = ord_subst)) |>
  dplyr::arrange(Substância, match(Categoria, levels_cat)) |>
  dplyr::mutate(Substância = as.character(Substância))

# -------- Exporta --------
writexl::write_xlsx(list("Substancias_n(%)" = res), path = out_xlsx)

if (use_ptBR_decimal && !inherits(old_locale, "try-error")) {
  try(Sys.setlocale("LC_NUMERIC", old_locale), silent = TRUE)
}

cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
