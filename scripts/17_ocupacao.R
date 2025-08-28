# ================================================================
# Ocupação materna — tabela final "n (p%)" por Total e por 4 clusters
# - Colunas: Total (n (%)), TYPE 1..TYPE 4  [p% por coluna TYPE]
# - Saída: data/output/ocupacao_materna_<timestamp>.xlsx (aba: Ocupacao_n(%))
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c("data/input/Banco_de_dados_final_0708_2_OCUPACAO.xlsx")
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")

sheets <- readxl::excel_sheets(input_xlsx)
sheet_name <- if ("GERAL" %in% sheets) "GERAL" else sheets[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("ocupacao_materna_", ts, ".xlsx"))

# (opcional) vírgula decimal (ex.: 14,0%)
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
    idx <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(idx)) return(nms[which(idx)[1]])
  }
  NULL
}

norm <- function(x){
  s <- toupper(trimws(as.character(x)))
  s <- iconv(s, to = "ASCII//TRANSLIT")      # remove acentos
  s <- gsub("\\s+", " ", s)
  s
}

map_ocup <- function(x){
  s <- norm(x)

  # Sem informação -> Outras
  if (s %in% c("", "NA") || grepl("S\\s*/\\s*INFO|SEM\\s*/?INFO|SEM INFORMA", s)) return("Outras")

  # Do lar
  if (grepl("\\b(DO|DIO|DOM|DOR) LAR\\b", s) || grepl("DONA DE CASA", s)) return("Do lar")

  # Estudante
  if (grepl("ESTUDANT", s)) return("Estudante")

  # Desempregada
  if (grepl("DESEMPREG", s)) return("Desempregada")

  # Vendedora / promotora
  if (grepl("VENDEDOR", s) || grepl("PROMOTORA? DE VENDAS", s)) return("Vendedora")

  # Autônoma / MEI / afins
  if (grepl("AUTONOM", s) || grepl("EMPRESAR", s) || grepl("MICROEMPREENDEDOR", s) || grepl("\\bMEI\\b", s) ||
      grepl("COMERCIANT", s) || grepl("CORRETOR", s) || grepl("FEIRANT", s) ||
      grepl("CABEL(E|E)REIR", s) || grepl("MANICURE", s) || grepl("ESTETICIST", s) ||
      grepl("ARTESA", s) || grepl("DESIGN(ER)? DE UNHAS", s) ||
      grepl("DONA DE SALAO", s) || grepl("MERENDEIRA EMPRESARIA", s)) return("Autônoma")

  "Outras"
}

# -------- Leitura --------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

ocu_col <- find_col(names(df0), c("^OCU[_ ]?MAT$", "OCUP", "OCU.*M"))
if (is.null(ocu_col)) stop("❌ Coluna de ocupação (OCU_MAT) não encontrada.")

grp_col <- find_col(names(df0), c("^CU$", "^TYPE$", "^TIPO$", "^cluster$", "^CLUSTER$", "Grupo", "GRUPO", "Group"))
if (is.null(grp_col)) stop("❌ Coluna de cluster/grupo (CU/TYPE/cluster/Grupo) não encontrada.")

# Ocupação normalizada + cluster 1..4
df <- df0 |>
  dplyr::transmute(
    Ocupacao = vapply(.data[[ocu_col]], map_ocup, character(1)),
    GRP_raw  = as.character(.data[[grp_col]])
  )

# extrai 1..4 de qualquer formato (ex.: "type 3", "Grupo 2")
df$GRP <- stringr::str_extract(df$GRP_raw, "[1-4]")

levels_out <- c("Do lar","Estudante","Autônoma","Vendedora","Desempregada","Outras")
grp_levels <- c("1","2","3","4")

df <- df |>
  dplyr::filter(GRP %in% grp_levels) |>
  dplyr::mutate(
    Ocupacao = factor(Ocupacao, levels = levels_out),
    GRP      = factor(GRP,      levels = grp_levels)
  )

# -------- Tabela de contagens (linhas = categorias; colunas = clusters) --------
tab_counts <- with(df, table(Ocupacao, GRP))                 # 6 x 4
counts_df  <- as.data.frame.matrix(tab_counts)
counts_df  <- counts_df[levels_out, , drop = FALSE]          # garante ordem das linhas
colnames(counts_df) <- paste0("TYPE ", grp_levels)           # renomeia colunas

# Percentuais por coluna (cada TYPE soma 100%)
col_sums <- colSums(counts_df)
pct_cols <- sweep(counts_df, 2, ifelse(col_sums == 0, 1, col_sums), "/") * 100

# Coluna Total: n (p%) sobre o total geral
total_n    <- rowSums(counts_df)
N_total    <- sum(total_n)
total_col  <- sprintf("%d (%s)", total_n, fmt_pct(100 * total_n / N_total))

# Monta "n (p%)" para cada TYPE (p = % dentro da coluna)
n_pct_df <- counts_df
for (j in seq_len(ncol(counts_df))) {
  ncol_vals <- as.integer(counts_df[, j])
  pcol_vals <- as.numeric(pct_cols[, j])
  n_pct_df[, j] <- sprintf("%d (%s)", ncol_vals, fmt_pct(pcol_vals))
}

# -------- Tabela final --------
res <- data.frame(
  Categoria      = levels_out,
  `Total (n (%))`= total_col,
  n_pct_df,
  check.names = FALSE
)

# -------- Exporta --------
writexl::write_xlsx(list("Ocupacao_n(%)" = res), path = out_xlsx)

if (use_ptBR_decimal && !inherits(old_locale, "try-error")) {
  try(Sys.setlocale("LC_NUMERIC", old_locale), silent = TRUE)
}

cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
