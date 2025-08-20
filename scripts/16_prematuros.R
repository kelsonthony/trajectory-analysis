# ================================================================
# Prematuridade por TYPE — % por categoria (colunas somam 100%)
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c(
  "data/input/Banco_de_dados_final_0708_2_PREMATURO.xlsx",
  "data/input/Banco_de_dados_final_0708_2.xlsx"
)
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")
sheets <- readxl::excel_sheets(input_xlsx)
sheet_name <- if ("GERAL" %in% sheets) "GERAL" else sheets[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("prematuridade_por_TYPE_", ts, ".xlsx"))

# -------- Helpers --------
find_col <- function(nms, patterns){
  for (pat in patterns){
    idx <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(idx)) return(nms[which(idx)[1]])
  }
  NULL
}

# Extrai semanas+dias; 34+8 -> 35+1
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

# Classificação de prematuridade
cat_premat <- function(total_dias){
  if (is.na(total_dias)) return(NA_character_)
  if (total_dias <= (27*7 + 6)) return("Extremamente prematuro")
  if (total_dias <= (31*7 + 6)) return("Muito prematuro")
  if (total_dias <= (33*7 + 6)) return("Prematuro moderado")
  if (total_dias <= (36*7 + 6)) return("Prematuro tardio")
  NA_character_  # >= 37s -> fora do escopo
}

fmt_pct <- function(x) sprintf("%.1f%%", x)

cat_levels  <- c("Extremamente prematuro",
                 "Muito prematuro",
                 "Prematuro moderado",
                 "Prematuro tardio")
type_levels <- c("1","2","3","4")

# -------- Leitura --------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

ig_col   <- find_col(names(df0), c("^IG$", "IG.*", "IDADE\\s*GEST", "GEST.*SEMAN"))
type_col <- find_col(names(df0), c("^type$", "^tipo$", "^cluster$", "grupo", "group"))
if (is.null(ig_col))   stop("❌ Coluna IG não encontrada.")
if (is.null(type_col)) stop("❌ Coluna TYPE/cluster/grupo não encontrada.")

df <- df0[, c(ig_col, type_col)]
names(df) <- c("IG", "TYPE_raw")

# Normaliza TYPE: extrai o dígito 1–4 de qualquer formato
df$TYPE <- as.character(df$TYPE_raw)
df$TYPE <- stringr::str_extract(df$TYPE, "[1-4]")

# -------- Converte IG e classifica --------
pars <- t(vapply(df$IG, parse_IG, FUN.VALUE = c(sem=NA_real_, dia=NA_real_, total_dias=NA_real_)))
df$sem        <- pars[, "sem"]
df$dia        <- pars[, "dia"]
df$total_dias <- pars[, "total_dias"]
df$Categoria  <- vapply(df$total_dias, cat_premat, character(1))

# Mantém só registros válidos (TYPE 1–4 e categoria definida)
df_clean <- subset(df, TYPE %in% type_levels & !is.na(Categoria))
df_clean$Categoria <- factor(df_clean$Categoria, levels = cat_levels)
df_clean$TYPE      <- factor(df_clean$TYPE,      levels = type_levels)

# -------- Contagens por categoria x TYPE --------
tab_counts <- with(df_clean, table(Categoria, TYPE))          # inclui zeros para níveis ausentes
counts_df  <- as.data.frame.matrix(tab_counts)                # linhas=categorias; colunas=TYPE
counts_df  <- counts_df[cat_levels, , drop = FALSE]           # garante ordem das linhas
colnames(counts_df) <- paste0("TYPE ", colnames(counts_df))   # "TYPE 1" .. "TYPE 4"

# Percentuais por TYPE (cada coluna soma 100%)
col_sums <- colSums(counts_df)
pct_df   <- sweep(counts_df, 2, ifelse(col_sums==0, 1, col_sums), FUN = "/") * 100
pct_df   <- as.data.frame(lapply(pct_df, fmt_pct), check.names = FALSE)

# Total por categoria (n) e Total (%)
total_n   <- rowSums(counts_df)
total_pct <- fmt_pct(100 * total_n / sum(total_n))

# -------- Tabela final (sem linhas duplicadas) --------
res <- data.frame(
  Categoria = cat_levels,
  `Total (%)` = total_pct,
  n = as.integer(total_n),
  check.names = FALSE
)
res <- cbind(res, pct_df)

# -------- Exporta --------
writexl::write_xlsx(list("Prematuridade_%" = res), path = out_xlsx)
cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
