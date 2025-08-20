# ================================================================
# Uso de substâncias (Álcool, Drogas, Tabaco)
# % por TYPE (1–4) e % Total, incluindo "Sem informação"
# Saída: data/output/substancias_por_TYPE_YYYYMMDD_HHMM.xlsx
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c(
  "data/input/dados_gerais_SUBSTANCIAS.xlsx",
  "data/input/Banco_de_dados_final_0708_2.xlsx"
)
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")
sheet_name <- readxl::excel_sheets(input_xlsx)[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("substancias_por_TYPE_", ts, ".xlsx"))

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

# normaliza texto, protegendo NA e bytes inválidos
norm <- function(x){
  if (length(x) != 1L) x <- x[1]
  if (is.na(x)) return(NA_character_)
  s <- toupper(trimws(as.character(x)))
  s <- iconv(s, to = "ASCII//TRANSLIT", sub = "")  # bytes inválidos viram ""
  if (is.na(s)) return(NA_character_)
  s <- gsub("\\s+", " ", s)
  if (s == "") return(NA_character_) else s
}

# classifica em Sim / Não / Sem informação, sem deixar NA no if()
class3 <- function(x){
  s <- norm(x)
  # Sem informação (NA, vazio, "S/INFO", "SEM INFO", etc.)
  if (is.na(s) || s %in% c("NA","N/A") ||
      grepl("(^| )S\\s*/\\s*INFO( |$)|SEM\\s*INFO|SEM\\s*INFORMA", s, perl = TRUE))
    return("Sem informação")

  # SIM (tolerante a 1, YES, USO, FUMANTE, ALCOOLISTA etc.)
  if (grepl("(^|\\b)(SIM|YES|Y|1|USA|USO|FUMANTE|TABAGISTA|ALCOOLISTA)(\\b|$)", s, perl = TRUE))
    return("Sim")

  # NAO / N / 0 / NEG (após norm, NÃO -> NAO)
  if (grepl("(^|\\b)(NAO|NO|N|0|NAO USA|NAO USO|NUNCA|NEG)(\\b|$)", s, perl = TRUE))
    return("Não")

  "Sem informação"
}

fmt_pct <- function(x) sprintf("%.1f%%", x)

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

# função que monta o bloco de uma substância
build_block <- function(vec, substancia_nome){
  tmp <- data.frame(TYPE = df$TYPE, val = vec)
  tmp$Categoria <- vapply(tmp$val, class3, character(1))
  tmp$Categoria <- factor(tmp$Categoria, levels = levels_cat)

  # Tabela de contagens Categoria x TYPE (com zeros para ausentes)
  tab <- with(tmp, table(Categoria, TYPE))
  counts <- as.data.frame.matrix(tab)
  # garante colunas 1..4 na ordem, preenchendo 0 se necessário
  for (lvl in levels_type) if (!lvl %in% colnames(counts)) counts[[lvl]] <- 0L
  counts <- counts[, levels_type, drop = FALSE]

  # % por TYPE (cada coluna soma 100%)
  colN <- colSums(counts)
  denom <- ifelse(colN == 0, 1, colN)  # evita divisão por zero
  pct  <- sweep(counts, 2, denom, "/") * 100
  pct  <- as.data.frame(lapply(as.data.frame(pct), fmt_pct), check.names = FALSE)
  colnames(pct) <- paste0("TYPE ", levels_type, " (%)")

  # % total (inclui "Sem informação" para somar 100%)
  total_pct <- rowSums(counts) / sum(counts) * 100

  data.frame(
    `Substância` = substancia_nome,
    `Categoria`  = levels_cat,
    `Total (%)`  = fmt_pct(total_pct),
    pct,
    check.names = FALSE
  )
}

blocks <- list()
if (!is.null(col_ALC)) blocks[["alcool"]] <- build_block(df$ALC, "Álcool")
if (!is.null(col_DRO)) blocks[["drogas"]] <- build_block(df$DRO, "Drogas")
if (!is.null(col_TAB)) blocks[["tabaco"]] <- build_block(df$TAB, "Tabaco")

res <- dplyr::bind_rows(blocks)

# Ordena como solicitado: Álcool, Drogas, Tabaco e categorias Sim/Não/Sem informação
ord_subst <- c("Álcool","Drogas","Tabaco")
res <- res |>
  dplyr::mutate(Substância = factor(Substância, levels = ord_subst)) |>
  dplyr::arrange(Substância, match(Categoria, levels_cat)) |>
  dplyr::mutate(Substância = as.character(Substância))

# -------- Exporta --------
writexl::write_xlsx(list("Substancias_%" = res), path = out_xlsx)
cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
