# ================================================================
# Gestações prévias (G-1) — 1 linha com "M=..%; DP=..%" por TYPE
# As células (por TYPE) são normalizadas para somar 100% (M e DP).
# Inclui Total (=100%;100%) e N_total / N_válido.
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c(
  "data/input/Banco_de_dados_final_0708_2_GESTACAO_TRATE.xlsx"
)
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")

sheets <- readxl::excel_sheets(input_xlsx)
sheet_name <- if ("GERAL" %in% sheets) "GERAL" else sheets[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("gestacao_previas_compacto_pctShares_", ts, ".xlsx"))

# -------- Helpers --------
find_col <- function(nms, patterns){
  for (pat in patterns){
    idx <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(idx)) return(nms[which(idx)[1]])
  }
  NULL
}
normalize_code <- function(x){
  s <- gsub("[\"'\\r\\n\\s]", "", toupper(iconv(as.character(x), to="ASCII//TRANSLIT")))
  gsub("[^A-Z0-9]", "", s)
}
# extrai G; se não houver P e G tiver 2–3 dígitos (ex.: G391A1), usa só o 1º dígito como G
extract_G <- function(code){
  s <- normalize_code(code)
  if (s %in% c("", "S/INFO", "S/INFORMACAO", "SEMINFO", "NA", "N/A")) return(NA_real_)
  mG <- stringr::str_match(s, "G(\\d+)")
  if (is.na(mG[1])) return(NA_real_)
  digits <- mG[2]
  hasP <- grepl("P\\d+", s)
  if (!hasP && nchar(digits) %in% c(2,3)) return(as.numeric(substr(digits,1,1)))
  as.numeric(digits)
}
fmt_pct_cell <- function(p_m, p_sd, digits = 2){
  paste0("M=", format(round(p_m, digits), nsmall = digits), "%; ",
         "DP=", format(round(p_sd, digits), nsmall = digits), "%")
}

# -------- Leitura --------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

gest_col    <- find_col(names(df0), c("Gx?P?y?A?z?", "GESTACAO", "GESTA(C|Ç)AO", "GPA"))
cluster_col <- find_col(names(df0), c("^type$", "^tipo$", "^cluster$", "^cu$", "grupo", "group"))
if (is.null(gest_col))    stop("❌ Coluna GxPyAz (gestação) não identificada.")
if (is.null(cluster_col)) stop("❌ Coluna TYPE/cluster/grupo não identificada.")

df <- df0 |>
  dplyr::select(GxPyAz = dplyr::all_of(gest_col),
                TYPE   = dplyr::all_of(cluster_col)) |>
  dplyr::mutate(TYPE = as.character(TYPE))

# -------- Gestações prévias = max(G - 1, 0) --------
G_vals <- vapply(df$GxPyAz, extract_G, FUN.VALUE = numeric(1))
df$gest_prev <- ifelse(is.na(G_vals), NA_real_, pmax(G_vals - 1, 0))

# Totais
N_total  <- nrow(df)                          # TODAS as linhas do arquivo
N_valido <- sum(!is.na(df$gest_prev))         # linhas usadas no cálculo

# -------- Estatísticas por TYPE (média e DP originais) --------
stats_type <- df |>
  dplyr::filter(!is.na(gest_prev)) |>
  dplyr::group_by(TYPE) |>
  dplyr::summarise(M = mean(gest_prev),
                   DP = stats::sd(gest_prev),
                   .groups = "drop") |>
  dplyr::arrange(TYPE)

# -------- Normaliza para % (as células somam 100%) --------
sumM  <- sum(stats_type$M,  na.rm = TRUE)
sumSD <- sum(stats_type$DP, na.rm = TRUE)

# Evita divisão por zero (se todas DP forem NA/0)
if (sumSD == 0 || is.na(sumSD)) stats_type$DP_share <- 0 else stats_type$DP_share <- 100 * stats_type$DP / sumSD
stats_type$M_share <- if (sumM == 0 || is.na(sumM)) 0 else 100 * stats_type$M / sumM

# -------- Monta a linha final --------
cells <- setNames(fmt_pct_cell(stats_type$M_share, stats_type$DP_share), stats_type$TYPE)

out <- data.frame(t(cells), check.names = FALSE, stringsAsFactors = FALSE)
out <- cbind(
  Medida = "Gestações prévias (G-1) — shares",
  Total  = "M=100.00%; DP=100.00%",
  out
)
out$N_total  <- N_total
out$N_válido <- N_valido
rownames(out) <- NULL

# -------- Exporta --------
writexl::write_xlsx(list("Gprev_compacto_pctShares" = out), path = out_xlsx)
cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
