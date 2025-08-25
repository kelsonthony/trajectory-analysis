# ================================================================
# 26_hist_IG_por_categoria.R
# Histogramas de Idade Gestacional (IG) por classificação de prematuridade
# - Segue a teoria: <=27+6, 28..31+6, 32..33+6, 34..36+6
# - Saídas em data/output/ (PNG) com fundo branco
# ================================================================

# ---------------- Pacotes ----------------
pkgs <- c("readxl","dplyr","stringr","ggplot2","tidyr","forcats","readr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------- Config ----------------
in_xlsx_candidates <- c("data/input/Banco_de_dados_final_0708_2_PREMATURO.xlsx")
in_xlsx <- NULL
for (p in in_xlsx_candidates) if (file.exists(p)) { in_xlsx <- p; break }
if (is.null(in_xlsx)) stop("❌ Arquivo de entrada não encontrado.")

sheets <- readxl::excel_sheets(in_xlsx)
sheet_name <- if ("GERAL" %in% sheets) "GERAL" else sheets[1]

out_dir <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts <- format(Sys.time(), "%Y%m%d_%H%M")

# ---------------- Helpers ----------------
find_col <- function(nms, patterns){
  for (pat in patterns){
    idx <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(idx)) return(nms[which(idx)[1]])
  }
  NULL
}

# 34+8 -> 35+1; devolve: semanas decimais e total em dias
parse_IG <- function(x){
  s <- toupper(trimws(as.character(x)))
  s <- gsub("\\s+", "", s)
  m <- stringr::str_match(s, "^(\\d{1,2})(?:\\+(\\d{1,2}))?$")
  if (is.na(m[1])) return(c(sem=NA_real_, dias=NA_real_, total_dias=NA_real_, sem_dec=NA_real_))
  w <- suppressWarnings(as.numeric(m[2]))
  d <- suppressWarnings(as.numeric(ifelse(is.na(m[3]), 0, m[3])))
  if (is.na(w) || is.na(d)) return(c(sem=NA_real_, dias=NA_real_, total_dias=NA_real_, sem_dec=NA_real_))
  if (d >= 7) { w <- w + floor(d/7); d <- d %% 7 }
  total_dias <- w*7 + d
  sem_dec    <- w + d/7
  c(sem=w, dias=d, total_dias=total_dias, sem_dec=sem_dec)
}

# Classificação pela teoria
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

# Cores (fill) e contorno
pal_fill <- c(
  "Extremamente prematuro" = "#1F78B4",  # azul
  "Muito prematuro"        = "#FF7F00",  # laranja
  "Prematuro moderado"     = "#33A02C",  # verde
  "Prematuro tardio"       = "#E31A1C"   # vermelho
)
pal_line <- pal_fill  # contorno igual ao fill

# ---------------- Leitura ----------------
df0 <- readxl::read_excel(in_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

ig_col   <- find_col(names(df0), c("^IG$", "IG.*", "IDADE\\s*GEST", "GEST.*SEMAN"))
type_col <- find_col(names(df0), c("^type$", "^tipo$", "^cluster$", "grupo", "group"))
if (is.null(ig_col))   stop("❌ Coluna IG não encontrada.")
if (is.null(type_col)) stop("❌ Coluna TYPE/cluster/grupo não encontrada.")

df <- df0[, c(ig_col, type_col)]
names(df) <- c("IG", "TYPE_raw")

# Parse IG
pars <- t(vapply(df$IG, parse_IG, FUN.VALUE = c(sem=NA_real_, dias=NA_real_, total_dias=NA_real_, sem_dec=NA_real_)))
df$ig_sem_dec   <- pars[, "sem_dec"]
df$total_dias   <- pars[, "total_dias"]

# TYPE 1..4
df$TYPE <- stringr::str_extract(as.character(df$TYPE_raw), "[1-4]")
df <- dplyr::filter(df, TYPE %in% type_levels)
df$TYPE <- factor(df$TYPE, levels = type_levels)

# Categoria de prematuridade
df$Categoria <- vapply(df$total_dias, cat_premat, character(1))
df$Categoria <- factor(df$Categoria, levels = cat_levels)

# Limpa NAs essenciais
df_clean <- df |> dplyr::filter(!is.na(ig_sem_dec), !is.na(Categoria))

# ---------------- Tema branco ----------------
theme_white_clean <- function(base_size = 13){
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_line(size = 0.25, colour = "grey85"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = "white", colour = "grey80"),
      strip.text       = element_text(face = "bold"),
      plot.title       = element_text(face = "bold")
    )
}

# ---------------- Linhas de corte (semanas decimais) ----------------
cuts_weeks <- c(27 + 6/7, 31 + 6/7, 33 + 6/7, 36 + 6/7)

# ---------------- 1) Histograma geral ----------------
p_all <- ggplot(df_clean, aes(x = ig_sem_dec, fill = Categoria)) +
  geom_histogram(binwidth = 0.5, position = "stack", colour = "white", alpha = 0.95) +
  scale_fill_manual(values = pal_fill, name = "Classificação") +
  scale_x_continuous(breaks = seq(22, 42, by = 1), limits = c(22, 42)) +
  labs(
    title = "Distribuição da Idade Gestacional por classificação de prematuridade",
    x = "Idade gestacional (semanas decimais)", y = "Frequência"
  ) +
  theme_white_clean()

# Linhas verticais dos cortes
for (v in cuts_weeks) p_all <- p_all + geom_vline(xintercept = v, linetype = "dashed", colour = "grey40")

out_all <- file.path(out_dir, paste0("hist_IG_por_classificacao_ALL_", ts, ".png"))
ggsave(out_all, p_all, width = 12, height = 6, dpi = 300, bg = "white")

# ---------------- 2) Histograma facetado por TYPE ----------------
p_facet <- ggplot(df_clean, aes(x = ig_sem_dec, fill = Categoria)) +
  geom_histogram(binwidth = 0.5, position = "stack", colour = "white", alpha = 0.95) +
  scale_fill_manual(values = pal_fill, name = "Classificação") +
  scale_x_continuous(breaks = seq(22, 42, by = 1), limits = c(22, 42)) +
  labs(
    title = "IG por classificação de prematuridade — facetado por TYPE",
    x = "Idade gestacional (semanas decimais)", y = "Frequência"
  ) +
  facet_wrap(~ TYPE, ncol = 2, labeller = labeller(TYPE = function(x) paste0("type ", x))) +
  theme_white_clean()

for (v in cuts_weeks) p_facet <- p_facet + geom_vline(xintercept = v, linetype = "dashed", colour = "grey40")

out_facet <- file.path(out_dir, paste0("hist_IG_por_classificacao_FACET_", ts, ".png"))
ggsave(out_facet, p_facet, width = 12, height = 8, dpi = 300, bg = "white")

# ---------------- 3) Um histograma por TYPE (arquivos separados) ----------------
for (ty in levels(df_clean$TYPE)) {
  dft <- dplyr::filter(df_clean, TYPE == ty)
  p_ty <- ggplot(dft, aes(x = ig_sem_dec, fill = Categoria)) +
    geom_histogram(binwidth = 0.5, position = "stack", colour = "white", alpha = 0.95) +
    scale_fill_manual(values = pal_fill, name = "Classificação") +
    scale_x_continuous(breaks = seq(22, 42, by = 1), limits = c(22, 42)) +
    labs(
      title = paste0("IG por classificação — type ", ty),
      x = "Idade gestacional (semanas decimais)", y = "Frequência"
    ) +
    theme_white_clean()
  for (v in cuts_weeks) p_ty <- p_ty + geom_vline(xintercept = v, linetype = "dashed", colour = "grey40")
  out_ty <- file.path(out_dir, paste0("hist_IG_por_classificacao_TYPE-", ty, "_", ts, ".png"))
  ggsave(out_ty, p_ty, width = 10, height = 6, dpi = 300, bg = "white")
  message("✔️  Gerado: ", out_ty)
}

# ---------------- (Opcional) Tabela de frequências por 0.5 semana ----------------
# útil para documentação e conferência
df_bins <- df_clean |>
  mutate(bin = cut(ig_sem_dec, breaks = seq(22, 42, by = 0.5), right = FALSE)) |>
  count(TYPE, Categoria, bin, name = "n") |>
  group_by(TYPE) |>
  mutate(pct_dentro_type = 100 * n / sum(n)) |>
  ungroup()

out_bins <- file.path(out_dir, paste0("tabela_bins_IG_", ts, ".xlsx"))
writexl::write_xlsx(list("freq_por_bin" = df_bins), path = out_bins)

cat("✅ Arquivos gerados:\n- ", out_all, "\n- ", out_facet, "\n- ", out_bins, "\n", sep = "")
