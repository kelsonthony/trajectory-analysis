# ================================================================
# APGAR por cluster — Histogramas COLORIDOS (1 gráfico por TYPE)
#  -> Sem violinos, densidade apenas linha (sem fill)
#  -> Fundo totalmente branco (panel + plot)
# Saída: apgar_hist_kde_TYPE-<n>_<ts>.png
# ================================================================

# ---------------- Pacotes ----------------
pkgs <- c("readxl","dplyr","tidyr","stringr","ggplot2")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------- Config ----------------
in_path <- "data/input/dados_gerais_APGAR.xlsx"
sheet_name <- readxl::excel_sheets(in_path)[1]
out_dir <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts <- format(Sys.time(), "%Y%m%d_%H%M")

# Paleta (fill claro + linha mais escura, colorblind-friendly)
pal_fill <- c(APGAR_1 = "#A6CEE3",  # azul claro
              APGAR_5 = "#F4A09C")  # salmão
pal_line <- c(APGAR_1 = "#1F78B4",  # azul mais escuro
              APGAR_5 = "#E76F51")  # salmão/laranja escuro

# ---------------- Leitura e preparação ----------------
df_raw <- readxl::read_excel(in_path, sheet = sheet_name)
names(df_raw) <- trimws(names(df_raw))

apgar1_col <- if ("APGAR_1" %in% names(df_raw)) "APGAR_1" else stop("Coluna APGAR_1 não encontrada.")
apgar5_col <- if ("APGAR_5" %in% names(df_raw)) "APGAR_5" else stop("Coluna APGAR_5 não encontrada.")
cluster_col <- if ("cluster" %in% names(df_raw)) "cluster" else {
  cand <- c("Cluster","CLUSTER","CU","Grupo","GRUPO","TYPE","Tipo","tipo")
  hit <- cand[cand %in% names(df_raw)]
  if (length(hit)) hit[1] else stop("Coluna de cluster não encontrada.")
}

df <- df_raw |>
  dplyr::transmute(
    cluster_raw = as.character(.data[[cluster_col]]),
    APGAR_1 = suppressWarnings(as.numeric(.data[[apgar1_col]])),
    APGAR_5 = suppressWarnings(as.numeric(.data[[apgar5_col]]))
  )

# normaliza cluster -> extrai 1..4 e rotula TYPE X
df$cluster_num <- stringr::str_extract(df$cluster_raw, "[1-4]")
df <- dplyr::filter(df, !is.na(cluster_num))
df$cluster_num <- factor(df$cluster_num, levels = c("1","2","3","4"))
df$cluster_lab <- factor(paste0("TYPE ", df$cluster_num), levels = paste0("TYPE ", c(1,2,3,4)))

# dados longos (só o necessário)
df_long <- df |>
  tidyr::pivot_longer(cols = c("APGAR_1","APGAR_5"),
                      names_to = "Apgar", values_to = "score") |>
  dplyr::filter(!is.na(score))

# ---------------- Tema branco e limpo ----------------
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

# ---------------- Geração por TYPE ----------------
for (cl in levels(df$cluster_num)) {
  lab <- paste0("TYPE ", cl)
  dfc <- dplyr::filter(df_long, cluster_lab == lab)

  p_hist <- ggplot(dfc, aes(x = score, fill = Apgar)) +
    # Histograma com alpha para ver a linha por cima
    geom_histogram(aes(y = ..density..), bins = 12, alpha = 0.55,
                   position = "identity", colour = NA) +
    # Densidade apenas com linha (sem fill) para não cobrir o histograma
    geom_density(aes(colour = Apgar), linewidth = 1.1, fill = NA) +
    facet_wrap(~ Apgar, ncol = 2) +
    scale_fill_manual(values = pal_fill) +
    scale_color_manual(values = pal_line) +
    scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
    labs(
      title = paste0("Distribuição dos Escores de Apgar — ", lab),
      x = "Escore", y = "Densidade"
    ) +
    theme_white_clean() +
    theme(legend.position = "none")

  out_file <- file.path(out_dir, paste0("apgar_hist_kde_", gsub(" ", "-", lab), "_", ts, ".png"))
  ggsave(out_file, p_hist, width = 10, height = 5.5, dpi = 300, bg = "white")

  message("✔️  Gerado (hist + densidade) para ", lab, ":\n - ", out_file)
}

cat("✅ Finalizado! Arquivos em: ", normalizePath(out_dir), "\n", sep = "")
