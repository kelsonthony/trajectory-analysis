# ================================================================
# APGAR por cluster — versão COLORIDA (1 par de figuras por TYPE)
# Saídas por cluster:
#   1) apgar_violin_box_TYPE-<n>_<ts>.png
#   2) apgar_hist_kde_TYPE-<n>_<ts>.png
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

# Paleta (colorblind-friendly)
pal <- c(APGAR_1 = "#2E86AB",  # azul
         APGAR_5 = "#E76F51")  # laranja

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

# dados longos
df_long <- df |>
  tidyr::pivot_longer(cols = c("APGAR_1","APGAR_5"),
                      names_to = "Apgar", values_to = "score") |>
  dplyr::filter(!is.na(score))

# ---------------- Tema ----------------
theme_clean <- function(base_size = 13){
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major = element_line(size = 0.25, colour = "grey85"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}

# ---------------- Geração por cluster ----------------
for (cl in levels(df$cluster_num)) {
  lab <- paste0("TYPE ", cl)
  dfc <- dplyr::filter(df_long, cluster_lab == lab)

  # 1) Violino + box + pontos (cores por APGAR)
  p1 <- ggplot(dfc, aes(x = Apgar, y = score, fill = Apgar, colour = Apgar)) +
    geom_violin(alpha = 0.35, linewidth = 0.6) +
    geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", alpha = 0.8, colour = "black") +
    geom_jitter(width = 0.10, height = 0, alpha = 0.5, size = 1.2, colour = "black") +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
    labs(title = paste0("Apgar 1 e 5 — ", lab), x = NULL, y = "Escore Apgar") +
    guides(colour = "none", fill = "none") +
    theme_clean()

  out1 <- file.path(out_dir, paste0("apgar_violin_box_", gsub(" ", "-", lab), "_", ts, ".png"))
  ggsave(out1, p1, width = 7.5, height = 6, dpi = 300)

  # 2) Histogramas + densidade (cores por APGAR; dois painéis)
  p2 <- ggplot(dfc, aes(x = score, fill = Apgar, colour = Apgar)) +
    geom_histogram(aes(y = ..density..), bins = 12, alpha = 0.5, position = "identity") +
    geom_density(linewidth = 1) +
    facet_wrap(~ Apgar, ncol = 2) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
    labs(title = paste0("Histograma com densidade — ", lab),
         x = "Escore Apgar", y = "Densidade") +
    theme_clean() +
    theme(legend.position = "none")

  out2 <- file.path(out_dir, paste0("apgar_hist_kde_", gsub(" ", "-", lab), "_", ts, ".png"))
  ggsave(out2, p2, width = 10, height = 5.5, dpi = 300)

  message("✔️  Gerados (coloridos) para ", lab, ":\n - ", out1, "\n - ", out2)
}

cat("✅ Finalizado! Arquivos coloridos em: ", normalizePath(out_dir), "\n", sep = "")
