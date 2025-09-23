# ================================================================
# FREQUÊNCIA DE ALTA (ALT) E ÓBITO (OBI) POR TRAJETÓRIA
# Desfecho por RN = precedência na sequência: OBI > ALT > OUTRO
# ================================================================

# ---------- 0) Pacotes ----------
pkgs <- c("readr","readxl","dplyr","tidyr","ggplot2","stringr",
          "glue","scales","writexl","lubridate","stringi","forcats","binom")
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------- 1) Parâmetros ----------
csv_file  <- "data/input/Trajetoria_neonatos_cluster_k4_TRATE_20250826_2201.csv"
xlsx_file <- "data/input/Banco_de_dados_final_0708_Trajetoria.xlsx"
xlsx_sheet <- "Trajetoria"
out_dir <- "data/output"; if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------- 2) Helpers ----------
norm_names <- function(x){
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^A-Za-z0-9]+","_", x)
  tolower(x)
}
norm_state <- function(x){
  x <- toupper(trimws(as.character(x)))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  dplyr::recode(x,
    "UCO"="UCINCO","UCI"="UCINCO","UCA"="UCINCA","UTI"="UTIN",
    "ALTA"="ALT","ALTA H"="ALT","ALTAH"="ALT","ALTA_H"="ALT","ALTAHOSP"="ALT",
    "OBITO"="OBI","ÓBITO"="OBI","OBIT0"="OBI","OBI"="OBI",
    .default = x
  )
}
fmt_npct <- function(n, N) sprintf("%d (%.1f%%)", n, ifelse(N > 0, 100*n/N, 0))

ic_wilson <- function(sucessos, total){
  if (is.na(total) || total <= 0) {
    tibble(lower = NA_real_, upper = NA_real_)
  } else {
    binom::binom.confint(sucessos, total, methods = "wilson") |>
      as_tibble() |>
      dplyr::select(lower, upper)
  }
}

# ---------- 3) Obter seq_long e clusters ----------
get_seq_long_and_clusters <- function(){
  if (exists("seq_long") && exists("clusters")) {
    message("✓ Usando objetos 'seq_long' e 'clusters' já carregados na memória.")
    return(list(seq_long = seq_long, clusters = clusters))
  }
  if (file.exists(csv_file)) {
    raw <- readr::read_csv(csv_file, show_col_types = FALSE)
    message("✓ Lido CSV: ", csv_file)
  } else {
    raw <- readxl::read_excel(xlsx_file, sheet = xlsx_sheet)
    message("✓ Lido Excel: ", xlsx_file, " [", xlsx_sheet, "]")
  }
  stopifnot(nrow(raw) > 0)

  nm <- names(raw); nm_norm <- norm_names(nm)
  # ID
  id_idx <- which(nm_norm %in% c("id","id_rn","idrn","rn_id","id_do_rn","id_paciente","idrn_anon"))
  if (!length(id_idx)) id_idx <- which(grepl("\\bid\\b.*rn|rn.*\\bid\\b", nm_norm))
  if (!length(id_idx)) stop("Não encontrei coluna de ID.")
  id_col <- nm[id_idx[1]]
  # traj/cluster
  cl_idx <- which(grepl("(\\bcluster\\b|\\btraj\\w*\\b|\\btipo\\b)", nm_norm))
  if (!length(cl_idx)) stop("Não encontrei coluna de trajetória/cluster.")
  cl_col <- nm[cl_idx[1]]
  # semana/estado
  wk_idx <- which(nm_norm %in% c("week","semana","sem"))
  st_idx <- which(nm_norm %in% c("state","estado"))
  if (!length(wk_idx) || !length(st_idx))
    stop("Arquivo não contém 'week' e 'state' para montar seq_long.")

  week_col  <- nm[wk_idx[1]]
  state_col <- nm[st_idx[1]]

  seq_long <- raw |>
    dplyr::select(all_of(c(id_col, week_col, state_col))) |>
    dplyr::rename(id = all_of(id_col), week = all_of(week_col), state = all_of(state_col)) |>
    dplyr::mutate(week = as.integer(week),
                  state = norm_state(state)) |>
    dplyr::filter(!is.na(id), !is.na(week)) |>
    dplyr::arrange(id, week)

  clusters <- raw |>
    dplyr::select(all_of(c(id_col, cl_col))) |>
    dplyr::rename(id = all_of(id_col), lab = all_of(cl_col)) |>
    dplyr::mutate(traj = readr::parse_number(as.character(lab)),
                  traj = dplyr::case_when(
                    is.na(traj) & grepl("tipo\\s*1", tolower(lab)) ~ 1,
                    is.na(traj) & grepl("tipo\\s*2", tolower(lab)) ~ 2,
                    is.na(traj) & grepl("tipo\\s*3", tolower(lab)) ~ 3,
                    is.na(traj) & grepl("tipo\\s*4", tolower(lab)) ~ 4,
                    TRUE ~ traj
                  ),
                  traj = as.character(traj)) |>
    dplyr::select(id, traj) |>
    dplyr::distinct()

  list(seq_long = seq_long, clusters = clusters)
}

objs <- get_seq_long_and_clusters()
seq_long <- objs$seq_long
clusters <- objs$clusters

# ---------- 4) Desfecho por precedência (OBI > ALT > OUTRO) ----------
dat <- seq_long |>
  dplyr::mutate(state = norm_state(state)) |>
  dplyr::inner_join(clusters, by = "id")

outcome_per_id <- dat |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    any_obi = any(state == "OBI", na.rm = TRUE),
    any_alt = any(state == "ALT", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    desfecho = dplyr::case_when(
      any_obi ~ "OBI",
      any_alt ~ "ALT",
      TRUE    ~ "OUTRO"
    )
  )

# ---------- 5) Frequências por trajetória ----------
base <- outcome_per_id |>
  dplyr::inner_join(clusters, by = "id")

# (A) Denominador = todos da trajetória
denom_all <- base |>
  dplyr::distinct(id, traj) |>
  dplyr::count(traj, name = "n_traj_total")

cont_all <- base |>
  dplyr::filter(desfecho %in% c("ALT","OBI")) |>
  dplyr::count(traj, desfecho, name = "n") |>
  tidyr::complete(traj = unique(denom_all$traj),
                  desfecho = c("ALT","OBI"),
                  fill = list(n = 0))

freq_A <- cont_all |>
  dplyr::left_join(denom_all, by = "traj") |>
  dplyr::mutate(pct = ifelse(n_traj_total > 0, 100*n/n_traj_total, 0),
                tipo = "A_todos_na_traj") |>
  dplyr::arrange(as.numeric(traj), desfecho)

# (B) Denominador = apenas (ALT + OBI)
denom_def <- cont_all |>
  dplyr::group_by(traj) |>
  dplyr::summarise(n_def = sum(n), .groups = "drop")

freq_B <- cont_all |>
  dplyr::left_join(denom_def, by = "traj") |>
  dplyr::mutate(pct = ifelse(n_def > 0, 100*n/n_def, 0),
                tipo = "B_somente_definidos") |>
  dplyr::arrange(as.numeric(traj), desfecho)

# ---------- 6) IC95% (Wilson) para ÓBITO ----------
obi_A <- freq_A |>
  dplyr::filter(desfecho == "OBI") |>
  dplyr::rowwise() |>
  dplyr::mutate(ci = list(ic_wilson(n, n_traj_total))) |>
  tidyr::unnest_wider(ci) |>
  dplyr::mutate(lcl = round(lower*100, 1),
                ucl = round(upper*100, 1)) |>
  dplyr::ungroup() |>
  dplyr::select(traj, n_obito = n, N = n_traj_total, pct_obito = pct, lcl, ucl)

obi_B <- freq_B |>
  dplyr::filter(desfecho == "OBI") |>
  dplyr::rowwise() |>
  dplyr::mutate(ci = list(ic_wilson(n, n_def))) |>
  tidyr::unnest_wider(ci) |>
  dplyr::mutate(lcl = round(lower*100, 1),
                ucl = round(upper*100, 1)) |>
  dplyr::ungroup() |>
  dplyr::select(traj, n_obito = n, N = n_def, pct_obito = pct, lcl, ucl)

# ---------- 7) Tabelas finais ----------
tblA_wide <- freq_A |>
  dplyr::mutate(cell = fmt_npct(n, n_traj_total)) |>
  dplyr::select(traj, desfecho, cell, n_traj_total) |>
  tidyr::pivot_wider(names_from = desfecho, values_from = cell) |>
  dplyr::rename(`Total (n)` = n_traj_total) |>
  dplyr::arrange(as.numeric(traj))

tblB_wide <- freq_B |>
  dplyr::mutate(cell = fmt_npct(n, n_def)) |>
  dplyr::select(traj, desfecho, cell, n_def) |>
  tidyr::pivot_wider(names_from = desfecho, values_from = cell) |>
  dplyr::rename(`Definidos (ALT+OBI) (n)` = n_def) |>
  dplyr::arrange(as.numeric(traj))

print(tblA_wide, n = nrow(tblA_wide))
print(tblB_wide, n = nrow(tblB_wide))

# ---------- 8) Gráfico (A) ----------
plot_A <- freq_A |>
  dplyr::mutate(traj = factor(traj, levels = sort(unique(traj))),
                desfecho = factor(desfecho, levels = c("ALT","OBI"))) |>
  ggplot(aes(x = traj, y = pct, fill = desfecho)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_stack(vjust = 0.5), size = 3.3, color = "black") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(x = "Trajetória", y = "Percentual dentro da trajetória",
       fill = "Desfecho", title = "Frequência de ALT e OBI por trajetória (denominador = todos)") +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "freq_alta_obito_por_traj.png"),
       plot_A, width = 7, height = 5, dpi = 300, bg = "white")

# ---------- 9) Exportar ----------
writexl::write_xlsx(
  list(
    "A_dentro_de_todos_traj (wide)" = tblA_wide,
    "B_somente_definidos (wide)"   = tblB_wide,
    "IC95_Wilson_OBI_A"            = obi_A,
    "IC95_Wilson_OBI_B"            = obi_B
  ),
  file.path(out_dir, "frequencia_alta_obito_por_traj.xlsx")
)

message("✅ Concluído. Arquivos salvos em: ", out_dir)
