# ================================================================
# % de permanência por estado de internação (por trajetória)
# - Lê Banco_de_dados_final_0708_Trajetoria.xlsx (aba "Trajetoria")
# - Constrói seq_long (id–semana–state) e clusters (id–traj)
# - Figura, Tabela wide e Parágrafos descritivos por trajetória
# ================================================================

# ---------- 0) Pacotes: checar/instalar ----------
pkgs <- c("readxl","dplyr","tidyr","ggplot2","forcats","stringr",
          "glue","readr","scales","writexl","lubridate","janitor","stringi")
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------- 1) Parâmetros ----------
in_file <- "data/input/Banco_de_dados_final_0708_Trajetoria.xlsx"
sheet   <- "Trajetoria"
out_dir <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

weeks_max  <- 30L
states_all <- c("UTIN","UCINCO","UCINCA","ENF","ALTA","OBITO")
states_bed <- c("UTIN","UCINCO","UCINCA","ENF")

# ---------- 2) Helpers ----------
norm_names <- function(x) {
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # remove acentos
  x <- gsub("[^A-Za-z0-9]+","_", x)                  # pontuação -> _
  tolower(x)
}

safe_as_datetime <- function(x) {
  if (inherits(x, c("POSIXct","POSIXt"))) return(as.POSIXct(x, tz = "UTC"))
  if (inherits(x, "Date"))                return(lubridate::as_datetime(x))
  if (is.numeric(x)) {
    # Datas Excel (dias desde 1899-12-30)
    if (all(is.finite(x) & x > 10000 & x < 100000, na.rm = TRUE)) {
      return(lubridate::as_datetime(as.Date(x, origin = "1899-12-30")))
    } else {
      return(lubridate::as_datetime(as.POSIXct(x, origin = "1970-01-01", tz = "UTC")))
    }
  }
  suppressWarnings(lubridate::as_datetime(
    lubridate::parse_date_time(x, orders = c("Ymd","dmY","mdY","Y-m-d","d/m/Y","m/d/Y"))
  ))
}

first_that_exists <- function(cands, pool) {
  out <- cands[cands %in% pool]
  if (length(out)) out[[1]] else NA_character_
}

# ---------- 3) Ler Excel e detectar colunas ----------
message("ℹ️  Construindo 'seq_long' e 'clusters' a partir de ", in_file)
raw <- readxl::read_excel(in_file, sheet = sheet)

# Detecta ID de forma robusta
nm_norm <- norm_names(names(raw))
id_idx <- which(nm_norm %in% c("id_rn","id","idrn","rn_id","id_do_rn","id_paciente"))
if (length(id_idx) == 0) id_idx <- which(grepl("\\bid\\b.*rn|rn.*\\bid\\b", nm_norm))
if (length(id_idx) == 0) stop("Não encontrei coluna de ID (ex.: 'ID RN').")
id_col <- names(raw)[id_idx[1]]
message("✓ Coluna de ID detectada: '", id_col, "'")

# Detecta coluna “trajetória” (cluster/tipo/traj)
cl_idx <- which(grepl("(\\bcluster\\b|\\btraj\\w*\\b|\\btipo\\b)", nm_norm))
if (length(cl_idx) == 0) stop("Não encontrei coluna de cluster/trajectória (ex.: 'Cluster').")
clust_col <- names(raw)[cl_idx[1]]
message("✓ Coluna de trajetória detectada: '", clust_col, "'")

# ---------- 4) Montar events_long: (id, unidade, data) ----------
cols_utin   <- intersect(c("UTIN 1","UTIN 2","UTIN 3"), names(raw))
cols_ucinco <- intersect(c("UCINCO 1","UCINCO 2","UCINCO 3"), names(raw))
cols_ucinca <- intersect(c("UCINCA 1","UCINCA 2","UCINCA 3"), names(raw))
col_enf     <- intersect("ENF/ALCON", names(raw))
col_alta    <- intersect("D.ALTA.H",  names(raw))
col_obito   <- intersect(c("D.ÓBITO","D.OBITO","D.ÓBITO "), names(raw))

stack_cols <- function(df, cols, unit) {
  if (!length(cols)) return(NULL)
  df |>
    dplyr::select(all_of(c(id_col, cols))) |>
    tidyr::pivot_longer(cols = all_of(cols), names_to = "colname", values_to = "date") |>
    dplyr::mutate(unidade = unit)
}

ev_utin   <- stack_cols(raw, cols_utin,   "UTIN")
ev_ucinco <- stack_cols(raw, cols_ucinco, "UCINCO")
ev_ucinca <- stack_cols(raw, cols_ucinca, "UCINCA")
ev_enf    <- stack_cols(raw, col_enf,     "ENF")

ev_alta <- if (length(col_alta)) {
  raw |>
    dplyr::select(all_of(c(id_col, col_alta))) |>
    dplyr::rename(date = all_of(col_alta)) |>
    dplyr::mutate(colname = col_alta, unidade = "ALTA")
} else NULL

ev_obito <- if (length(col_obito)) {
  c0 <- col_obito[1]
  raw |>
    dplyr::select(all_of(c(id_col, c0))) |>
    dplyr::rename(date = all_of(c0)) |>
    dplyr::mutate(colname = c0, unidade = "OBITO")
} else NULL

events_long <- dplyr::bind_rows(ev_utin, ev_ucinco, ev_ucinca, ev_enf, ev_alta, ev_obito) |>
  dplyr::filter(!is.na(date)) |>
  dplyr::mutate(date = safe_as_datetime(date)) |>
  dplyr::arrange(.data[[id_col]], date)

if (nrow(events_long) == 0) stop("Sem eventos datados para montar a sequência.")

# ---------- 5) clusters (id, traj) a partir da coluna de trajetória ----------
clusters <- raw |>
  dplyr::select(all_of(c(id_col, clust_col))) |>
  dplyr::rename(id = all_of(id_col), lab = all_of(clust_col)) |>
  dplyr::mutate(
    lab  = as.character(lab),
    traj = readr::parse_number(lab),
    traj = dplyr::case_when(
      is.na(traj) & grepl("tipo\\s*1", tolower(lab)) ~ 1,
      is.na(traj) & grepl("tipo\\s*2", tolower(lab)) ~ 2,
      is.na(traj) & grepl("tipo\\s*3", tolower(lab)) ~ 3,
      is.na(traj) & grepl("tipo\\s*4", tolower(lab)) ~ 4,
      TRUE ~ traj
    ),
    traj = as.character(traj)  # <- evita problemas de fator
  ) |>
  dplyr::select(id, traj) |>
  dplyr::distinct()

if (any(is.na(clusters$traj))) {
  stop("Valores de trajetória não reconhecidos (ex.: não são 'Tipo 1..4' nem números).")
}

# níveis de trajetórias realmente presentes (ex.: pode haver só 1..3)
traj_levels <- intersect(c("1","2","3","4"), sort(unique(clusters$traj)))

# ---------- 6) Construir seq_long (id–semana–state) ----------
seq_list <- split(events_long, events_long[[id_col]], drop = TRUE)

seq_long <- dplyr::bind_rows(lapply(seq_list, function(df_i) {
  df_i <- dplyr::arrange(df_i, date)
  t0 <- df_i$date[1]
  mids <- t0 + (0:(weeks_max-1))*7 + lubridate::days(3)
  idx <- findInterval(mids, df_i$date)
  st  <- ifelse(idx == 0, NA_character_, df_i$unidade[pmax(idx,1)])
  tibble(
    id    = df_i[[id_col]][1],
    week  = seq_len(weeks_max),
    state = st
  )
})) |>
  dplyr::mutate(
    state = toupper(trimws(as.character(state))),
    state = dplyr::recode(state,
      "ÓBITO"="OBITO","ENFERMARIA"="ENF","ENF/ALCON"="ENF",
      "UCI"="UCINCO","UCO"="UCINCO","UCA"="UCINCA","UTI"="UTIN"
    ),
    state = factor(state, levels = states_all)
  )

# ---------- 7) Preparar base final e tamanhos ----------
dat <- seq_long |>
  dplyr::inner_join(clusters, by = "id") |>
  dplyr::mutate(traj = factor(traj, levels = traj_levels))

traj_sizes <- dat |>
  dplyr::distinct(id, traj) |>
  dplyr::count(traj, name = "n_id") |>
  dplyr::mutate(p_id = 100 * n_id / sum(n_id),
                lab_leg = glue::glue("Trajetória {as.character(traj)} (n={n_id}, {scales::number(p_id, accuracy=0.1)}%)"))

# ---------- 8) % de permanência por estado ----------
occ_pct <- dat |>
  dplyr::count(traj, state, name = "n_weeks") |>
  dplyr::group_by(traj) |>
  dplyr::mutate(total_weeks = sum(n_weeks),
                pct = 100 * n_weeks / total_weeks) |>
  dplyr::ungroup()

occ_pct_bed <- occ_pct |> dplyr::filter(state %in% states_bed)

# ---------- 9) Figura ----------
occ_plot <- occ_pct_bed |>
  dplyr::left_join(traj_sizes |> dplyr::select(traj, lab_leg), by = "traj") |>
  dplyr::mutate(state = forcats::fct_relevel(state, states_bed))

p <- ggplot(occ_plot, aes(x = state, y = pct, fill = lab_leg)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.85) +
  geom_text(aes(label = sprintf("%.1f", pct)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100),
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = "Estados de internação",
       y = "Percentual de permanência (%)",
       fill = "Trajetória",
       title = "Percentual de permanência por estado de internação, por trajetória") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank())

print(p)
ggplot2::ggsave(file.path(out_dir, "perc_permanencia_por_traj.png"),
                p, width = 10, height = 6, dpi = 300)

# ---------- 10) Tabela wide ----------
occ_pct_wide <- occ_pct_bed |>
  dplyr::mutate(traj_chr = as.character(traj)) |>
  dplyr::select(traj_chr, state, pct) |>
  dplyr::mutate(pct = round(pct, 1)) |>
  tidyr::pivot_wider(names_from = traj_chr, values_from = pct,
                     names_prefix = "Traj_") |>
  dplyr::arrange(match(state, states_bed))

print(occ_pct_wide, n = nrow(occ_pct_wide))
writexl::write_xlsx(list("Perc_permanencia_por_estado" = occ_pct_wide),
                    file.path(out_dir, "perc_permanencia_por_estado.xlsx"))

# ---------- 11) Parágrafos descritivos ----------
occ_pct_all <- occ_pct  # (inclui ALTA/OBITO se presentes)

make_para <- function(traj_id, tbl, sizes_tbl) {
  traj_id <- as.character(traj_id)  # <- padroniza para evitar Ops.factor()
  sizes  <- sizes_tbl |> dplyr::filter(as.character(traj) == traj_id)
  resumo <- tbl       |> dplyr::filter(as.character(traj) == traj_id)

  cab <- glue::glue("Quanto à descrição da Trajetória {traj_id} (n={sizes$n_id}, ",
                    "{scales::number(sizes$p_id, accuracy=0.1)}% da amostra), ")

  part_leitos <- resumo |>
    dplyr::filter(state %in% states_bed) |>
    dplyr::arrange(match(state, states_bed)) |>
    dplyr::transmute(txt = glue::glue("{state}: {scales::number(pct, accuracy = 0.1)}%")) |>
    dplyr::pull(txt) |>
    paste(collapse = "; ")

  part_outcomes <- resumo |>
    dplyr::filter(state %in% c("ALTA","OBITO")) |>
    dplyr::arrange(match(state, c("ALTA","OBITO"))) |>
    dplyr::transmute(txt = glue::glue("{state}: {scales::number(pct, accuracy = 0.1)}%")) |>
    dplyr::pull(txt) |>
    paste(collapse = "; ")

  if (nzchar(part_outcomes)) {
    glue::glue("{cab}observa-se a seguinte distribuição do tempo de internação: {part_leitos}; ",
               "e, quanto aos desfechos, {part_outcomes}.")
  } else {
    glue::glue("{cab}observa-se a seguinte distribuição do tempo de internação: {part_leitos}.")
  }
}

# usa apenas trajetórias realmente presentes
descricoes <- tibble(
  traj = traj_levels,
  paragrafo = vapply(traj_levels,
                     function(t) make_para(as.character(t), occ_pct_all, traj_sizes),
                     FUN.VALUE = character(1))
)

cat(paste0(descricoes$paragrafo, collapse = "\n\n"))
writexl::write_xlsx(list("Descricoes_trajetorias" = descricoes),
                    file.path(out_dir, "descricoes_trajetorias.xlsx"))

message("✅ Concluído. Arquivos salvos em: ", out_dir)
