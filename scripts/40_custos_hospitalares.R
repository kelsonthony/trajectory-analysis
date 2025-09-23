# ================================================================
# Custos por trajetória
# Saída: T1..T4 com N, "Custo por RN (média±DP)", "Custo por RN (mediana [P25–P75])",
#        "Custo por paciente-dia (média±DP)" e "Custo por paciente-dia (mediana [P25–P75])"
# ================================================================

# ---------- 0) Pacotes ----------
pkgs <- c("readxl","dplyr","tidyr","stringr","readr","writexl","glue","janitor")
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------- 1) Parâmetros ----------
in_file <- "data/input/Banco_de_dados_final_0708_Custos.xlsx"
sheet   <- NULL              # se souber o nome da aba, defina aqui (ex.: "Custos"); NULL = 1ª aba
out_dir <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------- 2) Helpers ----------
norm_names <- function(x){
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^A-Za-z0-9]+","_", x)
  tolower(x)
}

# parse numérico robusto (aceita "R$ 12.345,67" ou "12,34" ou "12.34")
parse_brl_number <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  x_chr <- as.character(x)
  # tenta com vírgula como decimal
  n1 <- suppressWarnings(readr::parse_number(x_chr, locale = readr::locale(decimal_mark = ",")))
  # onde não funcionou, tenta ponto como decimal
  need <- is.na(n1)
  if (any(need)) {
    n2 <- suppressWarnings(readr::parse_number(x_chr[need], locale = readr::locale(decimal_mark = ".")))
    n1[need] <- n2
  }
  as.numeric(n1)
}

fmt_mean_sd <- function(x, digits = 2){
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return("—")
  m <- mean(x); s <- stats::sd(x)
  paste0(format(round(m, digits), nsmall = digits), " \u00B1 ",  # ±
         format(round(s, digits), nsmall = digits))
}

fmt_med_iqr <- function(x, digits = 2){
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return("—")
  q <- as.numeric(stats::quantile(x, probs = c(.5,.25,.75), na.rm = TRUE, names = FALSE))
  med <- q[1]; p25 <- q[2]; p75 <- q[3]
  paste0(
    format(round(med, digits), nsmall = digits), " [",
    format(round(p25, digits), nsmall = digits), "\u2013",  # – (en dash)
    format(round(p75, digits), nsmall = digits), "]"
  )
}

# ---------- 3) Ler base ----------
if (is.null(sheet)) {
  # pega a primeira aba
  sheets <- readxl::excel_sheets(in_file)
  if (length(sheets) == 0) stop("Planilha sem abas.")
  sheet <- sheets[1]
}
raw <- readxl::read_excel(in_file, sheet = sheet)
stopifnot(nrow(raw) > 0)

# ---------- 4) Detectar colunas ----------
nm <- names(raw); nm_norm <- norm_names(nm)

# Trajetória (cluster/tipo/traj) -> vira "1","2","3","4"
cand_traj <- c("traj","trajetoria","cluster","tipo","classe","grupo")
traj_idx <- which(nm_norm %in% cand_traj)
if (!length(traj_idx)) traj_idx <- which(grepl("(cluster|traj|tipo)", nm_norm))
if (!length(traj_idx)) stop("Não encontrei coluna de trajetória (ex.: 'cluster', 'traj', 'tipo').")
traj_col <- nm[traj_idx[1]]

# Custo total do RN
cand_custo <- c("custo_total","custo_total_rn","custo_rn_total","custo","custo_direto_total",
                "custo_internacao","custo_final","custo_total_internacao")
custo_idx <- which(nm_norm %in% cand_custo)
if (!length(custo_idx)) custo_idx <- which(grepl("custo", nm_norm))
if (!length(custo_idx)) stop("Não encontrei coluna de custo total por RN.")
custo_col <- nm[custo_idx[1]]

# Dias de internação (para custo por paciente-dia)
cand_los <- c("los_dias","los","dias","dias_total","dias_internacao","tempo_total_dias",
              "tempo_de_internacao_dias","tempo_internacao_dias")
los_idx <- which(nm_norm %in% cand_los)
if (!length(los_idx)) los_idx <- which(grepl("(los|dias).*intern", nm_norm))
los_col <- if (length(los_idx)) nm[los_idx[1]] else NULL

# ---------- 5) Preparar base de cálculo ----------
custos <- raw %>%
  dplyr::transmute(
    traj_raw = !!rlang::sym(traj_col),
    traj = readr::parse_number(as.character(traj_raw)),
    traj = dplyr::case_when(
      is.na(traj) & grepl("tipo\\s*1|^t\\s*1$|^t1$", tolower(as.character(traj_raw))) ~ 1,
      is.na(traj) & grepl("tipo\\s*2|^t\\s*2$|^t2$", tolower(as.character(traj_raw))) ~ 2,
      is.na(traj) & grepl("tipo\\s*3|^t\\s*3$|^t3$", tolower(as.character(traj_raw))) ~ 3,
      is.na(traj) & grepl("tipo\\s*4|^t\\s*4$|^t4$", tolower(as.character(traj_raw))) ~ 4,
      TRUE ~ traj
    ),
    custo_total = parse_brl_number(!!rlang::sym(custo_col)),
    los_dias = if (!is.null(los_col)) parse_brl_number(!!rlang::sym(los_col)) else NA_real_
  ) %>%
  dplyr::filter(!is.na(traj)) %>%
  dplyr::mutate(traj = as.integer(traj),
                T = paste0("T", traj),
                custo_dia = ifelse(is.finite(los_dias) & los_dias > 0, custo_total/los_dias, NA_real_))

# ---------- 6) Resumos por trajetória ----------
# Custo por RN
res_rn <- custos %>%
  dplyr::group_by(T) %>%
  dplyr::summarise(
    N = dplyr::n(),
    `Custo por RN (média±DP)`           = fmt_mean_sd(custo_total),
    `Custo por RN (mediana [P25–P75])`  = fmt_med_iqr(custo_total),
    .groups = "drop"
  )

# Custo por paciente-dia (usa apenas registros com custo_dia válido)
res_dia <- custos %>%
  dplyr::filter(is.finite(custo_dia)) %>%
  dplyr::group_by(T) %>%
  dplyr::summarise(
    `Custo por paciente-dia (média±DP)`          = fmt_mean_sd(custo_dia),
    `Custo por paciente-dia (mediana [P25–P75])` = fmt_med_iqr(custo_dia),
    .groups = "drop"
  )

# Junta tudo e ordena T1..T4
ord <- paste0("T", 1:4)
tabela_final <- res_rn %>%
  dplyr::left_join(res_dia, by = "T") %>%
  dplyr::mutate(T = factor(T, levels = ord)) %>%
  dplyr::arrange(T) %>%
  dplyr::rename(`Trajetória` = T)

# ---------- 7) Exibir e salvar ----------
print(tabela_final, n = nrow(tabela_final))

out_xlsx <- file.path(out_dir, "tabela_custos_por_trajetoria.xlsx")
writexl::write_xlsx(
  list(
    "Tabela para artigo" = tabela_final,
    "Base utilizada (limpa)" = custos
  ),
  out_xlsx
)
message("✅ Arquivo salvo em: ", out_xlsx)
