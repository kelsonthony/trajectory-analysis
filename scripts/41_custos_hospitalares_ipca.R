# ================================================================
# Custos por trajetória ajustados pelo IPCA (preços de 2024) — CORRIGIDO
# - Garante cálculo de los_dias (Alta/Óbito vs Admissão) ANTES do custo_dia_2024
# ================================================================

suppressPackageStartupMessages({
  pkgs <- c("readxl","dplyr","tidyr","stringr","readr","writexl","janitor",
            "stringi","purrr","lubridate","scales","sidrar")
  new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if (length(new)) install.packages(new, dependencies = TRUE)
  lapply(pkgs, library, character.only = TRUE)
})

# ---------------- paths / parâmetros ----------------
in_file <- "data/input/Banco_de_dados_final_0708_Custos.xlsx"
sheet   <- readxl::excel_sheets(in_file)[1]
out_dir <- "data/output"; if (!dir.exists(out_dir)) dir.create(out_dir, TRUE)

# 7 componentes da planilha
comp_cols <- c("Diárias_UTI","Exames","Material","Tratamentos",
               "Consultas","Ass.Fisio","Serv/Trat")

# mês/ano de referência do IPCA (trazer para 2024)
ipca_ref_date <- as.Date("2024-12-01")   # troque se quiser (ex.: "2024-06-01")

# ---------------- helpers ----------------
norm_names <- function(x){
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^A-Za-z0-9]+","_", x)
  tolower(x)
}
to_num_strict <- function(x) {
  x <- gsub("[^0-9,.-]", "", as.character(x))
  has_comma <- grepl(",", x, fixed = TRUE)
  if (any(has_comma)) {
    x[has_comma] <- gsub("\\.", "", x[has_comma])  # milhar
    x[has_comma] <- gsub(",", ".", x[has_comma])   # decimal
  }
  suppressWarnings(as.numeric(x))
}
fmt_mean_sd <- function(x, digits=2){
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (!length(x)) return("—")
  paste0(format(round(mean(x),digits), nsmall=digits), " \u00B1 ",
         format(round(stats::sd(x),digits), nsmall=digits))
}
fmt_med_iqr <- function(x, digits=2){
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (!length(x)) return("—")
  q <- as.numeric(stats::quantile(x, c(.5,.25,.75), na.rm=TRUE, names=FALSE))
  paste0(format(round(q[1],digits), nsmall=digits), " [",
         format(round(q[2],digits), nsmall=digits), "\u2013",
         format(round(q[3],digits), nsmall=digits), "]")
}
as_date_guess <- function(v){
  if (inherits(v,"Date")) return(v)
  if (inherits(v,c("POSIXct","POSIXt"))) return(as.Date(v))
  if (is.numeric(v)) return(as.Date(v, origin="1899-12-30"))
  x <- as.character(v)
  d <- suppressWarnings(lubridate::dmy(x))
  d[is.na(d)] <- suppressWarnings(lubridate::ymd(x[is.na(d)]))
  d[is.na(d)] <- suppressWarnings(lubridate::mdy(x[is.na(d)]))
  d
}
brl <- scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal_mark = ",")

# ---------------- ler planilha e preparar base (nominal) ----------------
raw <- readxl::read_excel(in_file, sheet = sheet)
stopifnot(nrow(raw) > 0)

nm <- names(raw); nn <- norm_names(nm)

# detectar trajetória
traj_col <- nm[which(nn %in% c("traj","trajetoria","cluster","tipo","classe","grupo"))[1]]
if (is.na(traj_col)) traj_col <- nm[grep("(cluster|traj|tipo)", nn, perl=TRUE)[1]]
stopifnot(!is.na(traj_col))

# detectar datas (para data de referência e LOS)
data_adm_col   <- nm[grep("^data_?adm|\\badm\\b", nn)][1]
data_alta_col  <- nm[grep("^data_?alta|\\balta\\b", nn)][1]
data_obito_col <- nm[grep("^data_?obito|obito|óbito", nn)][1]

# garantir componentes
miss <- setdiff(comp_cols, names(raw))
if (length(miss)) stop(paste0("Colunas de componente ausentes: ", paste(miss, collapse=", ")))

base <- raw %>%
  mutate(
    traj_raw = .data[[traj_col]],
    traj = readr::parse_number(as.character(traj_raw)),
    traj = dplyr::case_when(
      is.na(traj) & grepl("tipo\\s*1|^t\\s*1$|^t1$", tolower(as.character(traj_raw))) ~ 1,
      is.na(traj) & grepl("tipo\\s*2|^t\\s*2$|^t2$", tolower(as.character(traj_raw))) ~ 2,
      is.na(traj) & grepl("tipo\\s*3|^t\\s*3$|^t3$", tolower(as.character(traj_raw))) ~ 3,
      is.na(traj) & grepl("tipo\\s*4|^t\\s*4$|^t4$", tolower(as.character(traj_raw))) ~ 4,
      TRUE ~ traj
    ),
    Data_adm   = if (!is.na(data_adm_col))   as_date_guess(.data[[data_adm_col]])   else as.Date(NA),
    Data_alta  = if (!is.na(data_alta_col))  as_date_guess(.data[[data_alta_col]])  else as.Date(NA),
    Data_obito = if (!is.na(data_obito_col)) as_date_guess(.data[[data_obito_col]]) else as.Date(NA)
  ) %>%
  filter(!is.na(traj)) %>%
  mutate(across(all_of(comp_cols), to_num_strict)) %>%
  rowwise() %>%
  mutate(custo_total = sum(c_across(all_of(comp_cols)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    # data usada para IPCA e para LOS
    data_ref_ipca = dplyr::coalesce(Data_alta, Data_obito, Data_adm),
    ym_ref        = as.Date(format(lubridate::floor_date(data_ref_ipca, "month"), "%Y-%m-01")),
    # LOS em dias (final - adm). Se não tiver alta nem óbito, fica NA.
    data_final_los = dplyr::coalesce(Data_alta, Data_obito),
    los_dias = as.numeric(data_final_los - Data_adm),
    los_dias = dplyr::if_else(is.finite(los_dias) & los_dias > 0, los_dias, NA_real_),
    T = paste0("T", as.integer(traj)),
    custo_dia = dplyr::if_else(is.finite(los_dias) & los_dias > 0, custo_total/los_dias, NA_real_)
  )

# ---------------- IPCA mensal (SIDRA) ----------------
ipca_raw <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")
nm_ip <- names(ipca_raw)
col_codigo <- nm_ip[grepl("M[êe]s \\(C[óo]digo\\)", nm_ip)]
if (length(col_codigo) == 0) col_codigo <- nm_ip[grepl("Mes \\(Codigo\\)|Mes \\(C[oó]digo\\)", nm_ip)]
stopifnot(length(col_codigo) > 0)

ipca <- ipca_raw %>%
  transmute(
    ym = lubridate::ymd(paste0(!!sym(col_codigo), "01")),
    indice = as.numeric(.data[["Valor"]])
  ) %>%
  arrange(ym) %>%
  distinct(ym, .keep_all = TRUE)

# valor de referência em 2024
ipca_2024 <- ipca %>% filter(format(ym, "%Y") == "2024") %>% arrange(ym)
stopifnot(nrow(ipca_2024) > 0)
ref_used <- if (ipca_ref_date %in% ipca_2024$ym) ipca_ref_date else max(ipca_2024$ym)
ipca_ref_val <- ipca$indice[ipca$ym == ref_used][1]

# ---------------- aplicar fator de atualização ----------------
base_adj <- base %>%
  left_join(ipca, by = c("ym_ref" = "ym")) %>%
  mutate(
    ipca_mes = indice,
    fator_ipca_2024 = ifelse(is.finite(ipca_mes) & ipca_mes > 0, ipca_ref_val / ipca_mes, NA_real_),
    custo_total_2024 = ifelse(is.finite(fator_ipca_2024), custo_total * fator_ipca_2024, NA_real_),
    custo_dia_2024   = ifelse(is.finite(fator_ipca_2024) & is.finite(los_dias) & los_dias > 0,
                              custo_total_2024/los_dias, NA_real_)
  )

# ---------------- resumos (preços de 2024) ----------------
res_rn_2024 <- base_adj %>%
  group_by(T) %>%
  summarise(
    N = n(),
    `Custo por RN (média±DP)`          = fmt_mean_sd(custo_total_2024),
    `Custo por RN (mediana [P25–P75])` = fmt_med_iqr(custo_total_2024),
    .groups = "drop"
  )

res_dia_2024 <- base_adj %>%
  filter(is.finite(custo_dia_2024)) %>%
  group_by(T) %>%
  summarise(
    `Custo por paciente-dia (média±DP)`          = fmt_mean_sd(custo_dia_2024),
    `Custo por paciente-dia (mediana [P25–P75])` = fmt_med_iqr(custo_dia_2024),
    .groups = "drop"
  )

tabela_2024 <- res_rn_2024 %>%
  left_join(res_dia_2024, by = "T") %>%
  mutate(T = factor(T, levels = paste0("T",1:4))) %>%
  arrange(T) %>%
  select(`Trajetória` = T, N,
         `Custo por RN (média±DP)`,
         `Custo por RN (mediana [P25–P75])`,
         `Custo por paciente-dia (média±DP)`,
         `Custo por paciente-dia (mediana [P25–P75])`)

# (opcional) versão formatada em R$
tab_2024_brl <- base_adj %>%
  group_by(T) %>%
  summarise(
    N = n(),
    mean_total = mean(custo_total_2024, na.rm=TRUE),
    sd_total   = sd(custo_total_2024, na.rm=TRUE),
    med_total  = median(custo_total_2024, na.rm=TRUE),
    p25_total  = quantile(custo_total_2024, 0.25, na.rm=TRUE),
    p75_total  = quantile(custo_total_2024, 0.75, na.rm=TRUE),
    mean_day   = mean(custo_dia_2024, na.rm=TRUE),
    sd_day     = sd(custo_dia_2024, na.rm=TRUE),
    med_day    = median(custo_dia_2024, na.rm=TRUE),
    p25_day    = quantile(custo_dia_2024, 0.25, na.rm=TRUE),
    p75_day    = quantile(custo_dia_2024, 0.75, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Custo por RN (média±DP)`          = paste0(brl(mean_total), " ± ", brl(sd_total)),
    `Custo por RN (mediana [P25–P75])` = paste0(brl(med_total), " [", brl(p25_total), "–", brl(p75_total), "]"),
    `Custo por paciente-dia (média±DP)`          = ifelse(is.finite(mean_day), paste0(brl(mean_day), " ± ", brl(sd_day)), "—"),
    `Custo por paciente-dia (mediana [P25–P75])` = ifelse(is.finite(med_day),  paste0(brl(med_day), " [", brl(p25_day), "–", brl(p75_day), "]"), "—")
  ) %>%
  mutate(T = factor(T, levels = paste0("T",1:4))) %>%
  arrange(T) %>%
  select(`Trajetória` = T, N,
         `Custo por RN (média±DP)`,
         `Custo por RN (mediana [P25–P75])`,
         `Custo por paciente-dia (média±DP)`,
         `Custo por paciente-dia (mediana [P25–P75])`)

# ---------------- salvar ----------------
out_xlsx <- file.path(out_dir, "tabela_custos_por_trajetoria_IPCA2024.xlsx")
writexl::write_xlsx(
  list(
    "Custos (preços de 2024)"      = tabela_2024,
    "Custos (R$ 2024 formatado)"   = tab_2024_brl,
    "Base ajustada (linha a linha)"= base_adj %>%
      select(traj, `Trajetória`=T,
             Data_adm, Data_alta, Data_obito, data_ref_ipca, ym_ref,
             ipca_mes, fator_ipca_2024,
             custo_total, custo_total_2024,
             los_dias, custo_dia, custo_dia_2024,
             all_of(comp_cols))
  ),
  out_xlsx
)
message("✅ Arquivo salvo em: ", out_xlsx, " | Referência IPCA: ", format(ref_used, "%Y-%m"))
