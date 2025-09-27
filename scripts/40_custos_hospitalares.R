# ================================================================
# CUSTOS POR TRAJETÓRIA — soma de componentes + custo/dia
# (corrigido: construção das colunas formatadas sem separar strings)
# ================================================================

suppressPackageStartupMessages({
  pkgs <- c("readxl","dplyr","tidyr","stringr","readr",
            "writexl","janitor","stringi","purrr","lubridate","scales")
  new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if (length(new)) install.packages(new, dependencies = TRUE)
  lapply(pkgs, library, character.only = TRUE)
})

# ---------------- paths ----------------
in_file <- "data/input/Banco_de_dados_final_0708_Custos.xlsx"
sheet   <- readxl::excel_sheets(in_file)[1]
out_dir <- "data/output"; if (!dir.exists(out_dir)) dir.create(out_dir, TRUE)

# (se quiser forçar, preencha exatamente estes nomes)
# component_cols_override <- c("Diárias_UTI","Exames","Material","Tratamentos",
#                              "Consultas","Ass.Fisio","Serv/Trat")
component_cols_override <- NULL

# componentes padrão (os 7 que você tem na planilha)
comp_cols_default <- c("Diárias_UTI","Exames","Material","Tratamentos",
                       "Consultas","Ass.Fisio","Serv/Trat")

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
    x[has_comma] <- gsub("\\.", "", x[has_comma])  # tira milhar
    x[has_comma] <- gsub(",", ".", x[has_comma])   # vírgula -> ponto
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
brl <- scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")

# ---------------- ler ----------------
raw <- readxl::read_excel(in_file, sheet = sheet)
stopifnot(nrow(raw) > 0)

nm <- names(raw); nn <- norm_names(nm)

# detectar trajetória
traj_col <- nm[which(nn %in% c("traj","trajetoria","cluster","tipo","classe","grupo"))[1]]
if (is.na(traj_col)) traj_col <- nm[grep("(cluster|traj|tipo)", nn, perl=TRUE)[1]]
stopifnot(!is.na(traj_col))

# detectar LOS direto e/ou datas para fallback
los_col <- nm[which(nn %in% c("los_dias","los","dias","dias_total","dias_internacao",
                              "tempo_total_dias","tempo_de_internacao_dias","tempo_internacao_dias"))[1]]
data_adm_col   <- nm[grep("^data_?adm|\\badm\\b", nn)][1]
data_alta_col  <- nm[grep("^data_?alta|\\balta\\b", nn)][1]
data_obito_col <- nm[grep("^data_?obito|obito|óbito", nn)][1]

# componentes a usar
comp_cols <- if (!is.null(component_cols_override)) component_cols_override else comp_cols_default
miss <- setdiff(comp_cols, names(raw))
if (length(miss)) stop(paste0("As seguintes colunas de componente não foram encontradas: ",
                              paste(miss, collapse=", ")))

# ---------------- preparar base ----------------
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
    )
  ) %>% 
  filter(!is.na(traj)) %>%
  # converte os 7 componentes
  mutate(across(all_of(comp_cols), to_num_strict)) %>%
  # soma dos componentes
  rowwise() %>%
  mutate(custo_total = sum(c_across(all_of(comp_cols)), na.rm = TRUE)) %>%
  ungroup()

# LOS
if (!is.na(los_col)) {
  base <- base %>% mutate(los_dias = to_num_strict(.data[[los_col]]))
} else if (!is.na(data_adm_col) && (!is.na(data_alta_col) || !is.na(data_obito_col))) {
  adm   <- as_date_guess(base[[data_adm_col]])
  saida <- if (!is.na(data_alta_col))  as_date_guess(base[[data_alta_col]])  else as.Date(NA)
  obito <- if (!is.na(data_obito_col)) as_date_guess(base[[data_obito_col]]) else as.Date(NA)
  fim   <- dplyr::coalesce(saida, obito)  # mantém classe Date
  base$los_dias <- as.numeric(fim - adm)
  base$los_dias[!is.finite(base$los_dias) | base$los_dias <= 0] <- NA_real_
} else {
  base$los_dias <- NA_real_
}

base <- base %>%
  mutate(
    T = paste0("T", as.integer(traj)),
    custo_dia = ifelse(is.finite(los_dias) & los_dias > 0, custo_total/los_dias, NA_real_)
  )

# ---------------- resumos numéricos ----------------
res_rn_num <- base %>%
  group_by(T) %>%
  summarise(
    N = n(),
    mean_total = mean(custo_total, na.rm=TRUE),
    sd_total   = sd(custo_total, na.rm=TRUE),
    med_total  = median(custo_total, na.rm=TRUE),
    p25_total  = quantile(custo_total, 0.25, na.rm=TRUE),
    p75_total  = quantile(custo_total, 0.75, na.rm=TRUE),
    .groups = "drop"
  )

res_dia_num <- base %>%
  filter(is.finite(custo_dia)) %>%
  group_by(T) %>%
  summarise(
    mean_day = mean(custo_dia, na.rm=TRUE),
    sd_day   = sd(custo_dia, na.rm=TRUE),
    med_day  = median(custo_dia, na.rm=TRUE),
    p25_day  = quantile(custo_dia, 0.25, na.rm=TRUE),
    p75_day  = quantile(custo_dia, 0.75, na.rm=TRUE),
    .groups = "drop"
  )

# ---------------- montar tabelas (texto simples e R$) ----------------
tabela_final <- res_rn_num %>%
  mutate(
    `Custo por RN (média±DP)`          = fmt_mean_sd(c(mean_total, sd_total)[1:length(mean_total)]*0 + mean_total), # usa helper
    `Custo por RN (mediana [P25–P75])` = paste0(format(round(med_total,2), nsmall=2)," [",
                                                format(round(p25_total,2), nsmall=2),"–",
                                                format(round(p75_total,2), nsmall=2),"]")
  ) %>%
  left_join(
    res_dia_num %>%
      mutate(
        `Custo por paciente-dia (média±DP)`          = fmt_mean_sd(c(mean_day, sd_day)[1:length(mean_day)]*0 + mean_day),
        `Custo por paciente-dia (mediana [P25–P75])` = paste0(format(round(med_day,2), nsmall=2)," [",
                                                              format(round(p25_day,2), nsmall=2),"–",
                                                              format(round(p75_day,2), nsmall=2),"]")
      ) %>%
      select(T, `Custo por paciente-dia (média±DP)`, `Custo por paciente-dia (mediana [P25–P75])`),
    by = "T"
  ) %>%
  mutate(T = factor(T, levels = paste0("T",1:4))) %>%
  arrange(T) %>%
  select(`Trajetória` = T, N,
         `Custo por RN (média±DP)`,
         `Custo por RN (mediana [P25–P75])`,
         `Custo por paciente-dia (média±DP)`,
         `Custo por paciente-dia (mediana [P25–P75])`)

# Versão com moeda (R$)
tabela_brl <- res_rn_num %>%
  mutate(
    `Custo por RN (média±DP)`          = paste0(brl(mean_total), " ± ", brl(sd_total)),
    `Custo por RN (mediana [P25–P75])` = paste0(brl(med_total), " [", brl(p25_total),"–", brl(p75_total), "]")
  ) %>%
  left_join(
    res_dia_num %>%
      mutate(
        `Custo por paciente-dia (média±DP)`          = paste0(brl(mean_day), " ± ", brl(sd_day)),
        `Custo por paciente-dia (mediana [P25–P75])` = paste0(brl(med_day), " [", brl(p25_day),"–", brl(p75_day), "]")
      ) %>%
      select(T, `Custo por paciente-dia (média±DP)`, `Custo por paciente-dia (mediana [P25–P75])`),
    by = "T"
  ) %>%
  mutate(T = factor(T, levels = paste0("T",1:4))) %>%
  arrange(T) %>%
  select(`Trajetória` = T, N,
         `Custo por RN (média±DP)`,
         `Custo por RN (mediana [P25–P75])`,
         `Custo por paciente-dia (média±DP)`,
         `Custo por paciente-dia (mediana [P25–P75])`)

# ---------------- salvar ----------------
writexl::write_xlsx(
  list(
    "Tabela para artigo"      = tabela_final,
    "Tabela (R$ formatado)"   = tabela_brl,
    "Base utilizada (limpa)"  = base %>% select(`Trajetória` = T, traj, custo_total, los_dias, custo_dia, all_of(comp_cols))
  ),
  file.path(out_dir, "tabela_custos_por_trajetoria.xlsx")
)
message("✅ Arquivo salvo em: ", file.path(out_dir, "tabela_custos_por_trajetoria.xlsx"))
