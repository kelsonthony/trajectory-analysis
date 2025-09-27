# ================================================================
# Custos por trajetória ajustados pelo IPCA (preços de 2024) — SIDRA API
# - Busca IPCA direto do IBGE (JSON) e higieniza valores
# - Considera Alta OU Óbito para LOS e mês de referência
# - Corrige RN-a-RN (para 2024) e só depois agrega
# - Totais por T, total da coorte
# - 1ª aba já com coluna de total por T e linha TOTAL
# - Abas <= 31 chars; formatador BRL sem warnings
# ================================================================

suppressPackageStartupMessages({
  pkgs <- c("readxl","dplyr","tidyr","stringr","readr","writexl","janitor",
            "stringi","purrr","lubridate","rlang","jsonlite","tibble")
  new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if (length(new)) install.packages(new, dependencies = TRUE)
  lapply(pkgs, library, character.only = TRUE)
})

# ---------------- paths / parâmetros ----------------
in_file <- "data/input/Banco_de_dados_final_0708_Custos.xlsx"
sheet   <- readxl::excel_sheets(in_file)[1]
out_dir <- "data/output"; if (!dir.exists(out_dir)) dir.create(out_dir, TRUE)

# 7 componentes de custo na planilha
comp_cols <- c("Diárias_UTI","Exames","Material","Tratamentos",
               "Consultas","Ass.Fisio","Serv/Trat")

# mês/ano alvo do IPCA (preços de 2024)
ipca_ref_date <- as.Date("2024-12-01")

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
    x[has_comma] <- gsub("\\.", "", x[has_comma])  # remove milhar
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
# Formatador BRL sem warnings (big.mark=".", decimal.mark=",")
brl_fmt <- function(x, digits = 2){
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.finite(x),
                paste0("R$ ", format(round(x, digits),
                                     big.mark=".", decimal.mark=",", nsmall=digits)),
                NA_character_)
  return(out)
}

# ---------------- ler planilha e preparar base (nominal) ----------------
raw <- readxl::read_excel(in_file, sheet = sheet)
stopifnot(nrow(raw) > 0)

nm <- names(raw); nn <- norm_names(nm)

# detectar coluna de trajetória
traj_col <- nm[which(nn %in% c("traj","trajetoria","cluster","tipo","classe","grupo"))[1]]
if (is.na(traj_col)) traj_col <- nm[grep("(cluster|traj|tipo)", nn, perl=TRUE)[1]]
stopifnot(!is.na(traj_col))

# detectar colunas de datas
data_adm_col   <- nm[grep("^data_?adm|\\badm\\b", nn)][1]
data_alta_col  <- nm[grep("^data_?alta|\\balta\\b", nn)][1]
data_obito_col <- nm[grep("^data_?obito|obito|óbito", nn)][1]

# checar componentes de custo
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
    # referência para IPCA e LOS (Alta > Óbito > Admissão)
    data_ref_ipca = dplyr::coalesce(Data_alta, Data_obito, Data_adm),
    ym_ref        = as.Date(format(lubridate::floor_date(data_ref_ipca, "month"), "%Y-%m-01")),
    data_final_los = dplyr::coalesce(Data_alta, Data_obito),
    los_dias = as.numeric(data_final_los - Data_adm),
    los_dias = dplyr::if_else(is.finite(los_dias) & los_dias > 0, los_dias, NA_real_),
    T = paste0("T", as.integer(traj)),
    custo_dia = dplyr::if_else(is.finite(los_dias) & los_dias > 0, custo_total/los_dias, NA_real_)
  )

# níveis dinâmicos de T (caso não existam exatamente T1..T4)
levT <- paste0("T", sort(unique(as.integer(base$traj))))

# ---------------- IPCA mensal (API SIDRA DIRETA) ----------------
ipca_url <- "https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2265/p/all/d/v2265%202"

ipca_json <- tryCatch({
  jsonlite::fromJSON(ipca_url)
}, error = function(e){
  stop("Falha ao consultar a API do SIDRA (IBGE). Verifique conexão/endpoint: ", ipca_url)
})

# primeira linha = nomes das colunas; limpar e transformar
colnames(ipca_json) <- ipca_json[1, ]
ipca_json <- ipca_json[-1, ]

# --- transformar JSON em tabela de índices (limpando 'Valor') ---
ipca <- ipca_json %>%
  as_tibble() %>%
  transmute(
    ym = lubridate::ymd(paste0(`Mês (Código)`, "01")),
    indice = Valor %>%
      gsub("\\s+", "", .) %>%
      gsub("\\.", "", .) %>%         # remove milhar pt-BR
      gsub(",", ".", .) %>%          # vírgula decimal -> ponto
      gsub("[^0-9\\.-]", "", .) %>%  # remove outros símbolos
      as.numeric()
  ) %>%
  arrange(ym) %>%
  distinct(ym, .keep_all = TRUE)

# validação mínima: precisa ter 2018..2024 com índice numérico
anos_needed <- 2018:2024
faltando <- ipca %>%
  dplyr::filter(lubridate::year(ym) %in% anos_needed & !is.finite(indice))
if (nrow(faltando) > 0) {
  stop("Há índice IPCA faltante/não numérico em: ",
       paste(format(faltando$ym, "%Y-%m"), collapse = ", "),
       ". Verifique a resposta da API: ", ipca_url)
}

# referência dentro de 2024
ipca_2024 <- ipca %>% dplyr::filter(format(ym, "%Y") == "2024") %>% arrange(ym)
stopifnot(nrow(ipca_2024) > 0)
ref_used <- if (ipca_ref_date %in% ipca_2024$ym) ipca_ref_date else max(ipca_2024$ym)
ipca_ref_val <- ipca$indice[ipca$ym == ref_used][1]

# ---------------- aplicar fator de atualização (RN-a-RN) ----------------
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
  mutate(T = factor(T, levels = levT)) %>%
  arrange(T) %>%
  select(`Trajetória` = T, N,
         `Custo por RN (média±DP)`,
         `Custo por RN (mediana [P25–P75])`,
         `Custo por paciente-dia (média±DP)`,
         `Custo por paciente-dia (mediana [P25–P75])`)

# ---------------- versão formatada em R$ (resumo adicional) ----------------
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
    `Custo por RN (média±DP)`          = paste0(brl_fmt(mean_total), " ± ", brl_fmt(sd_total)),
    `Custo por RN (mediana [P25–P75])` = paste0(brl_fmt(med_total), " [", brl_fmt(p25_total), "–", brl_fmt(p75_total), "]"),
    `Custo por paciente-dia (média±DP)`          = ifelse(is.finite(mean_day), paste0(brl_fmt(mean_day), " ± ", brl_fmt(sd_day)), "—"),
    `Custo por paciente-dia (mediana [P25–P75])` = ifelse(is.finite(med_day),  paste0(brl_fmt(med_day), " [", brl_fmt(p25_day), "–", brl_fmt(p75_day), "]"), "—")
  ) %>%
  mutate(T = factor(T, levels = levT)) %>%
  arrange(T) %>%
  select(`Trajetória` = T, N,
         `Custo por RN (média±DP)`,
         `Custo por RN (mediana [P25–P75])`,
         `Custo por paciente-dia (média±DP)`,
         `Custo por paciente-dia (mediana [P25–P75])`)

# ---------------- totais por trajetória e total da coorte ----------------
totais_por_trajetoria_2024 <- base_adj %>%
  group_by(T) %>%
  summarise(
    N = n(),
    custo_total_trajetoria_2024 = sum(custo_total_2024, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(T = factor(T, levels = levT)) %>%
  arrange(T)

total_coorte_2024 <- base_adj %>%
  summarise(custo_total_coorte_2024 = sum(custo_total_2024, na.rm = TRUE))

# -------- 1ª aba: incluir coluna de total por T e linha TOTAL --------
tabela_2024_full <- tabela_2024 %>%
  left_join(
    totais_por_trajetoria_2024 %>% select(Trajetória = T, custo_total_trajetoria_2024),
    by = "Trajetória"
  ) %>%
  mutate(`Custo total da trajetória (R$ 2024)` = brl_fmt(custo_total_trajetoria_2024)) %>%
  select(-c(custo_total_trajetoria_2024))

linha_total <- tibble::tibble(
  `Trajetória` = "TOTAL",
  N = sum(totais_por_trajetoria_2024$N),
  `Custo por RN (média±DP)` = "—",
  `Custo por RN (mediana [P25–P75])` = "—",
  `Custo por paciente-dia (média±DP)` = "—",
  `Custo por paciente-dia (mediana [P25–P75])` = "—",
  `Custo total da trajetória (R$ 2024)` = brl_fmt(total_coorte_2024$custo_total_coorte_2024)
)

tabela_2024_full <- dplyr::bind_rows(tabela_2024_full, linha_total)

# -------- outras abas (formatadas e base) --------
totais_por_trajetoria_2024_brl <- totais_por_trajetoria_2024 %>%
  mutate(`Custo total da trajetória (R$ 2024)` = brl_fmt(custo_total_trajetoria_2024)) %>%
  select(`Trajetória` = T, N, `Custo total da trajetória (R$ 2024)`)

total_coorte_2024_brl <- total_coorte_2024 %>%
  mutate(`Custo total da coorte (R$ 2024)` = brl_fmt(custo_total_coorte_2024)) %>%
  select(`Custo total da coorte (R$ 2024)`)

# (resumo adicional com coluna de total e linha TOTAL)
tabela_2024_com_totais <- tabela_2024 %>%
  left_join(
    totais_por_trajetoria_2024 %>% select(Trajetória = T, custo_total_trajetoria_2024),
    by = "Trajetória"
  )
tabela_2024_com_totais_brl <- tabela_2024_com_totais %>%
  mutate(`Custo total da trajetória (R$ 2024)` = brl_fmt(custo_total_trajetoria_2024)) %>%
  select(-c(custo_total_trajetoria_2024))
tabela_2024_final_brl <- dplyr::bind_rows(
  tabela_2024_com_totais_brl,
  linha_total
)

# ---------------- salvar (abas <= 31 caracteres) ----------------
out_xlsx <- file.path(out_dir, "tabela_custos_por_trajetoria_IPCA2024.xlsx")
writexl::write_xlsx(
  list(
    "Custos 2024"           = tabela_2024_full,  # 1ª aba com col. total e linha TOTAL
    "Custos R$ 2024"        = tab_2024_brl,
    "Totais por T (num)"    = totais_por_trajetoria_2024,
    "Totais por T (R$)"     = totais_por_trajetoria_2024_brl,
    "Total coorte (num)"    = total_coorte_2024,
    "Total coorte (R$)"     = total_coorte_2024_brl,
    "Resumo+TOTAL (R$)"     = tabela_2024_final_brl,
    "Base ajustada"         = base_adj %>%
      dplyr::select(traj, `Trajetória`=T,
             Data_adm, Data_alta, Data_obito, data_ref_ipca, ym_ref,
             ipca_mes, fator_ipca_2024,
             custo_total, custo_total_2024,
             los_dias, custo_dia, custo_dia_2024,
             dplyr::all_of(comp_cols))
  ),
  out_xlsx
)
message("✅ Arquivo salvo em: ", out_xlsx,
        " | Referência IPCA: ", format(ref_used, "%Y-%m"),
        " | Endpoint IBGE: ", ipca_url)
