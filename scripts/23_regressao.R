# ================================================================
# Regressão Logística Multinomial – Trajetórias (type 1-4)
# Preditores: IG (semanas), Apgar 5º min, Reanimação (S/N)
# Referência: type 2
# Saídas: OR/IC/p (csv/xlsx), Pseudo-R², gráficos PNG com fundo branco
# ================================================================

# -------- 0) Pacotes --------
required <- c("nnet","dplyr","readxl","readr","stringr","tidyr",
              "ggplot2","forcats","purrr","tibble","writexl")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(required, library, character.only = TRUE))

# -------- 1) Parâmetros --------
input_file  <- "data/input/dados_regressao.xlsx"  # <-- ajuste se preciso
sheet_name  <- "Planilha1"
output_dir  <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
timestamp   <- format(Sys.time(), "%Y%m%d_%H%M")

# -------- 2) Leitura --------
df <- readxl::read_excel(input_file, sheet = sheet_name)

# Esperado: colunas 'cluster', 'IG', 'APGAR 5', 'RCP'
# cluster: "type 1" ... "type 4"
# IG: "32+2" (semanas + dias) ou "32"
# APGAR 5: numérico
# RCP: "SIM"/"NÃO" (variações aceitas)

# -------- 3) Higienização --------
# 3.1 IG "32+2" -> 32 + 2/7
parse_ig <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub(",", ".", x, fixed = TRUE)
  m <- stringr::str_match(x, "^(\\d{1,2})\\s*[\\+\\./:\\- ]?\\s*(\\d{0,2})$")
  weeks <- suppressWarnings(as.numeric(m[,2]))
  days  <- suppressWarnings(as.numeric(ifelse(m[,3] == "" | is.na(m[,3]), "0", m[,3])))
  weeks + (days/7)
}

# 3.2 Montar base do modelo
df_model <- df %>%
  transmute(
    traj_raw  = .data[["cluster"]],
    ig        = parse_ig(.data[["IG"]]),
    apgar5    = suppressWarnings(as.numeric(.data[["APGAR 5"]])),
    rean_raw  = .data[["RCP"]]
  )

# 3.3 Trajetória (ref = type 2)
extract_num <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  num <- suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d+")))
  num
}
tipo_num <- extract_num(df_model$traj_raw)
if (any(is.na(tipo_num))) stop("Valores de 'cluster' sem número reconhecível (1..4).")
df_model$traj <- factor(paste0("type ", tipo_num), levels = paste0("type ", 1:4))
df_model$traj <- forcats::fct_relevel(df_model$traj, "type 2")

# 3.4 Reanimação S/N
rean_chr <- tolower(trimws(as.character(df_model$rean_raw)))
map_rean <- c("s"="Sim","sim"="Sim","1"="Sim","y"="Sim","yes"="Sim","true"="Sim","sim "="Sim",
              "n"="Não","nao"="Não","não"="Não","nao "="Não","não"="Não","0"="Não","no"="Não","false"="Não")
rean_std <- dplyr::recode(rean_chr, !!!map_rean, .default = NA_character_)
df_model$rean <- factor(rean_std, levels = c("Não","Sim"))

# 3.5 Remover NAs
n0 <- nrow(df_model)
df_model <- df_model %>% dplyr::select(traj, ig, apgar5, rean) %>% tidyr::drop_na()
message(sprintf("Casos completos: %d (removidos %d por NA).", nrow(df_model), n0 - nrow(df_model)))
print(table(df_model$traj))

# -------- 4) Modelo multinomial --------
set.seed(123)
mod <- nnet::multinom(traj ~ ig + apgar5 + rean, data = df_model, trace = FALSE)

# -------- 5) Tabela de OR/IC95/p --------
sum_mod <- summary(mod)
betas   <- sum_mod$coefficients
ses     <- sum_mod$standard.errors

or_tbl <- purrr::map2_dfr(
  split(betas, row(betas)),
  split(ses,   row(ses)),
  ~{
    b <- matrix(.x, nrow = 1); s <- matrix(.y, nrow = 1)
    cn <- colnames(betas)
    cat_lab <- rownames(betas)[as.integer(row(b))]
    tibble::tibble(
      contraste = cat_lab,
      variavel  = cn,
      beta      = as.numeric(b),
      se        = as.numeric(s),
      z         = beta / se,
      p_value   = 2 * pnorm(abs(z), lower.tail = FALSE),
      OR        = exp(beta),
      IC95_low  = exp(beta - 1.96 * se),
      IC95_high = exp(beta + 1.96 * se)
    )
  }
) %>%
  mutate(
    variavel = dplyr::recode(
      variavel,
      ig      = "Idade gestacional (sem)",
      apgar5  = "Apgar (5 min)"
    ),
    variavel = ifelse(grepl("^rean", variavel), "Reanimação = Sim", variavel)
  ) %>%
  select(Contraste = contraste, Variável = variavel,
         OR, `IC95%_low` = IC95_low, `IC95%_high` = IC95_high, `p-valor` = p_value) %>%
  arrange(Contraste, Variável)

print(or_tbl, n = Inf)

# Salvar
out_or_csv  <- file.path(output_dir, sprintf("OR_multinom_%s.csv",  timestamp))
out_or_xlsx <- file.path(output_dir, sprintf("OR_multinom_%s.xlsx", timestamp))
readr::write_csv(or_tbl, out_or_csv)
writexl::write_xlsx(list(OR = or_tbl), out_or_xlsx)
message("Tabelas de OR salvas em:\n- ", out_or_csv, "\n- ", out_or_xlsx)

# -------- 6) Pseudo-R² (McFadden) --------
mod_null <- nnet::multinom(traj ~ 1, data = df_model, trace = FALSE)
ll_full  <- as.numeric(logLik(mod))
ll_null  <- as.numeric(logLik(mod_null))
pseudoR2_McFadden <- 1 - (ll_full / ll_null)
message(sprintf("Pseudo-R2 (McFadden): %.3f", pseudoR2_McFadden))

# -------- 7) Probabilidades e gráficos (fundo branco) --------
# Helper: tema branco sólido
theme_white_bg <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background   = element_rect(fill = "white", colour = "white"),
      panel.background  = element_rect(fill = "white", colour = "white"),
      legend.background = element_rect(fill = "white", colour = "white"),
      legend.key        = element_rect(fill = "white", colour = "white"),
      strip.background  = element_rect(fill = "white", colour = "white")
    )
}

q_ig     <- stats::quantile(df_model$ig,     probs = c(.10,.25,.50,.75,.90), na.rm = TRUE)
q_apgar5 <- stats::quantile(df_model$apgar5, probs = c(.10,.25,.50,.75,.90), na.rm = TRUE)

# (A) vs IG (Apgar fixo na mediana; rean Não/Sim)
new_ig <- expand.grid(
  ig     = seq(min(df_model$ig), max(df_model$ig), length.out = 200),
  apgar5 = as.numeric(q_apgar5[3]),
  rean   = factor(c("Não","Sim"), levels = c("Não","Sim"))
)
probs_ig <- as.data.frame(predict(mod, newdata = new_ig, type = "probs"))
names(probs_ig) <- levels(df_model$traj)

plot_ig <- cbind(new_ig, probs_ig) |>
  tidyr::pivot_longer(cols = starts_with("type "), names_to = "tipo", values_to = "prob") |>
  ggplot2::ggplot(aes(x = ig, y = prob, color = tipo)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::facet_wrap(~ rean) +
  ggplot2::labs(x = "Idade gestacional (semanas)",
                y = "Probabilidade predita",
                color = "Trajetória",
                title = "Probabilidades preditas por IG (Apgar fixo na mediana)") +
  theme_white_bg()

# (B) vs Apgar 5 (IG fixa na mediana; rean Não/Sim)
new_apgar <- expand.grid(
  ig     = as.numeric(q_ig[3]),
  apgar5 = seq(min(df_model$apgar5), max(df_model$apgar5), length.out = 200),
  rean   = factor(c("Não","Sim"), levels = c("Não","Sim"))
)
probs_apgar <- as.data.frame(predict(mod, newdata = new_apgar, type = "probs"))
names(probs_apgar) <- levels(df_model$traj)

plot_apgar <- cbind(new_apgar, probs_apgar) |>
  tidyr::pivot_longer(cols = starts_with("type "), names_to = "tipo", values_to = "prob") |>
  ggplot2::ggplot(aes(x = apgar5, y = prob, color = tipo)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::facet_wrap(~ rean) +
  ggplot2::labs(x = "Apgar 5º minuto",
                y = "Probabilidade predita",
                color = "Trajetória",
                title = "Probabilidades preditas por Apgar 5 (IG fixa na mediana)") +
  theme_white_bg()

# Salvar SEM transparência (bg branco)
out_plot_ig    <- file.path(output_dir, sprintf("plot_prob_por_IG_%s.png",     timestamp))
out_plot_apgar <- file.path(output_dir, sprintf("plot_prob_por_Apgar5_%s.png", timestamp))
ggplot2::ggsave(out_plot_ig,    plot = plot_ig,    width = 9, height = 5.5, dpi = 300, bg = "white")
ggplot2::ggsave(out_plot_apgar, plot = plot_apgar, width = 9, height = 5.5, dpi = 300, bg = "white")
message("Gráficos salvos em:\n- ", out_plot_ig, "\n- ", out_plot_apgar)

# -------- 8) Exportar base preparada --------
out_data <- file.path(output_dir, sprintf("dados_modelo_%s.csv", timestamp))
readr::write_csv(df_model, out_data)
message("Base do modelo salva em:\n- ", out_data)

# -------- 9) Resumo --------
message("Resumo:")
message(sprintf("* Pseudo-R2 (McFadden): %.3f", pseudoR2_McFadden))
message("* Contraste: cada 'type' vs referência 'type 2'.")
message("* Interpretação: OR > 1 = maior chance relativa de estar no 'type' (vs type 2) por unidade do preditor.")
