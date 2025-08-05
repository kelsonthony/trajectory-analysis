# =========================================
# SCRIPT FINAL: montar_trajetoria_csv.R
# =========================================

# 1. Pacotes
packages_needed <- c("dplyr", "tidyr", "openxlsx")

for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# 2. Função para tratar datas
parse_date_safe <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "NÃO", "SIM")] <- NA

  is_numeric <- suppressWarnings(!is.na(as.numeric(x)))
  is_numeric[is.na(is_numeric)] <- FALSE

  x_date <- rep(NA, length(x))
  x_date[is_numeric] <- as.Date(as.numeric(x[is_numeric]), origin = "1899-12-30")
  x_date[!is_numeric] <- suppressWarnings(as.Date(x[!is_numeric], tryFormats = c(
    "%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d"
  )))
  as.Date(x_date, origin = "1970-01-01")
}

# 3. Função principal
montar_trajetoria <- function(
  input_path = "data/input/trajectory-start.csv",
  output_path = "data/output/trajetoria_montada.xlsx"
) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  df <- read.csv(input_path, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  df <- df %>% rename(n_neonato = ID.RN)

  colunas_niveis <- c(
    "UTIN.1" = "UTIN", "UTIN.2" = "UTIN", "UTIN.3" = "UTIN",
    "UCINCO.1" = "UCINCO", "UCINCO.2" = "UCINCO", "UCINCO.3" = "UCINCO",
    "UCINCA.1" = "UCINCA", "UCINCA.2" = "UCINCA", "UCINCA.3" = "UCINCA"
  )

  for (col in names(colunas_niveis)) {
    df[[col]] <- parse_date_safe(df[[col]])
  }

  df$D.ALTA.H <- parse_date_safe(df$D.ALTA.H)
  df$D.ÓBITO  <- parse_date_safe(df$D.ÓBITO)

  traj_list <- list()

  for (i in 1:nrow(df)) {
    row <- df[i, ]
    neonato_id <- row$n_neonato

    datas_nivel <- data.frame(dt = as.Date(character()), nivel = character())

    for (col in names(colunas_niveis)) {
      data <- row[[col]]
      if (!is.na(data)) {
        datas_nivel <- bind_rows(datas_nivel, data.frame(
          dt = as.Date(data),
          nivel = colunas_niveis[[col]],
          stringsAsFactors = FALSE
        ))
      }
    }

    datas_nivel <- datas_nivel %>% filter(!is.na(dt)) %>% arrange(dt)
    if (nrow(datas_nivel) == 0) next

    data_saida_final <- if (!is.na(row$D.ALTA.H)) row$D.ALTA.H else row$D.ÓBITO

    for (j in 1:nrow(datas_nivel)) {
      entrada <- as.Date(datas_nivel$dt[j])
      nivel   <- datas_nivel$nivel[j]

      if (j < nrow(datas_nivel)) {
        saida <- as.Date(datas_nivel$dt[j + 1]) - 1
      } else if (!is.na(data_saida_final)) {
        saida <- data_saida_final - 1  # ⚠️ agora corrigido: evita repetição da data
      } else {
        next
      }

      traj_list[[length(traj_list) + 1]] <- data.frame(
        n_neonato = neonato_id,
        dt_entrada = entrada,
        dt_saida = saida,
        nivel_assistencia = nivel,
        stringsAsFactors = FALSE
      )
    }

    # ALTA ou ÓBITO (como último evento no dia da saída)
    if (!is.na(data_saida_final)) {
      traj_list[[length(traj_list) + 1]] <- data.frame(
        n_neonato = neonato_id,
        dt_entrada = data_saida_final,
        dt_saida = data_saida_final,
        nivel_assistencia = ifelse(!is.na(row$D.ALTA.H), "ALTA", "ÓBITO"),
        stringsAsFactors = FALSE
      )
    }
  }

  traj_final <- bind_rows(traj_list) %>%
    arrange(n_neonato, dt_entrada)

  traj_final$dt_entrada <- format(as.Date(traj_final$dt_entrada), "%Y-%m-%d")
  traj_final$dt_saida   <- format(as.Date(traj_final$dt_saida), "%Y-%m-%d")

  write.xlsx(traj_final, output_path)
  message("✔️ Trajetória gerada com sucesso: ", output_path)
}

# 4. Executar
montar_trajetoria()
