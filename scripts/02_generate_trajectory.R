# =========================================
# SCRIPT PARA GERAR TABELA DE TRAJETÓRIA DE NEONATOS (COM SIGLAS ABREVIADAS)
# =========================================

library(data.table)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)

gerar_trajetoria <- function(
  input_path = "data/output/trajetoria_montada.xlsx",
  sheet_name = "Sheet 1",
  limite_semanas = 30
) {
  # 1. Verifica se o arquivo existe
  if (!file.exists(input_path)) stop("Arquivo não encontrado: ", input_path)

  # 2. Geração dos nomes de saída com timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_xlsx <- paste0("data/output/trajetoria_", timestamp, ".xlsx")
  output_csv  <- paste0("data/output/trajetoria_", timestamp, ".csv")

  # 3. Leitura da planilha
  dados <- read_excel(input_path, sheet = sheet_name,
                      col_types = c("numeric", "text", "text", "text"))

  # 4. Padroniza nomes
  colnames(dados)[1] <- "n_neonato"

  # 5. Converte datas de texto para Date
  dados$dt_entrada <- suppressWarnings(as.Date(dados$dt_entrada))
  dados$dt_saida   <- suppressWarnings(as.Date(dados$dt_saida))

  # 6. Marca início da trajetória
  dados <- dados %>%
    group_by(n_neonato) %>%
    mutate(data_inicio = min(dt_entrada, na.rm = TRUE)) %>%
    ungroup()

  # 7. Cálculo de semanas
  dados <- dados %>%
    mutate(
      Dif_dias_entrada = as.numeric(dt_entrada - data_inicio),
      Dif_dias_saida   = as.numeric(dt_saida - data_inicio),
      semana_entrada   = pmax(1, ceiling(Dif_dias_entrada / 7)),
      semana_saida     = pmax(1, ceiling(Dif_dias_saida / 7))
    )

  # 8. Conversão dos níveis para número
  nivel_map <- c(
    "ALTA"      = 1,
    "ENF/ALCON" = 2,
    "UCINCO"    = 3,
    "UCINCA"    = 4,
    "UTIN"      = 5
  )
  dados$nivel_assist_num <- nivel_map[dados$nivel_assistencia]
  dados$nivel_assist_num[is.na(dados$nivel_assist_num)] <- 6  # Outros / óbito

  # 9. Geração da sequência de semanas
  dados <- dados %>%
    rowwise() %>%
    mutate(
      semanas = list(
        if (!is.na(semana_entrada) && !is.na(semana_saida)) {
          seq(semana_entrada, semana_saida)
        } else {
          NA_integer_
        }
      )
    ) %>%
    ungroup()

  # 10. Expansão da trajetória
  traj_expandida <- dados %>%
    select(n_neonato, semanas, nivel_assist_num, nivel_assistencia, dt_entrada, dt_saida) %>%
    unnest(cols = c(semanas)) %>%
    filter(!is.na(semanas))

  # 11. Resolução de conflitos
  traj_expandida <- traj_expandida %>%
    arrange(n_neonato, semanas, desc(nivel_assist_num)) %>%
    distinct(n_neonato, semanas, .keep_all = TRUE)

  # 12. Mapeia para siglas abreviadas
  nivel_estado <- c(
    "1" = "ALT",
    "2" = "ENF",
    "3" = "UCO",
    "4" = "UCA",
    "5" = "UTI",
    "6" = "OBI"
  )
  traj_expandida$estado <- nivel_estado[as.character(traj_expandida$nivel_assist_num)]

  # 13. Pivotagem para formato largo
  tabela_larga <- traj_expandida %>%
    filter(semanas <= limite_semanas) %>%
    mutate(semanas = as.character(semanas)) %>%
    pivot_wider(
      id_cols = n_neonato,
      names_from = semanas,
      values_from = estado,
      values_fill = "ALT"
    )

  # 14. Adiciona colunas ausentes
  semanas_faltando <- setdiff(as.character(1:limite_semanas), names(tabela_larga))
  for (sem in semanas_faltando) {
    tabela_larga[[sem]] <- "ALT"
  }

  # 15. Reordena colunas
  tabela_larga <- tabela_larga[, c("n_neonato", as.character(1:limite_semanas))]

  # 16. Trata óbitos
  semanas_morte <- traj_expandida %>%
    filter(estado == "OBI") %>%
    group_by(n_neonato) %>%
    summarise(semana_morte = min(semanas), .groups = "drop")

  tabela_larga <- left_join(tabela_larga, semanas_morte, by = "n_neonato")
  for (i in 1:limite_semanas) {
    col <- as.character(i)
    tabela_larga[[col]] <- ifelse(
      !is.na(tabela_larga$semana_morte) & i >= tabela_larga$semana_morte,
      "OBI",
      tabela_larga[[col]]
    )
  }
  tabela_larga <- tabela_larga %>% select(-semana_morte)

  # 17. Exportação
  dir.create(dirname(output_xlsx), showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(output_csv), showWarnings = FALSE, recursive = TRUE)

  write.xlsx(tabela_larga, output_xlsx)
  write.csv2(tabela_larga, output_csv, row.names = FALSE)

  message("✔️ Trajetória gerada com sucesso:")
  message("   - XLSX: ", output_xlsx)
  message("   - CSV : ", output_csv)
}

# Executar a função
gerar_trajetoria()
