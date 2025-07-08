# =========================================
# SCRIPT PARA GERAR TABELA DE TRAJETÓRIA DE NEONATOS
# =========================================

# Pacotes necessários
library(data.table)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)

gerar_trajetoria <- function(
  input_path = "data/input/TABELAS_coleta_HRT.xlsx",
  output_xlsx = "data/output/trajetoria.xlsx",
  output_csv = "data/output/trajetoria.csv",
  limite_semanas = 30
) {
  # 1. Verificar existência do arquivo
  if (!file.exists(input_path)) stop("Arquivo não encontrado: ", input_path)
  
  # 2. Leitura da planilha
  dados <- read_excel(input_path, sheet = "RTrajet.",
                      col_types = c("numeric", "date", "date", "text"))
  
  # 3. Padronização de nomes
  colnames(dados)[1] <- "n_neonato"
  dados$dt_entrada <- as.Date(dados$dt_entrada)
  dados$dt_saida   <- as.Date(dados$dt_saida)
  
  # 4. Marcar início da trajetória por neonato
  dados <- dados %>%
    group_by(n_neonato) %>%
    mutate(data_inicio = min(dt_entrada, na.rm = TRUE)) %>%
    ungroup()
  
  # 5. Cálculo de semanas
  dados <- dados %>%
    mutate(
      Dif_dias_entrada = as.numeric(dt_entrada - data_inicio),
      Dif_dias_saida = as.numeric(dt_saida - data_inicio),
      semana_entrada = pmax(1, ceiling(Dif_dias_entrada / 7)),
      semana_saida   = pmax(1, ceiling(Dif_dias_saida / 7))
    )
  
  # 6. Conversão dos níveis
  nivel_map <- c(
    "ALTA" = 1,
    "ENF/ALCON" = 2,
    "UCINCO" = 3,
    "UCINCA" = 4,
    "UTIN" = 5
  )
  dados$nivel_assist_num <- nivel_map[dados$nivel_assistencia]
  dados$nivel_assist_num[is.na(dados$nivel_assist_num)] <- 6  # Outros / óbito
  
  # 7. Gerar vetor de semanas por linha
  dados <- dados %>%
    rowwise() %>%
    mutate(semanas = list(semana_entrada:semana_saida)) %>%
    ungroup()
  
  # 8. Expandir as semanas
  traj_expandida <- dados %>%
    select(n_neonato, semanas, nivel_assist_num, nivel_assistencia, dt_entrada, dt_saida) %>%
    unnest(cols = c(semanas))
  
  # 9. Resolver conflitos: maior nível prevalece
  traj_expandida <- traj_expandida %>%
    arrange(n_neonato, semanas, desc(nivel_assist_num)) %>%
    distinct(n_neonato, semanas, .keep_all = TRUE)
  
  # 10. Mapeia de volta para nome do estado
  nivel_estado <- c(
    "1" = "ALTA",
    "2" = "ENF/ALCON",
    "3" = "UCINCO",
    "4" = "UCINCA",
    "5" = "UTIN",
    "6" = "ÓBITO"
  )
  traj_expandida$estado <- nivel_estado[as.character(traj_expandida$nivel_assist_num)]
  
  # 11. Pivotar para formato largo
  tabela_larga <- traj_expandida %>%
    filter(semanas <= limite_semanas) %>%
    mutate(semanas = as.character(semanas)) %>%
    pivot_wider(
      id_cols = n_neonato,
      names_from = semanas,
      values_from = estado,
      values_fill = "ALTA"
    )
  
  # 12. Adicionar colunas faltantes até semana limite
  semanas_faltando <- setdiff(as.character(1:limite_semanas), names(tabela_larga))
  for (sem in semanas_faltando) {
    if (!sem %in% names(tabela_larga)) {
      tabela_larga[[sem]] <- "ALTA"
    }
  }
  
  # 13. Reordenar colunas corretamente
  tabela_larga <- tabela_larga[, c("n_neonato", as.character(1:limite_semanas)), with = FALSE]
  
  # 14. Tratar óbito: sobrescrever semanas posteriores
  semanas_morte <- traj_expandida %>%
    filter(estado == "ÓBITO") %>%
    group_by(n_neonato) %>%
    summarise(semana_morte = min(semanas), .groups = "drop")
  
  tabela_larga <- left_join(tabela_larga, semanas_morte, by = "n_neonato")
  
  for (i in 1:limite_semanas) {
    col <- as.character(i)
    tabela_larga[[col]] <- ifelse(
      !is.na(tabela_larga$semana_morte) & i >= tabela_larga$semana_morte,
      "ÓBITO",
      tabela_larga[[col]]
    )
  }
  
  tabela_larga <- tabela_larga %>% select(-semana_morte)
  
  # 15. Exportar para Excel e CSV
  write.xlsx(tabela_larga, output_xlsx)
  write.csv2(tabela_larga, output_csv, row.names = FALSE)
  
  message("✔️ Trajetória gerada com sucesso:")
  message("   - XLSX: ", output_xlsx)
  message("   - CSV : ", output_csv)
}
