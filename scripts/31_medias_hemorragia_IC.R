# ================================================================
# Frequência de HEMOR_IC (GRAU) por cluster e total
# - robusto a nomes de coluna e rótulos
# - saída: data/output/frequencia_hemor_ic_por_cluster.xlsx
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","tidyr","stringr","janitor","writexl","tibble","purrr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------------------------------------------------------
# Config
input_file <- "data/input/Banco_de_dados_final_0708_HEMORRAGIA_IC.xlsx"
sheet_name <- "GERAL"
out_dir    <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------------------------------------------------------
# Leitura
raw <- readxl::read_excel(input_file, sheet = sheet_name, col_types = "text") %>%
  janitor::clean_names()

# ---------------------------------------------------------------
# Helpers
pick_col_regex <- function(df, patterns) {
  nms <- names(df)
  for (pat in patterns) {
    hits <- grep(pat, nms, ignore.case = TRUE, value = TRUE)
    if (length(hits)) return(hits[1])
  }
  stop("❌ Não foi possível localizar a coluna com os padrões: ",
       paste(patterns, collapse=" | "))
}

# coluna HEMOR_IC (GRAU) — aceita diversas variações
hemor_col <- pick_col_regex(
  raw,
  c("hemor.*ic.*grau", "hemorrag.*ic.*grau", "hemor.*grau", "hemorrag.*grau")
)

# coluna cluster — aceita type/cluster/tipo
cluster_col <- pick_col_regex(
  raw,
  c("^type$", "^cluster$", "^tipo$")
)

# Normalização do cluster para "Tipo 1..4"
norm_cluster <- function(x) {
  y <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    y %in% c("1","tipo 1","type 1") ~ "Tipo 1",
    y %in% c("2","tipo 2","type 2") ~ "Tipo 2",
    y %in% c("3","tipo 3","type 3") ~ "Tipo 3",
    y %in% c("4","tipo 4","type 4") ~ "Tipo 4",
    TRUE ~ as.character(x)
  )
}

# Normalização dos rótulos de HEMOR_IC (GRAU)
norm_hemor <- function(x) {
  z <- trimws(toupper(as.character(x)))
  z <- stringr::str_replace_all(z, "\\s+", " ")
  z <- stringr::str_replace_all(z, "Á|Á", "A") # normaliza acento solto
  z <- stringr::str_replace_all(z, "Ã|Ã", "A")
  # mapear números para romanos
  z <- stringr::str_replace_all(z, "GRAU\\s*1", "GRAU I")
  z <- stringr::str_replace_all(z, "GRAU\\s*2", "GRAU II")
  z <- stringr::str_replace_all(z, "GRAU\\s*3", "GRAU III")
  z <- stringr::str_replace_all(z, "GRAU\\s*4", "GRAU IV")
  # padronizar SIM/NÃO
  z <- ifelse(z %in% c("SIM"), "SIM", z)
  z <- ifelse(z %in% c("NAO","NÃO","NÃO","NÃo","NÂO"), "NÃO", z)
  z
}

df <- raw %>%
  mutate(
    cluster_norm = norm_cluster(.data[[cluster_col]]),
    hemor_norm   = norm_hemor(.data[[hemor_col]])
  )

# Ordem desejada de categorias
ord_hemor <- c("GRAU I","GRAU II","GRAU III","GRAU IV","SIM","NÃO")

# Mantém apenas linhas com HEMOR_IC não-missing para cálculo de %
df_valid <- df %>% filter(!is.na(hemor_norm) & nzchar(hemor_norm))

# ---------------------------------------------------------------
# Frequências por cluster (denominador = total de válidos por cluster)
# completa categorias ausentes com 0
base_counts <- df_valid %>%
  count(cluster_norm, hemor_norm, name = "n")

# completar grelha (todas categorias x clusters)
all_clusters <- sort(unique(df$cluster_norm))
grid <- expand.grid(cluster_norm = all_clusters, hemor_norm = ord_hemor, stringsAsFactors = FALSE)

counts_full <- grid %>%
  left_join(base_counts, by = c("cluster_norm","hemor_norm")) %>%
  mutate(n = tidyr::replace_na(n, 0L))

# totais por cluster (apenas válidos)
tot_cluster <- counts_full %>%
  group_by(cluster_norm) %>%
  summarise(den = sum(n), .groups = "drop")

tabela_cluster <- counts_full %>%
  left_join(tot_cluster, by = "cluster_norm") %>%
  mutate(pct = ifelse(den > 0, 100 * n/den, NA_real_),
         cel = sprintf("%d (%.1f%%)", n, pct)) %>%
  select(hemor_norm, cluster_norm, cel) %>%
  tidyr::pivot_wider(names_from = cluster_norm, values_from = cel)

# ---------------------------------------------------------------
# Frequências no Total (denominador = total de válidos na amostra)
tot_den <- sum(counts_full$n)
tabela_total <- counts_full %>%
  group_by(hemor_norm) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(pct = ifelse(tot_den > 0, 100 * n/tot_den, NA_real_),
         Total = sprintf("%d (%.1f%%)", n, pct)) %>%
  select(hemor_norm, Total)

# ---------------------------------------------------------------
# Junta cluster + total e ordena categorias
tabela_final <- tabela_cluster %>%
  left_join(tabela_total, by = "hemor_norm") %>%
  mutate(hemor_norm = factor(hemor_norm, levels = ord_hemor)) %>%
  arrange(hemor_norm) %>%
  rename(`HEMOR_IC (GRAU)` = hemor_norm)

# ---------------------------------------------------------------
# Exporta
out_xlsx <- file.path(out_dir, "frequencia_hemor_ic_por_cluster.xlsx")
writexl::write_xlsx(list("Frequencia HEMOR_IC" = tabela_final), out_xlsx)

# Preview no console
print(tabela_final, n = nrow(tabela_final))
message("✅ Tabela salva em: ", out_xlsx)
