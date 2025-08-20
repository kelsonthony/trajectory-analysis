# ================================================================
# Ocupação materna — frequências e percentuais por 6 grupos
# Saída: XLSX com 1 aba "Ocupacao_%"
# ================================================================

# Pacotes
pkgs <- c("readxl","dplyr","stringr","writexl")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_candidates <- c(
  "data/input/Banco_de_dados_final_0708_2_OCUPACAO.xlsx",
  "data/input/Banco_de_dados_final_0708_2.xlsx"
)
input_xlsx <- NULL
for (p in input_candidates) if (file.exists(p)) { input_xlsx <- p; break }
if (is.null(input_xlsx)) stop("❌ Arquivo de entrada não encontrado.")

sheets <- readxl::excel_sheets(input_xlsx)
sheet_name <- if ("GERAL" %in% sheets) "GERAL" else sheets[1]

out_dir  <- "data/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ts       <- format(Sys.time(), "%Y%m%d_%H%M")
out_xlsx <- file.path(out_dir, paste0("ocupacao_materna_", ts, ".xlsx"))

# -------- Helpers --------
find_col <- function(nms, patterns){
  for (pat in patterns){
    idx <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(idx)) return(nms[which(idx)[1]])
  }
  NULL
}

norm <- function(x){
  s <- toupper(trimws(as.character(x)))
  s <- iconv(s, to = "ASCII//TRANSLIT")        # remove acentos
  s <- gsub("\\s+", " ", s)
  s
}

map_ocup <- function(x){
  s <- norm(x)

  # Sem informação -> Outras
  if (s %in% c("", "NA") || grepl("S\\s*/\\s*INFO|SEM\\s*/?INFO|SEM INFORMA", s)) return("Outras")

  # Do lar (muitas variantes/typos)
  if (grepl("\\b(DO|DIO|DOM|DOR) LAR\\b", s) ||
      grepl("DONA DE CASA", s)) return("Do lar")

  # Estudante
  if (grepl("ESTUDANT", s)) return("Estudante")

  # Desempregada
  if (grepl("DESEMPREG", s)) return("Desempregada")

  # Vendedora (inclui promotora de vendas)
  if (grepl("VENDEDOR", s) || grepl("PROMOTORA? DE VENDAS", s)) return("Vendedora")

  # Autônoma / MEI / dona do próprio negócio / profissões tipicamente autônomas
  if (grepl("AUTONOM", s) || grepl("AUTONOMA", s) || grepl("AUTONOMA", s) ||
      grepl("EMPRESAR", s) || grepl("MICROEMPREENDEDOR", s) || grepl("\\bMEI\\b", s) ||
      grepl("COMERCIANT", s) || grepl("CORRETOR", s) || grepl("FEIRANT", s) ||
      grepl("CABEL(E|E)REIR", s) || grepl("MANICURE", s) || grepl("ESTETICIST", s) ||
      grepl("ARTESA", s) || grepl("DESIGN(ER)? DE UNHAS", s) ||
      grepl("DONA DE SALAO", s) || grepl("MERENDEIRA EMPRESARIA", s)) return("Autônoma")

  # fallback
  "Outras"
}

fmt_pct <- function(x) sprintf("%.1f%%", x)

# -------- Leitura --------
df0 <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df0) <- trimws(names(df0))

ocu_col <- find_col(names(df0), c("^OCU[_ ]?MAT$", "OCUP", "OCU.*M"))
if (is.null(ocu_col)) stop("❌ Coluna de ocupação (OCU_MAT) não encontrada.")

df <- df0 |>
  dplyr::transmute(Ocupacao = vapply(.data[[ocu_col]], map_ocup, character(1)))

# -------- Tabela total --------
tab <- df |>
  dplyr::count(Ocupacao, name = "n") |>
  dplyr::mutate(`%` = 100 * n / sum(n)) |>
  dplyr::mutate(`%` = fmt_pct(`%`)) |>
  # garante a ordem solicitada + "Outras" no fim
  dplyr::mutate(Ocupacao = factor(Ocupacao,
                                  levels = c("Do lar","Estudante","Autônoma","Vendedora","Desempregada","Outras"))) |>
  dplyr::arrange(Ocupacao) |>
  dplyr::mutate(Ocupacao = as.character(Ocupacao))

# -------- Exporta --------
writexl::write_xlsx(list("Ocupacao_%" = tab), path = out_xlsx)
cat("✅ Arquivo gerado:\n- ", out_xlsx, "\n", sep = "")
