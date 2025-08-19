# ================================================================
# TABELA COMPLETA (Total + por grupo)
# IG exibida como W.D (semanas.dias 0..6), APGAR 0..10,
# Nominais em %, ESC_MAT robusto (COMP/INC, acentos, abreviações).
# ================================================================

# -------- Pacotes --------
pkgs <- c("readxl","dplyr","tidyr","stringr","readr",
          "writexl","officer","flextable","purrr","tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -------- Config --------
input_xlsx <- "data/input/Banco_de_dados_final_0708_2.xlsx"
sheet_name <- "GERAL"
output_dir <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
ts <- format(Sys.time(), "%Y%m%d_%H%M")
out_csv  <- file.path(output_dir, paste0("tabela_completa_", ts, ".csv"))
out_xlsx <- file.path(output_dir, paste0("tabela_completa_", ts, ".xlsx"))
out_docx <- file.path(output_dir, paste0("tabela_completa_", ts, ".docx"))

PERCENT_ONLY <- TRUE  # TRUE = só "%", FALSE = "n (xx.x%)"

`%||%` <- function(a,b) if (!is.null(a)) a else b

# -------- Helpers numéricos/texto --------
num_clean <- function(x){
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub(",", ".", x)
  x <- gsub("[^0-9.\\-]", "", x)
  suppressWarnings(as.numeric(x))
}
norm_txt <- function(x){
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("","NA","N/A","NULL",
             "SEM INFO","SEM/INFO","SEM-INF",
             "SEM INFORMACAO","SEMINFORMACAO")] <- NA
  x[x == "NAO"] <- "NÃO"
  x[x == "MASC"] <- "MASCULINO"
  x[x == "FEM"]  <- "FEMININO"
  x
}

# -------- IG --------
ig_to_weeks <- function(x){
  if (is.na(x)) return(NA_real_)
  s <- gsub("\\s+","", as.character(x))
  if (s == "") return(NA_real_)
  if (grepl("\\+", s)) {
    parts <- strsplit(gsub("[^0-9+]", "", s), "\\+", fixed = TRUE)[[1]]
    w <- suppressWarnings(as.numeric(parts[1]))
    d <- if (length(parts) > 1) suppressWarnings(as.numeric(parts[2])) else 0
  } else {
    w <- suppressWarnings(as.numeric(gsub("[^0-9.\\-]","", s))); d <- 0
  }
  if (is.na(w)) return(NA_real_)
  if (is.na(d)) d <- 0
  if (d >= 7) { w <- w + floor(d/7); d <- d %% 7 }
  if (d < 0)  { k <- ceiling(abs(d)/7); w <- w - k; d <- d + 7*k }
  weeks <- w + d/7
  if (weeks < 20 || weeks > 45) return(NA_real_)
  weeks
}
weeks_to_WD <- function(w){
  if (is.na(w)) return(NA_character_)
  total_days <- round(w * 7)
  ww <- floor(total_days/7)
  dd <- total_days %% 7
  if (dd == 7) { ww <- ww + 1; dd <- 0 }
  sprintf("%d.%d", ww, dd)
}
fmt_mean_sd_numeric <- function(v, d_mean=1, d_sd=1, clamp=NULL){
  if (!is.numeric(v)) v <- num_clean(v)
  if (!is.null(clamp)) v[v < clamp[1] | v > clamp[2]] <- NA
  if (all(is.na(v))) return("NA")
  paste0(round(mean(v, na.rm=TRUE), d_mean),
         " (", round(sd(v, na.rm=TRUE), d_sd), ")")
}
fmt_mean_sd_apgar <- function(v){
  v <- num_clean(v); v[v<0 | v>10] <- NA
  if (all(is.na(v))) return("NA")
  paste0(round(mean(v, na.rm=TRUE), 1),
         " (", round(sd(v, na.rm=TRUE), 1), ")")
}
fmt_mean_sd_IG_WD <- function(v_weeks){
  w <- v_weeks; w[w < 20 | w > 45] <- NA
  if (all(is.na(w))) return("NA")
  m <- mean(w, na.rm=TRUE); s <- sd(w, na.rm=TRUE)
  paste0(weeks_to_WD(m), " (", weeks_to_WD(s), ")")
}

# % genérica (normaliza; bom pra SIM/NÃO etc)
fmt_pct <- function(x, level = NULL, yes_levels = NULL){
  v <- norm_txt(x)
  if (!is.null(level)) {
    targets <- toupper(c(level, if (level=="NÃO") "NAO"))
    n_yes <- sum(!is.na(v) & v %in% targets)
    N     <- sum(!is.na(v))
  } else {
    yl <- toupper(yes_levels %||% c("SIM","YES","1"))
    n_yes <- sum(!is.na(v) & v %in% yl)
    N     <- sum(!is.na(v))
  }
  pct <- if (N>0) round(100*n_yes/N,1) else NA_real_
  if (PERCENT_ONLY) if (is.na(pct)) "NA" else paste0(pct, "%") else paste0(n_yes, " (", pct, "%)")
}

# % exata (NÃO normaliza; preserva “SEM INFORMAÇÃO”)
fmt_pct_exact_strict <- function(x, level){
  v <- as.character(x)  # nada de norm_txt aqui!
  n_yes <- sum(!is.na(v) & v == level)
  N     <- sum(!is.na(v))
  pct <- if (N>0) round(100*n_yes/N,1) else NA_real_
  if (PERCENT_ONLY) if (is.na(pct)) "NA" else paste0(pct, "%") else paste0(n_yes, " (", pct, "%)")
}

# -------- Leitura --------
stopifnot(file.exists(input_xlsx))
df <- readxl::read_excel(input_xlsx, sheet = sheet_name)
names(df) <- trimws(names(df))

# -------- Grupo --------
pick_group_col <- function(df){
  cands <- c("CU","cluster","CLUSTER","Grupo","GRUPO","Group","TYPE","TIPO")
  hit <- cands[cands %in% names(df)]
  if (length(hit)) hit[1] else NA_character_
}
group_col <- pick_group_col(df)
if (is.na(group_col)) stop("❌ Não encontrei a coluna de grupos (CU/cluster/Grupo/TYPE...).")
df$Grupo <- as.character(df[[group_col]])
lvl <- unique(df$Grupo)
group_cols <- c("Total (N)", lvl)

# -------- Mapeamento de colunas --------
first_existing <- function(cands, cols){ hit <- cands[cands %in% cols]; if (length(hit)) hit[1] else NA_character_ }
col <- list(
  IG        = first_existing(c("IG"), names(df)),
  PESO      = first_existing(c("P_NAS","PESO","PESO_NASC","PESO_NASCIMENTO"), names(df)),
  SEXO      = first_existing(c("GÊNERO","GENERO","SEXO","Sexo","SEXO_NASC"), names(df)),
  APGAR1    = first_existing(c("APGAR 1","APGAR1","APGAR_1"), names(df)),
  APGAR5    = first_existing(c("APGAR 5","APGAR5","APGAR_5"), names(df)),
  RCP       = first_existing(c("RCP","REANIMACAO","REANIMAÇÃO"), names(df)),
  SUP_O2    = first_existing(c("SUP.O2","SUP_O2","SUP_OXIGENIO","SUP_OXIGÊNIO"), names(df)),
  VM        = first_existing(c("VM"), names(df)),
  VNI       = first_existing(c("VNI"), names(df)),
  CNAF      = first_existing(c("CNAF"), names(df)),
  CPAP      = first_existing(c("CPAP"), names(df)),
  HOOD      = first_existing(c("HOOD"), names(df)),
  CNO2      = first_existing(c("CNO2","CATETER NASAL","CATETER_NASAL"), names(df)),
  PARTO     = first_existing(c("T.PARTO","TIPO_PARTO","PARTO","Tipo de parto"), names(df)),
  HEM_PULM  = first_existing(c("HEMOR_PULM"), names(df)),
  HEM_IC    = first_existing(grep("^HEMOR_IC", names(df), value = TRUE), names(df)),
  HIPERT    = first_existing(c("HIPERT_PULM"), names(df)),
  PNEU_CONG = first_existing(c("PNEU_CONG"), names(df)),
  PNEU_ADQ  = first_existing(c("PNEU_ADQ"), names(df)),
  ENT_NEC   = first_existing(c("ENT_NEC"), names(df)),
  INF_PRES  = first_existing(c("INFEC_PRES"), names(df)),
  INF_TAR   = first_existing(c("INFEC_TAR"), names(df)),
  SEPSE     = first_existing(c("SEPSE"), names(df)),
  ATB       = first_existing(c("ATB"), names(df)),
  SIFILIS   = first_existing(c("SÍFILIS","SIFILIS"), names(df)),
  IDA_MAT   = first_existing(c("IDA_MAT","IDADE_MAT"), names(df)),
  CPN       = first_existing(c("CPN"), names(df)),
  ESC_MAT   = first_existing(c("ESC_MAT"), names(df)),
  OCU_MAT   = first_existing(c("OCU_MAT","OCUPACAO","OCUPAÇÃO"), names(df)),
  GPA       = first_existing(c("GxPyAz","GxPxA"), names(df))
)

# -------- Normalizações gerais --------
df$IG_weeks <- if (!is.na(col$IG)) vapply(df[[col$IG]], ig_to_weeks, numeric(1)) else NA_real_

for (nm in c(col$PESO, col$APGAR1, col$APGAR5, col$IDA_MAT, col$CPN)) {
  if (!is.na(nm)) df[[nm]] <- num_clean(df[[nm]])
}
for (nm in c(col$SEXO, col$RCP, col$SUP_O2, col$VM, col$VNI, col$CNAF, col$CPAP, col$HOOD, col$CNO2,
             col$PARTO, col$HEM_PULM, col$HIPERT, col$PNEU_CONG, col$PNEU_ADQ, col$ENT_NEC,
             col$INF_PRES, col$INF_TAR, col$SEPSE, col$ATB, col$SIFILIS, col$OCU_MAT)) {
  if (!is.na(nm)) df[[nm]] <- norm_txt(df[[nm]])
}

# -------- Tipo de parto --------
if (!is.na(col$PARTO)){
  p <- df[[col$PARTO]]
  df$PARTO_CAT <- dplyr::case_when(
    grepl("CESAR", p) ~ "CESÁREA",
    grepl("VAGIN", p) ~ "VAGINAL",
    is.na(p) ~ "SEM INFORMAÇÃO",
    TRUE ~ "SEM INFORMAÇÃO"
  )
}

# -------- Hemorragia IC --------
if (!is.na(col$HEM_IC)){
  h <- df[[col$HEM_IC]]
  df$HEM_IC_GRAU <- dplyr::case_when(
    is.na(h) ~ "NÃO",
    h %in% c("NAO","NÃO","0","ZERO") ~ "NÃO",
    grepl("\\bIV\\b|\\b4\\b", h) ~ "GRAU IV",
    grepl("\\bIII\\b|\\b3\\b", h) ~ "GRAU III",
    grepl("\\bII\\b|\\b2\\b", h) ~ "GRAU II",
    grepl("\\bI\\b|\\b1\\b", h) ~ "GRAU I",
    TRUE ~ "NÃO"
  )
}

# -------- Gestações prévias --------
gest_prev_from_gpa <- function(x){
  s <- as.character(x)
  g <- suppressWarnings(as.numeric(sub(".*?G\\s*([0-9]+).*", "\\1", paste0("G", s))))
  ifelse(is.na(g), NA, pmax(g-1, 0))
}
df$GEST_PREV <- if (!is.na(col$GPA)) gest_prev_from_gpa(df[[col$GPA]]) else NA_real_

# -------- ESCOLARIZAÇÃO (7 níveis: SUPERIOR C/INC, MÉDIO C/INC, FUND INC/COMP, SEM INFO) --------
if (!is.na(col$ESC_MAT)){
  e_raw <- as.character(df[[col$ESC_MAT]])
  e <- toupper(trimws(iconv(e_raw, from = "", to = "ASCII//TRANSLIT")))
  e <- gsub("[^A-Z0-9/ ]", " ", e); e <- gsub("\\s+", " ", e)

  df$ESC_CAT7 <- dplyr::case_when(
    grepl("^S ?/ ?INFO$|^SEM ?INFO(RMACAO)?$", e) ~ "SEM INFORMAÇÃO",

    grepl("\\bSUPERIOR\\b\\s*(COMP|COMPL|COMPLETO)\\b|^SUPERIOR COMP$", e) ~ "SUPERIOR COMPLETO",
    grepl("\\bSUPERIOR\\b\\s*(INC|INCOMPL|INCOMPLETO)\\b|^SUPERIOR INC$", e) ~ "SUPERIOR INCOMPLETO",

    grepl("\\b(MEDIO|ENSINO MEDIO|2 ?O? ?GRAU|SEGUNDO GRAU)\\b\\s*(COMP|COMPL|COMPLETO)\\b|^MEDIO COMP$", e) ~ "MÉDIO COMPLETO",
    grepl("\\b(MEDIO|ENSINO MEDIO|2 ?O? ?GRAU|SEGUNDO GRAU)\\b\\s*(INC|INCOMPL|INCOMPLETO)\\b|^MEDIO INC$", e) ~ "MÉDIO INCOMPLETO",

    grepl("\\bFUND(AMENTAL)?\\b\\s*(INC|INCOMPL|INCOMPLETO)\\b|^FUND INC$", e) ~ "FUNDAMENTAL INCOMPLETO",
    grepl("\\bFUND(AMENTAL)?\\b\\s*(COMP|COMPL|COMPLETO)\\b|^FUND COMP$", e) ~ "FUNDAMENTAL COMPLETO",

    TRUE ~ "SEM INFORMAÇÃO"
  )
  df$ESC_CAT7 <- factor(
    df$ESC_CAT7,
    levels = c("SUPERIOR COMPLETO","SUPERIOR INCOMPLETO",
               "MÉDIO COMPLETO","MÉDIO INCOMPLETO",
               "FUNDAMENTAL INCOMPLETO","FUNDAMENTAL COMPLETO",
               "SEM INFORMAÇÃO")
  )
}

# -------- Ocupação (6) --------
if (!is.na(col$OCU_MAT)){
  o <- norm_txt(df[[col$OCU_MAT]])
  df$OCU_CAT6 <- dplyr::case_when(
    grepl("^DO ?LAR$", o) ~ "DO LAR",
    grepl("^ESTUDANTE$", o) ~ "ESTUDANTE",
    grepl("AUTONOM", o) ~ "AUTÔNOMA",
    grepl("VENDEDOR", o) ~ "VENDEDORA",
    grepl("DESEMPREG", o) ~ "DESEMPREGADA",
    is.na(o) ~ NA_character_,
    TRUE ~ "OUTRAS"
  )
}

# -------- Especificação (ordem) --------
spec <- list(
  list(type="ig_wd",    label="Idade gestacional, semanas", var="IG_weeks"),
  list(type="num",      label="Peso ao nascer,\ngramas",    var=col$PESO),
  list(type="apgar",    label="Apgar 1º min*",              var=col$APGAR1),
  list(type="apgar",    label="Apgar 5º min*",              var=col$APGAR5),

  list(type="pct_exact",label="Reanimação\nSim", var=col$RCP, level="SIM"),
  list(type="pct_exact",label="Reanimação\nNão", var=col$RCP, level="NÃO"),

  list(type="pct_exact",label="Sexo\nMasculino", var=col$SEXO, level="MASCULINO"),
  list(type="pct_exact",label="Sexo\nFeminino",  var=col$SEXO, level="FEMININO"),

  list(type="pct_exact",label="Suporte de O2\nSim", var=col$SUP_O2, level="SIM"),
  list(type="pct_exact",label="Suporte de O2\nNão", var=col$SUP_O2, level="NÃO"),

  list(type="pct_yes",  label="VM",            var=col$VM),
  list(type="pct_yes",  label="VNI",           var=col$VNI),
  list(type="pct_yes",  label="CNAF",          var=col$CNAF),
  list(type="pct_yes",  label="CPAP",          var=col$CPAP),
  list(type="pct_yes",  label="HOOD",          var=col$HOOD),
  list(type="pct_yes",  label="Cateter nasal", var=col$CNO2),

  list(type="pct_exact",label="Tipo de parto\nCesárea",        var="PARTO_CAT", level="CESÁREA"),
  list(type="pct_exact",label="Tipo de parto\nVaginal",        var="PARTO_CAT", level="VAGINAL"),
  list(type="pct_exact",label="Tipo de parto\nSem informação", var="PARTO_CAT", level="SEM INFORMAÇÃO"),

  list(type="pct_yes",  label="Hemorragia pulmonar", var=col$HEM_PULM),

  list(type="pct_exact",label="Hemorragia intracraniana*\nNão",    var="HEM_IC_GRAU", level="NÃO"),
  list(type="pct_exact",label="Hemorragia intracraniana*\nGrau I", var="HEM_IC_GRAU", level="GRAU I"),
  list(type="pct_exact",label="Hemorragia intracraniana*\nGrau II",var="HEM_IC_GRAU", level="GRAU II"),
  list(type="pct_exact",label="Hemorragia intracraniana*\nGrau III",var="HEM_IC_GRAU", level="GRAU III"),
  list(type="pct_exact",label="Hemorragia intracraniana*\nGrau IV", var="HEM_IC_GRAU", level="GRAU IV"),

  list(type="pct_yes",  label="Hipertensão pulmonar", var=col$HIPERT),
  list(type="pct_yes",  label="Pneumonia congênita",  var=col$PNEU_CONG),
  list(type="pct_yes",  label="Pneumonia adquirida",  var=col$PNEU_ADQ),
  list(type="pct_yes",  label="Enterocolite necrosante", var=col$ENT_NEC),
  list(type="pct_yes",  label="Infecção presumida",   var=col$INF_PRES),
  list(type="pct_yes",  label="Infecção tardia",      var=col$INF_TAR),
  list(type="pct_yes",  label="Sepse",                var=col$SEPSE),
  list(type="pct_yes",  label="Uso de antibióticos",  var=col$ATB),
  list(type="pct_yes",  label="Sífilis",              var=col$SIFILIS),

  list(type="num",      label="Características maternas\nIdade, anos", var=col$IDA_MAT),
  list(type="num",      label="N. de gestação prévia", var="GEST_PREV"),
  list(type="num",      label="N. consultas prenatal", var=col$CPN),

  # -------- Escolarização (usa contagem "strict", sem normalizar) --------
  list(type="pct_exact_strict",label="Escolarização\nSuperior completo",      var="ESC_CAT7", level="SUPERIOR COMPLETO"),
  list(type="pct_exact_strict",label="Escolarização\nSuperior incompleto",    var="ESC_CAT7", level="SUPERIOR INCOMPLETO"),
  list(type="pct_exact_strict",label="Escolarização\nMédio completo",         var="ESC_CAT7", level="MÉDIO COMPLETO"),
  list(type="pct_exact_strict",label="Escolarização\nMédio incompleto",       var="ESC_CAT7", level="MÉDIO INCOMPLETO"),
  list(type="pct_exact_strict",label="Escolarização\nFundamental incompleto", var="ESC_CAT7", level="FUNDAMENTAL INCOMPLETO"),
  list(type="pct_exact_strict",label="Escolarização\nFundamental completo",   var="ESC_CAT7", level="FUNDAMENTAL COMPLETO"),
  list(type="pct_exact_strict",label="Escolarização\nSem informação",         var="ESC_CAT7", level="SEM INFORMAÇÃO"),

  # -------- Ocupação --------
  list(type="pct_exact",label="Ocupação\nDo lar",        var="OCU_CAT6", level="DO LAR"),
  list(type="pct_exact",label="Ocupação\nEstudante",     var="OCU_CAT6", level="ESTUDANTE"),
  list(type="pct_exact",label="Ocupação\nAutônoma",      var="OCU_CAT6", level="AUTÔNOMA"),
  list(type="pct_exact",label="Ocupação\nVendedora",     var="OCU_CAT6", level="VENDEDORA"),
  list(type="pct_exact",label="Ocupação\nDesempregada",  var="OCU_CAT6", level="DESEMPREGADA"),
  list(type="pct_exact",label="Ocupação\nOutras",        var="OCU_CAT6", level="OUTRAS")
)

# -------- Construtores --------
sum_row <- function(label, values_named_vec){
  tibble::tibble(Variavel = label, !!!setNames(as.list(values_named_vec[group_cols]), group_cols))
}
build_row <- function(sp){
  lab <- sp$label; v <- sp$var
  if (is.null(v) || !v %in% names(df)) {
    return(sum_row(lab, setNames(rep("NA", length(group_cols)), group_cols)))
  }
  total_val <- switch(sp$type,
    "ig_wd"            = fmt_mean_sd_IG_WD(df[[v]]),
    "num"              = fmt_mean_sd_numeric(df[[v]]),
    "apgar"            = fmt_mean_sd_apgar(df[[v]]),
    "pct_exact"        = fmt_pct(df[[v]], level = sp$level),
    "pct_exact_strict" = fmt_pct_exact_strict(df[[v]], level = sp$level),
    "pct_yes"          = fmt_pct(df[[v]], yes_levels = c("SIM","YES","1")),
    "NA"
  )
  vals <- c("Total (N)" = total_val)
  for (g in lvl){
    sub <- dplyr::filter(df, Grupo == g)
    vals[g] <- switch(sp$type,
      "ig_wd"            = fmt_mean_sd_IG_WD(sub[[v]]),
      "num"              = fmt_mean_sd_numeric(sub[[v]]),
      "apgar"            = fmt_mean_sd_apgar(sub[[v]]),
      "pct_exact"        = fmt_pct(sub[[v]], level = sp$level),
      "pct_exact_strict" = fmt_pct_exact_strict(sub[[v]], level = sp$level),
      "pct_yes"          = fmt_pct(sub[[v]], yes_levels = c("SIM","YES","1")),
      "NA"
    )
  }
  sum_row(lab, vals)
}

# -------- Primeira linha: N --------
n_vals <- c("Total (N)" = as.character(nrow(df)))
for (g in lvl) n_vals[g] <- as.character(sum(df$Grupo == g, na.rm = TRUE))
tabela <- sum_row("N", n_vals)

# -------- Demais linhas --------
for (sp in spec) tabela <- dplyr::bind_rows(tabela, build_row(sp))

# -------- Exportações --------
readr::write_csv(tabela, out_csv)
writexl::write_xlsx(tabela, out_xlsx)

ft <- flextable::flextable(tabela) |> flextable::autofit() |> flextable::align(j=2:ncol(tabela), align="center", part="all")
doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Table 1. Clinical and sociodemographic characteristics (por grupo)", style="heading 1")
doc <- flextable::body_add_flextable(doc, ft)
doc <- officer::body_add_par(doc, "")
doc <- officer::body_add_par(doc, if (PERCENT_ONLY) "Média (dp) ou %; IG em W.D (semanas.dias)." else "Média (dp) ou n (%); IG em W.D (semanas.dias).")
print(doc, target = out_docx)

cat("✅ Gerado:\n- ", out_csv, "\n- ", out_xlsx, "\n- ", out_docx, "\n", sep = "")
