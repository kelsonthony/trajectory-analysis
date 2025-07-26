## Projeto: Análise de Trajetórias de Neonatos

Este projeto realiza análises descritivas e de agrupamento das trajetórias semanais de neonatos internados, utilizando o pacote `TraMineR` e outras bibliotecas em R.

---

## 📦 Requisitos

### ✅ Software Necessário

* [R](https://cran.r-project.org/) (>= 4.0)
* [RStudio](https://posit.co/download/rstudio-desktop/) (recomendado)
* [Git](https://git-scm.com/) (opcional para clonar o repositório)

---

## 📁 Organização do Projeto

```
trajectory-analysis/
│
├── data/
│   └── input/           # Arquivo XLSX original com dados
│   └── output/          # Resultados em CSV, XLSX e PNG
│
├── scripts/
│   ├── 01_generate_trajectory.R
│   ├── 02_analysis_cluster.R
│   ├── 03_analysis_descriptive.R
│   ├── 04_group_analysis.R
│   ├── 05_generate_graphics.R
│   └── 06_cluster_time_profile.R
│
├── main.R               # Orquestra todos os scripts
├── renv.lock            # Arquivo de lock de pacotes
├── .Rprofile            # Ativa renv automaticamente
└── DOCUMENTATION.md     # Este documento
```

---

## 🚀 Como Executar

### 1. Clone o projeto

```bash
git clone https://github.com/seu-usuario/trajectory-analysis.git
cd trajectory-analysis
```

### 2. Inicie o R ou RStudio no diretório do projeto

```r
setwd("CAMINHO/para/trajectory-analysis")  # se estiver fora do projeto
```

### 3. Inicialize o `renv` e instale os pacotes

```r
install.packages("renv")
renv::restore()  # Instala todos os pacotes com base no renv.lock
```

> 💡 Caso prefira instalar manualmente, veja a lista de pacotes abaixo.

---

## 📚 Pacotes R Utilizados

Estes pacotes são automaticamente gerenciados por `renv`, mas você pode instalá-los manualmente com:

```r
install.packages(c(
  "data.table", "dplyr", "tidyr", "ggplot2", "openxlsx", "readxl",
  "cluster", "factoextra", "TraMineR", "reshape2"
))
```

---

## ▶️ Executar o pipeline completo

No console do R:

```r
source("main.R", encoding = "UTF-8")
```

> Isso irá:
>
> * Ler o Excel de entrada em `data/input/`
> * Gerar a base de trajetória em CSV/XLSX
> * Executar cluster de sequência (TraMineR)
> * Gerar gráficos de desfecho
> * Executar análise de grupo
> * Gerar gráficos complementares
> * Salvar os resultados em `data/output/`

---

## 📈 Resultados Esperados

Após execução, os seguintes arquivos serão gerados na pasta `data/output/`:

* `trajetoria_YYYYMMDD_HHMM.csv`
* `Trajetoria_neonatos_cluster_YYYYMMDD_HHMM.csv`
* `agrupamento_por_tempo_YYYYMMDD_HHMM.csv`
* Gráficos `.png` de trajetórias, barras, pizza, Gantt, heatmap, etc.

---

## 💡 Dicas

* Rode `renv::dependencies()` para checar quais pacotes estão sendo usados.
* Rode `renv::snapshot()` caso adicione novos pacotes ao projeto.
* Para executar scripts individualmente, use por exemplo:

```r
source("scripts/03_analysis_descriptive.R", encoding = "UTF-8")
```

---

## 🧼 Para reiniciar do zero

Se precisar limpar o ambiente:

```r
rm(list = ls())
renv::restore()
source("main.R", encoding = "UTF-8")
```

---

Se quiser, posso salvar o arquivo como `DOCUMENTATION.md` pronto para download. Deseja isso?
