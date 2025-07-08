# Neonatal Trajectory Analysis with TraMineR

This repository contains the complete pipeline for analyzing longitudinal care trajectories of newborns using the [TraMineR](http://traminer.unige.ch) package in R. The goal is to identify and visualize distinct patterns of healthcare utilization across 30 weeks after birth, and classify neonates into meaningful trajectory clusters.

---

## ⚠️ Important Note

> **The input files (`Trajetoria_neonatos.csv` and any `.xlsx` versions) are not included in this repository** due to data privacy and ethical considerations. You must provide your own input file with the same structure.

---

## 🧠 Objective

To perform a **sequence analysis** of weekly care states (e.g., NICU, intermediate care, discharge, death) in a cohort of neonates using:

- Optimal Matching (OM) distance computation  
- Hierarchical clustering (Ward's method)  
- Automatic cluster number selection using **Silhouette** method  
- Graphical visualization of state distributions and trajectories  

---

## 📊 Data Description

The input dataset `Trajetoria_neonatos.csv` must contain:

- **One row per individual**  
- **30 columns** representing the care state of the newborn for each of the first 30 weeks of life  
- Possible **states**:
  - `UTIN`: Neonatal Intensive Care Unit  
  - `UCINCO`: Intermediate Care (Conventional)  
  - `UCINCA`: Intermediate Care (Kangaroo)  
  - `ENF/ALCON`: Rooming-in / Clinical Ward  
  - `ALTA`: Discharge  
  - `ÓBITO`: Death

---

## 🧪 Methodology

### 1. Sequence Definition  
Using `seqdef()` from TraMineR to define weekly care trajectories.

### 2. Distance Calculation  
Pairwise distances using **Optimal Matching (OM)** with constant substitution costs.

```r
dist_mat <- seqdist(seq_obj, method = "OM", indel = 1, sm = "CONSTANT")

```

### 3. Hierarchical Clustering

With Ward's method on the dissimilarity matrix via `agnes()`.

### 4. Optimal Number of Clusters

Automatically selected using the **Silhouette method**:

```r
fviz_nbclust(as.matrix(dist_mat), FUN = hcut, method = "silhouette", k.max = 6)
```

### 5. Cluster Assignment

The optimal number of clusters is applied using:

```r
traj$group <- cutree(agnes_fit, k = optimal_k)
```

---

## 📈 Output Visualizations

All output images are saved in the `saida-img/` folder:

* `plot_distribuicao_geral.png`: Overall state distribution by week
* `dendrograma_trajetorias.png`: Dendrogram of trajectory distances
* `trajetorias_por_grupo.png`: State evolution by cluster
* `heatmap_por_grupo.png`: Heatmap of state frequencies per group
* `curva_acumulada_estados.png`: Cumulative distribution of states over time

---

## 🧾 Output Dataset

A file `Trajetoria_neonatos_cluster.csv` is generated, containing the original trajectories with an additional `grupo` column identifying cluster membership.

---

## 🔧 Requirements

Install the required R packages:

```r
install.packages(c("TraMineR", "data.table", "cluster", "factoextra", "ggplot2", "reshape2", "dplyr"))
```

---

## 🗂️ Folder Structure

```
📁 your-project/
├── Trajetoria_neonatos.csv              # 🔒 Not included in the repo
├── analise_grupo.R
├── Trajetoria_neonatos_cluster.csv
└── saida-img/
    ├── plot_distribuicao_geral.png
    ├── dendrograma_trajetorias.png
    ├── trajetorias_por_grupo.png
    ├── heatmap_por_grupo.png
    └── curva_acumulada_estados.png
```

---

## 📚 References

* Gabadinho, A., Ritschard, G., Müller, N.S., & Studer, M. (2011). [Analyzing and visualizing state sequences in R with TraMineR](https://www.jstatsoft.org/article/view/v040i04). *Journal of Statistical Software*, 40(4).
* Studer, M. (2013). *WeightedCluster library manual: A practical guide to creating typologies of trajectories in the social sciences with R*.

---

## 📬 Contact

For questions, feedback, or collaboration, please contact \[[k.thony@gmail.com](mailto:your_email@example.com)].