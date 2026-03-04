# voronoi_tesselation_plot

This repository contains R code and examples demonstrating Voronoi tessellation visualization of 2D raw or projected data, as presented in the accompanying scientific paper.

## Overview

Voronoi tessellation provides an intuitive visualization of class separation and decision boundaries in raw data, or projected data from dimensionality reduction techniques such as PCA, PLS-DA, UMAP etc. By dividing the plot space into regions based on proximity to data points, Voronoi diagrams effectively display clustering results and classification boundaries.

## Main Functions

The code provides two main functions:

### `create_tesselation_plots()`
Generates standard 2D plots with optional confidence ellipses and Voronoi tessellation overlays.

**Parameters:**
- `data`: Data frame with ≥2 numeric columns for coordinates
- `class_column`: Column name or vector of class labels
- `case_labels`: Optional individual case labels
- `show_labels`: Logical, whether to show the case labels
- `coord_names`: Names for coordinate axes (default: "Dim1", "Dim2")
- Additional aesthetic parameters for colors, sizes and styling

**Output:** List containing `scatter_plot`, `ellipse_plot`, `voronoi_plot`, and `voronoi_plot_plus_ellipse`

### `create_voronoi_plot()`
Generates Voronoi tessellation plots with optional dual classifications.

**Parameters:**
- `coordinate_columns`: Columns to use as coordinates
- `class_column`: Primary class classification
- `alternative_class_column`: Optional alternative classification
- `color_points`, `fill_voronoi`: Which classification to use for colors/fills
- `add_grid_lines`: Logical, whether to add origin grid lines
- Additional aesthetic parameters

**Output:** Single ggplot object with Voronoi tessellation

## Examples

### Iris dataset PCA visualization showcase

This example demonstrates a **Principal Component Analysis (PCA)** on the classic *Iris* dataset.  
The figure below shows the data projected into the first two principal components, with clusters and species classes highlighted for comparison.

![Iris PCA Plot](iris_pca_plot.svg)

**Legend**  
- **Points / Regions**: Individual flower samples projected in PCA space  
- **Colors**: Iris species (Setosa, Versicolor, Virginica)  
- **Boundaries**: Iris species  
- **Axes**: Principal components PC1 and PC2, capturing the majority of variance in the dataset

### Main examples

The repository includes example scripts demonstrating the application of Voronoi tessellation to various data types:

### Examples used in the paper:
- **`covid_metaboanalyst_example.R`**: COVID-19 metabolomics data from MetaboAnalyst (PCA projections)
- **`pain_data_example.R`**: Quantitative sensory testing pain data with multiple projection methods (PLS-DA, PCA, UMAP)
- **`PsA_data_example.R`**: Psoriatic arthritis lipidomics data (PLS-DA projections)

### Additional examples:
- **`Two_class_artifical_data_example.R`**: Artificial two-class data with controlled separation and scenarios with/without classification errors
- **`Three_class_artifical_data_example.R`**: Artificial three-class data demonstrating multi-class visualizations
- **`Lsun_example.R`**: FCPS Lsun dataset illustrating Voronoi tessellation with multiple clustering algorithms (k-means and single linkage)

## Data

The `data/` directory contains three datasets:

### Lsun_OriginalData.csv
Two-dimensional clustering benchmark with 400 samples in three classes (FCPS dataset). Used in `Lsun_example.R`.

**Reference**: Ultsch A, Lötsch J. The fundamental clustering and projection suite (FCPS): A dataset collection to test the performance of clustering and data projection algorithms. Data. 2020;5(1):13. doi: https://doi.org/10.3390/data5010013

### QSTpainEJP.csv
Quantitative sensory testing (QST) data with 22 pain measures from 72 healthy subjects. Used in `pain_data_example.R`.

**Public access**: https://data.mendeley.com/preview/9v8ndhctvz (DOI: 10.17632/9v8ndhctvz.1)

**References**: Lötsch J, Dimova V, Ultsch A, et al. Eur J Pain. 2016;20(5):777-89. doi: 10.1002/ejp.803 | Rolke R, Magerl W, Campbell KA, et al. Eur J Pain. 2006;10(1):77-88. doi: 10.1016/j.ejpain.2005.02.003

### covid_metabolomics_pca_score.csv
PCA scores from LC-MS metabolomics data of 59 samples (20 controls, 39 COVID-19 patients). Used in `covid_metaboanalyst_example.R`.

**Data source**: https://api2.xialab.ca/api/download/metaboanalyst/covid_metabolomics_data.csv

**Reference**: Pang Z, Lu Y, Zhou G, et al. MetaboAnalyst 6.0: towards a unified platform for metabolomics data processing, analysis and interpretation. Nucleic Acids Res. 2024;52(W1):W398-w406. doi: 10.1093/nar/gkae253

### Additional datasets
- **PsA lipidomics**: Used in `PsA_data_example.R`
- **Artificial data**: Generated in `Two_class_artifical_data_example.R` and `Three_class_artifical_data_example.R`

## Dependencies

The code requires:
- `ggplot2` (plotting)
- `ggthemes` (colorblind-friendly palettes)
- `cowplot` (plot arrangement)
- `deldir` (Voronoi tessellation computation)
- `ggrepel` (smart label positioning)
- `MASS` (ellipse computation)

Optional packages for specific examples:
- `mixOmics` (PLS-DA and PCA projections)
- `umap` (UMAP dimensionality reduction)

## Note on the VoronoiBiomedPlot R Package

This code repository also serves as the basis for the published R package [VoronoiBiomedPlot](https://cran.r-project.org/package=VoronoiBiomedPlot), available on CRAN, which provides a packaged version of these functions for convenient installation and long-term maintenance.


You can install it from CRAN:

```r
install.packages("VoronoiBiomedPlot")
```


## License

CC-BY 4.0

## Citation

Jorn Lotsch and Dario Kringel (2026). Voronoi tessellation as a complement or replacement for confidence ellipses in the visualization of data projection and clustering results. *PLoS One* (in revision) 



