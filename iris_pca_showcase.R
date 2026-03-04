# --- Libraries ---
library(ggplot2)
library(ggfortify)           # for autoplot(prcomp(...))
library(ggthemes)            # for scale_color_colorblind, scale_fill_colorblind
library(VoronoiBiomedPlot)   # for create_voronoi_plot()
# If iris is not already available (e.g., in a minimal environment), load datasets:
# library(datasets)

# --- Iris Dataset: PCA and Clustering Visualization ---

# Load and prepare input data
# We use only the numeric feature columns (1–4) from the classic iris dataset.
df_iris <- iris[, 1:4]

# --- Principal Component Analysis (PCA) ---
# Perform PCA with scaling to standardize features (mean = 0, sd = 1)
pca_res_iris <- prcomp(df_iris, scale. = TRUE)

# Visualize PCA results with loading vectors and variable labels
autoplot(pca_res_iris, loadings = TRUE, loadings.label = TRUE)

# Optionally, visualize percentage of variance explained by each principal component
autoplot(pca_res_iris, variance_percentage = TRUE)

# Determine number of principal components (PCs) to retain.
# Here, we keep at least 2 PCs or all with eigenvalue > 1 (using the Kaiser criterion).
nPC <- max(2, sum(pca_res_iris$sdev ^ 2 > 1))

# --- Hierarchical Clustering (Ward’s method) ---
# Cluster data in the reduced PCA space using Euclidean distance.
iris_ward <- hclust(dist(pca_res_iris$x[, 1:nPC]), method = "ward.D2")

# Plot the resulting dendrogram to visualize cluster structure.
plot(iris_ward)

# Cut the dendrogram to obtain 2 clusters (for simplicity)
ward_clusters <- cutree(iris_ward, 2)

# --- Statistical Comparison Between Clusters ---
# For each feature, compute the Wilcoxon test between clusters.
# Convert p-values to -log10 scale for visual emphasis.
wilcox.p <- -log10(apply(df_iris, 2, function(x) wilcox.test(x ~ ward_clusters)$p.value))

# Visualize feature importance using a bar plot of -log10(p-values)
barplot(sort(wilcox.p), las = 2, main = "Feature significance between clusters")

# Combine cluster assignments, class labels, and PCA coordinates for visualization
iris_clusters <- cbind.data.frame(
  cluster = ward_clusters,
  class = iris[, 5],
  pca_res_iris$x[, 1:nPC]
)

# --- Custom Voronoi Plot Visualization ---
# Create a Voronoi diagram to visualize PCA clusters and true iris classes
iris_pca_plot <- create_voronoi_plot(
  data = iris_clusters,
  class_column = "class",
  coordinate_columns = c("PC1", "PC2"),
  case_labels = rownames(df_iris),
  alternative_class_column = "class",
  show_labels = TRUE,
  fill_voronoi = "alternative",
  show_island_count = TRUE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(title = "Iris Dataset PCA Projection")

# Print final PCA plot
print(iris_pca_plot)

# --- Export Results ---
# Save the final PCA plot as an SVG file for use in reports or repositories
ggsave(
  filename = "iris_pca_plot.svg",
  plot = iris_pca_plot,
  width = 10,
  height = 10
)
