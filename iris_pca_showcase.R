# --- Libraries ---
library(ggplot2)
library(ggfortify) # for autoplot(prcomp(...))
library(VoronoiBiomedPlot) # for create_voronoi_plot()
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

# Cut the dendrogram to obtain 3 clusters (for simplicity)
ward_clusters <- cutree(iris_ward, 3)

# --- Statistical Comparison Between Clusters ---
# For each feature, compute the Wilcoxon test between clusters.
# Convert p-values to -log10 scale for visual emphasis.
if (length(unique(ward_clusters)) > 1) {
  if (length(unique(ward_clusters)) == 2) {
    cluster_p <- -log10(apply(df_iris, 2, function(x) wilcox.test(x ~ ward_clusters)$p.value))
  } else {
    cluster_p <- -log10(apply(df_iris, 2, function(x) kruskal.test(x ~ ward_clusters)$p.value))
  }
  # Visualize feature importance using a bar plot of -log10(p-values)
  barplot(sort(cluster_p), las = 2, main = "Feature significance between clusters", ylab = "-log10(p)")
  abline(h = -log10(0.05), lty = 2, col = "red")
}

# Combine cluster assignments, class labels, and PCA coordinates for visualization
iris_clusters <- cbind.data.frame(
  cluster = ward_clusters,
  class = iris[, 5],
  pca_res_iris$x[, 1:nPC]
)

# --- Custom Voronoi Plot Visualization ---
# Create a Voronoi diagram to visualize true iris classes on the PCA projection
set.seed(42)
iris_pca_plot_prior_classes <- create_voronoi_plot(
  data = iris_clusters,
  class_column = "class",
  coordinate_columns = c("PC1", "PC2"),
  case_labels = rownames(df_iris),
  alternative_class_column = "class",
  show_labels = TRUE,
  fill_voronoi = "primary",
  show_island_count = TRUE
) +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  labs(title = "Iris dataset PCA projection: Cells and points colored for original species")

# Print PCA plot 1
print(iris_pca_plot_prior_classes)

# Create a Voronoi diagram to visualize PCA clusters and true iris classes
set.seed(42)
iris_pca_plot_clusters <- create_voronoi_plot(
  data = iris_clusters,
  class_column = "cluster",
  coordinate_columns = c("PC1", "PC2"),
  case_labels = rownames(df_iris),
  alternative_class_column = "class",
  show_labels = TRUE,
  fill_voronoi = "alternative",
  color_points = "primary",
  show_island_count = TRUE
) +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  labs(title = "Iris Dataset PCA Projection: Cells colored for original species, points colored for Ward clusters")

# Print PCA plot 2
print(iris_pca_plot_clusters)

# Create a Voronoi diagram to visualize PCA clusters and true iris classes
set.seed(42)
iris_pca_plot_clusters2 <- create_voronoi_plot(
  data = iris_clusters,
  class_column = "cluster",
  coordinate_columns = c("PC1", "PC2"),
  case_labels = rownames(df_iris),
  alternative_class_column = "class",
  show_labels = TRUE,
  fill_voronoi = "primary",
  color_points = "alternative",
  show_island_count = TRUE
) +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  labs(title = "Iris Dataset PCA Projection: Cells colored for Ward clusters, points colored for original species")

# Print PCA plot 2
print(iris_pca_plot_clusters2)

# Combine plots into a final plot
iris_pca_plot <- cowplot::plot_grid(iris_pca_plot_prior_classes, iris_pca_plot_clusters, iris_pca_plot_clusters2, nrow = 1)

# Print final PCA plot
print(iris_pca_plot)

# --- Export Results ---
# Save the final PCA plot as an SVG file for use in reports or repositories
ggsave(
  filename = "iris_pca_plot.svg",
  plot = iris_pca_plot,
  width = 30,
  height = 10
)
