############### Libraries ##############################
library(ggplot2)
library(ggthemes)
library(cowplot)

source("create_voronoi_plot.R")

############### Functions ##############################
# Basic function to extend subtitles
add_subtitle_prefix <- function(p, extra) {
  old_sub <- p$labels$subtitle
  # handle missing subtitle
  if (is.null(old_sub)) old_sub <- ""

  new_sub <- if (nzchar(old_sub)) {
    paste0(extra, ": ", old_sub)
  } else {
    extra
  }

  p + labs(subtitle = new_sub)
}

############### Read the Lsun data ##############################

lsun_data <- read.csv("data/Lsun_OriginalData.csv", row.names = 1)
names(lsun_data) <- c("class", "x", "y")

############### Data clustering ##############################
# Basic scatter plot with original classes
plot(
  lsun_data[, 2] ~ lsun_data[, 3],
  col = lsun_data$class
)

# Perform k-means clustering
set.seed(42)
k2 <- kmeans(
  scale(lsun_data[, 2:3]),
  centers = 3,
  nstart = 100
)

lsun_data$kmeans_clusters <- k2$cluster

# Perform single linkage clustering
Lsun_single <- hclust(dist(scale(lsun_data[, 2:3])), method = "single")
lsun_data$single_clusters <- cutree(Lsun_single, k = 3)

# Plot with k-means and single clusters
par(mfrow = c(2, 1))
plot(
  lsun_data[, 2] ~ lsun_data[, 3],
  col = lsun_data$kmeans_clusters
)
plot(
  lsun_data[, 2] ~ lsun_data[, 3],
  col = lsun_data$single_clusters
)
par(mfrow = c(1, 1))

############### Create Voronoi plots ##############################
lsun_orig_classes_plot <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "class",
  add_grid_lines = FALSE,
  show_island_count = TRUE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun original classes"
  )

lsun_orig_classes_plot <- add_subtitle_prefix(
  lsun_orig_classes_plot,
  "Points and cells colored as prior classes"
)

lsun_kmeans_plot <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "class",
  alternative_class_column = "kmeans_clusters",
  add_grid_lines = FALSE,
  fill_voronoi = "alternative",
  show_island_count = TRUE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun k-means clusters"
  )

lsun_kmeans_plot <- add_subtitle_prefix(
  lsun_kmeans_plot,
  "Points colored as prior classes, cells as clusters"
)


lsun_kmeans_plot_variant2 <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "kmeans_clusters",
  alternative_class_column = "class",
  add_grid_lines = FALSE,
  show_island_count = TRUE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun k-means clusters"
  )

lsun_kmeans_plot_variant2 <- add_subtitle_prefix(
  lsun_kmeans_plot_variant2,
  "Points and cells colored as clusters")

lsun_single_plot <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "class",
  alternative_class_column = "single_clusters",
  add_grid_lines = FALSE,
  fill_voronoi = "alternative",
  show_island_count = TRUE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun single linkage clusters"
  )

lsun_single_plot <- add_subtitle_prefix(
  lsun_single_plot,
  "Points colored as prior classes, cells as clusters")

lsun_single_plot_variant2 <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "single_clusters",
  alternative_class_column = "class",
  add_grid_lines = FALSE,
  show_island_count = TRUE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun single linkage clusters"
  )

lsun_single_plot_variant2 <- add_subtitle_prefix(
  lsun_single_plot_variant2,
  "Points and cells colored as clusters")


############### Combine and save plots ##############################
# Combine plots
lsun_combined <- cowplot::plot_grid(
  lsun_orig_classes_plot,
  lsun_kmeans_plot,
  lsun_kmeans_plot_variant2,
  "blank",
  lsun_single_plot,
  lsun_single_plot_variant2,
  labels = c("A", "B", "C", "", "D", "E"),
  nrow = 2
)
print(lsun_combined)

# Save combined plot
ggsave(
  filename = "Lsun_combined.svg",
  plot = lsun_combined,
  width = 18,
  height = 14
)
