############### Libraries ##############################
library(mixOmics)
library(umap)
library(uwot)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(ggfortify)
library(ggthemes)
library(parallel)
library(pbmcapply)

############### Constants ##############################
PROJECTION_TYPE <- "PLS-DA" # Default projection type

############### Functions ##############################
perform_projection_analysis <- function(data_frame, projection_method = PROJECTION_TYPE) {

  # UMAP settings
  custom.settings <- umap.defaults
  # custom.settings$n_neighbors = 20

  switch(projection_method,
         "PLS-DA" = mixOmics::plsda(X = data_frame[, -1], Y = data_frame$class, scale = TRUE, ncomp = 2),
         "PCA" = mixOmics::pca(X = data_frame[, -1], scale = TRUE),
         "UMAP" = umap::umap(scale(data_frame[, -1]), config = custom.settings)$layout,
  # "UMAP" = uwot::umap( scale(data_frame[,-1]), n_neighbors = 15, n_trees = 50),
         mixOmics::plsda(X = data_frame[, -1], Y = data_frame$class, scale = TRUE))
}

source("create_tesselation_plots.R")

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

############### Read the pain data ##############################

pain_tests_data_complete <- read.csv("data/QSTpainEJP.csv", row.names = 1)
dim(pain_tests_data_complete)
table(pain_tests_data_complete$class)

############### Perform different projections ##############################

projection_methods <- c("PLS-DA", "PCA", "UMAP")
pain_projections_and_plots <- pbmcapply::pbmclapply(projection_methods, function(projection_method) {

  ############### Project and plot the data ##############################

  set.seed(42)
  proj_pain_tests_data_complete <- perform_projection_analysis(
    data_frame = pain_tests_data_complete,
    projection_method = projection_method
  )

  if (!projection_method == "UMAP") {
    proj_pain_tests_data_complete_data <- mixOmics::plotIndiv(
      proj_pain_tests_data_complete,
      ellipse = FALSE,
      legend = TRUE,
      style = "graphics"
    )
    projection_data <- proj_pain_tests_data_complete_data$df
    if (!projection_method == "PLS-DA") projection_data$group <- pain_tests_data_complete$class
  } else {
    projection_data <- as.data.frame(proj_pain_tests_data_complete)
    names(projection_data) <- c("x", "y")
    projection_data$group <- pain_tests_data_complete$class
  }

  pain_tests_data_complete_plot <- create_tesselation_plots(
    data = projection_data,
    class_column = "group",
    case_labels = rownames(pain_tests_data_complete),
    show_labels = TRUE,
    show_island_count = TRUE
  )

  # Create individual plots
  pain_ellipse_plot <- pain_tests_data_complete_plot$ellipse_plot +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    labs(
      title = paste0("Pain data: ", projection_method, " projection"),
      subtitle = "Confidence ellipses for prior classes"
    )

  pain_voronoi_plot <- pain_tests_data_complete_plot$voronoi_plot +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    labs(
      title = paste0("Pain data: ", projection_method, " projection")
    )

  pain_voronoi_plot <- add_subtitle_prefix(
    pain_voronoi_plot,
    "Voronoi tesselation for prior classes"
  )

  pain_voronoi_ellipse_plot <- pain_tests_data_complete_plot$voronoi_plot_plus_ellipse +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    labs(
      title = paste0("Pain data: ", projection_method, " projection")
    )

  pain_voronoi_ellipse_plot <- add_subtitle_prefix(
    pain_voronoi_ellipse_plot,
    "Voronoi tesselation and confidence ellipses for prior classes"
  )

  return(list(
    pain_ellipse_plot = pain_ellipse_plot,
    pain_ellipse_plot = pain_ellipse_plot,
    pain_voronoi_plot = pain_voronoi_plot,
    pain_voronoi_ellipse_plot = pain_voronoi_ellipse_plot
    ))
}, mc.cores = min(length(projection_methods), parallel::detectCores() - 1))

names(pain_projections_and_plots) <- projection_methods

# Combine plots
pain_combined_visualization_6 <- cowplot::plot_grid(
  pain_projections_and_plots$`PLS-DA`$pain_ellipse_plot,
  pain_projections_and_plots$`PLS-DA`$pain_voronoi_plot,
  pain_projections_and_plots$`PLS-DA`$pain_voronoi_ellipse_plot,
  # pain_projections_and_plots$PCA$pain_ellipse_plot,
  # pain_projections_and_plots$PCA$pain_voronoi_plot,
  # pain_projections_and_plots$PCA$pain_voronoi_ellipse_plot,
  pain_projections_and_plots$UMAP$pain_ellipse_plot,
  pain_projections_and_plots$UMAP$pain_voronoi_plot,
  pain_projections_and_plots$UMAP$pain_voronoi_ellipse_plot,
  labels = "AUTO",
  nrow = 2 
)
print(pain_combined_visualization_6)

ggsave(
  filename = "pain_combined_visualization_6.svg",
  plot = pain_combined_visualization_6,
  width = 18.5,
  height = 14.5
)

############### PCA and Ward - Test only ##############################

# df <- pain_tests_data_complete[, -1]
# pca_res <- prcomp(df, scale. = TRUE)
# autoplot(pca_res, loadings = TRUE, loadings.label = TRUE)
# autoplot(pca_res, variance_percentage = TRUE)
# 
# # PCA
# nPC <- sum(pca_res$sdev ^ 2 > 1)
# 
# # Ward's Method
# pain_ward <- hclust(dist(pca_res$x[, 1:nPC]), method = "ward.D2")
# plot(pain_ward)
# ward_clusters <- cutree(pain_ward, 2)
# 
# wilcox.p <- -log10(apply(pain_tests_data_complete[, -1], 2, function(x) wilcox.test(x ~ ward_clusters)$p.value))
# barplot(sort(wilcox.p), las = 2)
# pain_tests_data_complete_clusters <- cbind.data.frame(
#   cluster = ward_clusters,
#   class = pain_tests_data_complete$class,
#   pca_res$x[, 1:nPC]
# )
# 
# pain_pca_plot <- create_tesselation_plots(
#   data = pain_tests_data_complete_clusters,
#   class_column = "cluster",
#   coordinate_columns = c("PC1", "PC2"),
#   case_labels = rownames(pain_tests_data_complete_clusters),
#   alternative_class_column = "class",
#   show_labels = TRUE,
#   fill_voronoi = "alternative",
#   show_island_count = TRUE
# )
# 
# print(pain_pca_plot)
