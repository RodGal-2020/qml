# Example: PCA in R
# data(iris)
# head(iris[, -5])

# Standardize the data
iris_scaled <- Data$original %>%
  select(-Class) %>%
  scale()

# Compute PCA
pca_result <- prcomp(iris_scaled)

# Summary of PCA
summary(pca_result)

# Extract PCA results
pca_data <- as.data.frame(pca_result$x) %>%  # Principal components (scores)
  bind_cols(Data$original %>% select(Class))  # Add class labels
loadings <- as.data.frame(pca_result$rotation)  # Loadings (eigenvectors)
explained_variance <- summary(pca_result)$importance[2, ] * 100  # % variance explained

# Add sample labels (optional, adjust if needed)
pca_data$Sample <- rownames(pca_data)

pca_plot = ggplot() +
  # Plot scores (data points)
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = Class), alpha = 0.7, size = 3) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * max(abs(pca_data$PC1)), yend = PC2 * max(abs(pca_data$PC2))),
               arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.8) +
  ggrepel::geom_text_repel(data = loadings, aes(x = PC1 * max(abs(pca_data$PC1)), y = PC2 * max(abs(pca_data$PC2)), label = rownames(loadings)), color = "blue", size = 4) +

  # Add axis labels
  labs(
    x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
    y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)"),
    title = "PCA Biplot",
    subtitle = "Scores and Loadings"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

pca_plot

################################################################################
# The same with jitter of 0.10 instead of geom_point()
pca_plot_jit = ggplot() +
  geom_jitter(data = pca_data, aes(x = PC1, y = PC2, color = Class), alpha = 0.7, size = 3, width = 0.10) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * max(abs(pca_data$PC1)), yend = PC2 * max(abs(pca_data$PC2))),
               arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.8) +
  ggrepel::geom_text_repel(data = loadings, aes(x = PC1 * max(abs(pca_data$PC1)), y = PC2 * max(abs(pca_data$PC2)), label = rownames(loadings)), color = "blue", size = 4) +
  labs(
    x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
    y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)"),
    title = "PCA Biplot",
    subtitle = "Scores and Loadings"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

pca_plot_jit

