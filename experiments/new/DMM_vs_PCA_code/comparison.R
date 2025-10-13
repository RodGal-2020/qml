# We want to join both plots. They are both ggplot objects
# pca_plot
# dmm_plot
library(patchwork)

# Combine both plots using patchwork
pca_plot <- pca_plot + theme(legend.position = "none")
comp_plot <- (pca_plot | dmm_plot) +
  plot_layout(guides = "collect") &  # Collect guides (legend)
  theme(legend.position = "bottom")  # Place legend at the bottom
comp_plot

# Now with the jitter plots: dmm_plot_jit, pca_plot_jit
dmm_plot_jit <- dmm_plot_jit + theme(legend.position = "none")
comp_plot_jit <- (pca_plot_jit | dmm_plot_jit) +
  plot_layout(guides = "collect") &  # Collect guides (legend)
  theme(legend.position = "bottom")  # Place legend at the bottom
comp_plot_jit
