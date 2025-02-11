qmat_plot <- qmats[[1]] %>%
  ggplot(aes(x = r, y = phi_1, color = .truth, shape = .pred)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Classification Example",
    # subtitle = .y,
    x = "",
    y = ""
  ) +
  coord_polar(theta = "y", start = 0) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.text.y = element_blank())    # Move legend to the bottom for better visibility
