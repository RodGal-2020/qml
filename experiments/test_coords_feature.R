# Test script for the new polar coordinates feature in predict.dm_fit()
# 
# This script demonstrates how to:
# 1. Fit a quantum matrix model
# 2. Make predictions with polar coordinates
# 3. Create visualizations using the coordinates

# Load the package
library(qml)
# Print package version
cat("Using qml package version:", as.character(packageVersion("qml")), "\n")

library(ggplot2)
library(dplyr)

# Prepare iris data for binary classification
iris_binary <- iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = droplevels(Species))

# Split data
set.seed(123)
train_indices <- sample(nrow(iris_binary), 0.7 * nrow(iris_binary))
train_data <- iris_binary[train_indices, ]
test_data <- iris_binary[-train_indices, ]

# Fit model
cat("Fitting quantum matrix model...\n")
model <- dm_fit(Species ~ ., data = train_data, verbose = 1)

# Standard predictions (without coordinates)
cat("\nStandard predictions:\n")
pred_standard <- predict(model, test_data, type = "class")
print(head(pred_standard))

# Predictions with probabilities (without coordinates)
cat("\nProbability predictions:\n")
pred_prob <- predict(model, test_data, type = "prob")
print(head(pred_prob))

# NEW FEATURE: Predictions with polar coordinates
cat("\nPredictions with polar coordinates:\n")
pred_with_coords <- predict(model, test_data, type = "class", return_coords = TRUE)
cat("Structure of result:\n")
str(pred_with_coords)

cat("\nPolar coordinates:\n")
print(head(pred_with_coords$coords))

# Create visualization using polar coordinates
if (nrow(pred_with_coords$coords) > 0) {
  cat("\nCreating visualization with polar coordinates...\n")
  
  # Prepare data for plotting
  plot_data <- pred_with_coords$coords %>%
    mutate(
      predicted_class = as.character(pred_with_coords$predictions),
      actual_class = as.character(test_data$Species)
    )
  
  # Create polar coordinate plot
  p1 <- ggplot(plot_data, aes(x = r, y = phi_1, color = predicted_class)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = "Test Data in Quantum Matrix Polar Coordinates",
      subtitle = "Visualization of test data points in (r, φ) space",
      x = "Radial coordinate (r)",
      y = "Angular coordinate (φ₁)",
      color = "Predicted Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  print(p1)
  
  # Create polar plot (actual polar coordinates)
  p2 <- ggplot(plot_data, aes(x = phi_1, y = r, color = predicted_class)) +
    geom_point(size = 3, alpha = 0.7) +
    coord_polar(theta = "x") +
    labs(
      title = "DM: Test Data in Polar Coordinate System",
      subtitle = "True polar visualization of quantum matrix coordinates",
      color = "Predicted Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_blank()
    )
  
  print(p2)
  
  # Comparison with actual classes
  p3 <- ggplot(plot_data, aes(x = r, y = phi_1)) +
    geom_point(aes(color = predicted_class, shape = actual_class), size = 3, alpha = 0.7) +
    labs(
      title = "Prediction Accuracy in Quantum Matrix Space",
      subtitle = "Color = Predicted, Shape = Actual",
      x = "Radial coordinate (r)",
      y = "Angular coordinate (φ₁)",
      color = "Predicted",
      shape = "Actual"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  print(p3)
  
} else {
  cat("No coordinates to plot (empty prediction set)\n")
}

# Test with probability predictions and coordinates
cat("\nProbability predictions with coordinates:\n")
pred_prob_coords <- predict(model, test_data, type = "prob", return_coords = TRUE)
cat("Probability predictions:\n")
print(head(pred_prob_coords$predictions))
cat("Coordinates (should be same as above):\n")
print(head(pred_prob_coords$coords))

cat("\nTest completed successfully!\n")
cat("The predict() function now supports return_coords=TRUE parameter\n")
cat("for visualizing test data in quantum matrix polar coordinate space.\n")
