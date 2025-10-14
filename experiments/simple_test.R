# Simple test to verify the quantum matrix classifier works
# Run this in R step by step

# Test loading libraries
library(dplyr)
library(magrittr) 
library(purrr)
library(tibble)

# Test loading the modules
source("R/dm_utils.R")
source("R/data_preprocessing.R")  
source("R/quantum_matrix.R")
source("R/kernel_functions.R")
source("R/coordinate_transforms.R")
source("R/dm_classifier.R")

# Simple test with iris
data(iris)
head(iris)

# Test the testify function
# iris_split <- testify(iris, "Species", test_prob = 0.3)
# table(iris_split$test, iris_split$Species)

# Test fitting a model
# Option 1: Manual droplevels (explicit approach)
filtered_iris <- iris %>% 
  filter(Species != "setosa") %>%
  mutate(Species = droplevels(Species))

# Option 2: dm_fit now automatically handles unused levels (with debugging)
filtered_data <- iris %>% filter(Species != "setosa")
cat("Filtered data dimensions:", dim(filtered_data), "\n")
cat("Classes in filtered data:", levels(filtered_data$Species), "\n")
cat("Class counts:", table(filtered_data$Species), "\n")

model <- dm_fit(Species ~ ., data = filtered_data, n_breaks = 3)
print(model)

set.seed(1974)
new_data <- filtered_data %>% sample_n(10)

# Test predictions
predictions <- predict(model, newdata = new_data)
print(predictions)

probabilities <- predict(model, newdata = new_data, type = "prob")
print(probabilities)
