# ⚛ Quantum Machine Learning (qml)

A modern R package implementing quantum-inspired machine learning algorithms, featuring the **Density Matrix (DM) method** for classification. This package provides a quantum-inspired alternative to traditional dimensionality reduction and classification methods like PCA.

## 🌟 Features

- **🔬 Quantum-Inspired Classification**: Novel approach using quantum density matrices and polar coordinate transformations
- **🎯 Tidymodels Integration**: Full compatibility with tidymodels ecosystem (`fit()`, `predict()`, workflows)
- **📊 Comparative Analysis**: Built-in tools to compare with traditional methods (PCA, logistic regression, etc.)
- **🔧 Robust Implementation**: S3 methods with comprehensive error handling and validation  
- **📈 Visualization Support**: Specialized plotting functions for quantum coordinate spaces
- **⚡ Modern Interface**: Clean, intuitive API following R best practices

## 📦 Installation

### From GitHub (Development Version)

```r
# Install devtools if needed
install.packages("devtools")

# Install qml from GitHub
devtools::install_github("RodGal-2020/qml")

# Load the package
library(qml)
```

### Local Development

```r
# Clone repository and install locally
git clone https://github.com/RodGal-2020/qml.git
cd qml

# In R/RStudio:
devtools::install_local(".", force = TRUE)
library(qml)
```

### Dependencies

The package automatically handles dependencies:

- **Core tidyverse**: dplyr, magrittr, purrr, tibble, rlang
- **Statistical testing**: fasano.franceschini.test  
- **Development**: lifecycle
- **Integration**: Works seamlessly with tidymodels ecosystem

## 🚀 Quick Start

### Basic Usage

```r
library(qml)
library(rsample)
library(yardstick)

# Load example data (binary classification)
data(iris)

# Create binary classification problem (setosa vs versicolor)
iris_binary <- iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = droplevels(Species))

# Create train/test split
iris_split <- initial_split(iris_binary, prop = 0.7, strata = Species)
train_data <- training(iris_split)
test_data <- testing(iris_split)

# Fit quantum matrix classifier
qm_model <- dm_fit(Species ~ ., data = train_data, n_breaks = 3)
print(qm_model)

# Make predictions
predictions <- predict(qm_model, newdata = test_data)
probabilities <- predict(qm_model, newdata = test_data, type = "prob")

# Evaluate performance
results <- test_data %>%
  select(Species) %>%
  bind_cols(predictions, probabilities)

accuracy(results, truth = Species, estimate = .pred_class)
```

### Comparison with Traditional Methods

```r
# Compare DM with PCA + Logistic Regression
library(tidymodels)

# Traditional PCA approach (using same binary data)
pca_recipe <- recipe(Species ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 2)

pca_model <- workflow() %>%
  add_recipe(pca_recipe) %>%
  add_model(logistic_reg()) %>%
  fit(train_data)

# Compare predictions
pca_pred <- predict(pca_model, test_data)
qm_pred <- predict(qm_model, test_data)

# Performance comparison
bind_rows(
  accuracy(test_data %>% bind_cols(pca_pred), Species, .pred_class) %>% mutate(method = "PCA+LR"),
  accuracy(test_data %>% bind_cols(qm_pred), Species, .pred_class) %>% mutate(method = "Quantum Matrix")
)
```

## 🎛️ Advanced Usage

### Custom Configuration

```r
# Advanced model configuration (using binary classification data)
advanced_model <- dm_fit(
  Species ~ ., 
  data = train_data,  # Same binary data: setosa vs versicolor
  n_breaks = 5,       # More discretization bins
  verbose = 2,        # Detailed output for debugging
  bandwidth = 0.15    # Kernel bandwidth for density estimation
)

# Model diagnostics
summary(advanced_model)
```

### Integration with Tidymodels Workflows

```r
library(workflows)

# Cross-validation evaluation (using binary classification)
library(rsample)
folds <- vfold_cv(train_data, v = 5, strata = Species)

# Performance across folds (manual implementation)
cv_results <- map_dfr(folds$splits, function(split) {
  analysis_data <- analysis(split)
  assessment_data <- assessment(split)
  
  fold_model <- dm_fit(Species ~ ., data = analysis_data, n_breaks = 3)
  fold_pred <- predict(fold_model, assessment_data)
  
  assessment_data %>%
    select(Species) %>%
    bind_cols(fold_pred) %>%
    metrics(truth = Species, estimate = .pred_class)
})

# Summarize cross-validation results
cv_results %>%
  group_by(.metric) %>%
  summarise(mean = mean(.estimate), sd = sd(.estimate))
```

### Alternative Datasets

```r
# Using Palmer Penguins for binary classification
library(palmerpenguins)
data(penguins)

# Create binary problem: Adelie vs Gentoo
penguins_binary <- penguins %>%
  filter(species %in% c("Adelie", "Gentoo")) %>%
  drop_na() %>%
  mutate(species = droplevels(species))

# Same workflow applies
penguin_split <- initial_split(penguins_binary, prop = 0.7, strata = species)
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)

penguin_model <- dm_fit(species ~ ., data = penguin_train, n_breaks = 3)
penguin_pred <- predict(penguin_model, penguin_test)
```

## 📖 API Reference

### Core Functions

| Function | Purpose | Usage |
|----------|---------|-------|
| `dm_fit(formula, data, ...)` | Fit quantum matrix classifier | `dm_fit(Species ~ ., data, n_breaks = 3, bandwidth = 0.1)` |
| `predict.dm_fit(object, newdata, type)` | Make predictions | `predict(model, newdata, type = "class")` |
| `print.dm_fit(x, ...)` | Display model summary | `print(model)` |
| `summary.dm_fit(object, ...)` | Detailed model information | `summary(model)` |

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `formula` | formula | - | Model specification (e.g., `y ~ .`) **Binary classification only** |
| `data` | data.frame | - | Training dataset with binary target variable |
| `n_breaks` | integer | 3 | Discretization bins for continuous variables |
| `verbose` | integer | 0 | Verbosity level (0=silent, 1+=detailed) |
| `bandwidth` | numeric | 0.1 | Kernel bandwidth used for density estimation |
| `type` | character | "class" | Prediction type: "class" or "prob" |

### Return Values

- **`dm_fit()`**: Returns S3 object of class "dm_fit" with training data and preprocessing parameters
- **`predict()`**: Returns factor vector (type="class") or tibble (type="prob") following tidymodels conventions

## 🔬 Algorithm Overview

The **Density Matrix (DM)** method is a quantum-inspired approach to dimensionality reduction and classification that offers an alternative to traditional linear methods like PCA.

### How It Works

1. **🔄 Data Preprocessing**: Discretizes continuous variables and encodes categorical data
2. **⚛️ Quantum Matrix Construction**: Creates quantum density matrices (ρ_d) from preprocessed data
3. **📐 SVD Transformation**: Projects data onto eigenvector basis using singular value decomposition  
4. **🌀 Polar Coordinates**: Converts to polar coordinate system (r, φ) for classification
5. **📊 Kernel Density Estimation**: Uses quantum-inspired kernels for probability estimation
6. **🎯 Classification**: Applies maximum likelihood principle for final predictions

> **⚠️ Current Limitation**: The DM method currently supports **binary classification only**. Multi-class extensions are planned for future releases.

### Mathematical Foundation

The core quantum density matrix is constructed as:

```math
ρ_d = (X X^T) / trace(X X^T)
```

Where X represents the quantum-encoded feature matrix. The method then uses SVD to find the optimal coordinate transformation:

```math
Coordinates = X_normalized × U_matrix
Polar: r = √(V₁² + V₂²), φ = atan2(V₂, V₁) ∈ [0, 2π)
```

### Advantages Over PCA

| Aspect | PCA | Density Matrix |
|--------|-----|----------------|
| **Transformation** | Linear only | Non-linear, quantum-inspired |
| **Assumptions** | Gaussian, linear relationships | Minimal assumptions |
| **Output Space** | Principal components | Polar coordinates |
| **Data Types** | Requires preprocessing | Handles mixed types naturally |
| **Interpretability** | Variance explained | Quantum probability distributions |

## 📁 Project Structure

```text
qml/
├── R/                          # Core package functions
│   ├── dm_classifier.R         # Main interface and S3 methods  
│   ├── quantum_matrix.R        # Quantum density matrix operations
│   ├── coordinate_transforms.R # Coordinate transformations and SVD
│   ├── kernel_functions.R      # Quantum kernel functions
│   ├── data_preprocessing.R    # Data validation and preparation
│   └── dm_utils.R             # Utility functions and constants
├── experiments/               # Research and comparison studies
│   ├── simple_test.R         # Basic functionality tests
│   ├── test_fixed_prediction.R # Debugging and validation
│   └── new/                  # Advanced comparisons and benchmarks
├── presentations/            # Educational materials
│   ├── DM.Rmd               # Main DM presentation
│   └── DMnPCA.Rmd           # DM vs PCA comparison
├── man/                     # Documentation (auto-generated)
├── tests/                   # Unit tests
└── data/                    # Example datasets and environments
```

## 🤝 Contributing

We welcome contributions! The modular architecture makes it easy to contribute:

- **Focused modules**: Each file has clear, specific responsibilities
- **Comprehensive docs**: All functions include roxygen2 documentation  
- **Clean dependencies**: Clear separation of concerns across modules
- **Robust validation**: Extensive input checking and error handling
- **Test coverage**: Unit tests and integration examples

### Development Workflow

1. **Fork** the repository
2. **Create** a feature branch (`git checkout -b feature/amazing-feature`)
3. **Follow** R package development best practices
4. **Test** your changes thoroughly
5. **Document** new functions with roxygen2
6. **Submit** a pull request

### Areas for Contribution

- **🔧 Performance optimization** for large datasets
- **📊 Additional visualization** functions
- **🧪 Extended unit tests** and benchmarks  
- **📚 Documentation** improvements and examples
- **🔬 New quantum-inspired** methods and algorithms
- **🤖 Integration** with other tidymodels components

## 📚 Citation

If you use this package in your research, please cite:

```bibtex
@software{qml_package,
  title = {qml: Quantum Machine Learning in R},
  author = {Rodríguez-Gallego, José-Antonio},
  year = {2025},
  url = {https://github.com/RodGal-2020/qml},
  note = {R package version 0.1.0}
}
```

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Tidymodels Team** for the excellent modeling framework
- **R Core Team** and **RStudio** for the robust development environment
- **Palmer Penguins Dataset** contributors for providing excellent test data

---

**⭐ Star this repo** if you find it useful! **🐛 Report issues** or **💡 suggest features** in the [Issues](https://github.com/RodGal-2020/qml/issues) section.
