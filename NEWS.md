# qml 0.1.0

## New Features

* **Complete package restructure**: Modular architecture with focused responsibilities
* **Standard R interface**: `dm_fit()` and `predict()` methods following R conventions
* **S3 methods**: Implementation with `print()` and `summary()` methods
* **Quantum matrix classification**: Implementation of the DM model with quantum density matrices
* **Stratified data splitting**: `testify()` function for balanced train/test splits
* **Comprehensive validation**: Input validation and error handling throughout
* **Multiple prediction types**: Support for both class and probability predictions

## Core Functions

* `dm_fit()` - Fit quantum matrix classifier with formula interface
* `predict.dm_fit()` - Make predictions on new data
* `K()` - Quantum kernel function
* `f_hat_h()` - Probability density estimation
* `get_rho_d()` - Quantum density matrix construction
* `get_C_tilde_polar()` - Coordinate transformation to polar system

## Package Infrastructure

* Modular code organization across 6 focused R files
* Comprehensive documentation with roxygen2
* Proper DESCRIPTION file with all dependencies
* MIT license
* Professional package structure following R standards

## Breaking Changes

* Legacy `dm()` function is deprecated (still available with warning)
* New formula-based interface replaces direct data passing
* Separate fitting and prediction phases

## Dependencies

* Core: dplyr, magrittr, purrr, tibble, rlang
* Statistical: fasano.franceschini.test
* Documentation: lifecycle
