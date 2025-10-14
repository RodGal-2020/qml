# Experiments Folder

This folder contains experimental code, package testing, and development files that are not part of the main package distribution.

## Testing Files

### `test_package.Rmd`
Comprehensive testing and validation document for the qml package. This RMarkdown file:
- Tests package installation and loading
- Validates all main functions
- Performs advanced testing with train/test splits
- Benchmarks performance
- Provides a complete validation report

**Usage**: Open in RStudio and knit to HTML for a complete test report.

### `test_qml_example.R`
Console-based testing script that runs the examples from the README. Provides:
- Step-by-step testing of core functionality
- Manual validation of results  
- Error handling verification

**Usage**: Run in R console with `source("experiments/test_qml_example.R")`

### `simple_test.R`
Minimal testing script for quick validation:
- Basic library loading
- Simple model fitting
- Quick prediction test

**Usage**: Run line-by-line in R for quick validation.

## Other Experimental Files

### `2025_02_SPN.Rmd`
Original experimental work and algorithm development.

### `new/`
Folder containing additional experimental analyses and comparisons.

### `SPN_code/`
Supporting code for experimental work.

## Running the Tests

To comprehensively test the package:

1. **Full validation**: Open and knit `test_package.Rmd`
2. **Quick console test**: Run `source("experiments/test_qml_example.R")`
3. **Minimal test**: Run `source("experiments/simple_test.R")`

These files help ensure the qml package works correctly across different environments and use cases.
