# ⚛ Quantum Machine Learning

## Project setup

1. Open the `.Rproj` file.
2. Launch `renv::restore()` and install all packages.
3. Go to the `experiments/` folder and run any experiment you want :).
  1. You may need to install additional packages:
  
```{r}
renv::install(c(
  "ranger",
  "kernlab",
  "xgboost"
))
```

## Project organization

- `data/`: Contains the datasets used in the experiments, as well as some environments.
- `experiments/`: Contains the experiments that were run. 
- `figs/`: Contains the figures generated in the experiments.
- `R/`: Contains the functions used in the experiments.
