# 🔧 workflowset-----------------------------------------------------------------
all_workflows <- workflow_set(
  preproc = list("rec1" = rec1),
  models = model_list) %>%
  option_add(grid = params$grid_size, strict = FALSE)

### Common options
# all_workflows %<>%
  # option_add(control = control_grid(
  #   # extract = function(x) x,
  #   save_pred = TRUE,
  #   parallel_over = "everything",
  #   save_workflow = TRUE),
  #   strict = FALSE
  # ) %>%
  # %>%
# option_add_parameters()

### Grids
## SVM grid
# svm_grid <- grid_regular(
#   cost(range = c(-8, 2)),
#   rbf_sigma(range = c(-5, 1)),
#   levels = params$grid_size
# )

## XGB grid
# xgb_grid <- grid_regular(
#   tree_depth(range = c(4, 12)),
#   learn_rate(range = c(0.01, 0.3)),
#   min_n(range = c(2, 20)),
#   # sample_size(range = c(0.5, 1)),
#   sample_size(range = c(0, 1)),
#   trees(range = c(100, 500)),
#   loss_reduction(range = c(1e-4, 10)),
#   levels = params$grid_size
# )

# all_workflows %<>%
  # option_add(grid = xgb_grid, id = "rec1_xgb", strict = FALSE) %>%
  # option_add(grid = svm_grid, id = "rec1_svm_r", strict = FALSE)
