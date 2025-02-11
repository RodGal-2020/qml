cat("The previous code returns the all_workflows object after being tuned")

# renv::install("ranger")
# renv::install("xgboost")

all_workflows = all_workflows %>% workflow_map(
  resamples = train_resamples,
  grid = params$grid_size,
  metrics = my_metrics,
  control = control_grid(save_pred = TRUE, parallel_over = "everything", save_workflow = TRUE),
  verbose = TRUE
) %>%
  mutate(wflow_id = sub("rec1_", "", wflow_id)) # Eliminamos "formula_" en cada identificador

all_workflows
# > Errores: Lo anterior puede fallar si alguno de los modelos considerados no admite tuning. Nótese que en dicho caso no es especialmente comunicativo.

# > Debugging de lo siguiente: `show_notes(.Last.tune.result)`. Alternativa: Mirar `models.R`
aw_ranks <- all_workflows %>% rank_results(rank_metric = params$rank_metric)

# Handy plot
aw_autoplot <- all_workflows %>% autoplot(metric = params$rank_metric)  +
  geom_text(aes(y = mean - 0.04, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(0.8, 0.95)) +
  theme(legend.position = "none")
