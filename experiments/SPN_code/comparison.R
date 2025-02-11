metrics_qmat <- qmat_metrics %>%
  select(.metric, .estimate) %>%
  mutate(wflow_id = "qmat", rank = 0)

metrics_others <- all_workflows %>%
  rank_results(rank_metric = params$rank_metric, select_best = TRUE) %>% # Ordenados según accuracy
  select(.metric, mean, wflow_id, rank) %>%
  rename(.estimate = mean)

metrics_full <- bind_rows(metrics_qmat, metrics_others) %>% arrange(rank) %>%
  mutate(.estimate = round(.estimate, 3))

metrics_table <- metrics_full %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  select(-rank) %>%
  relocate(wflow_id, f_meas, accuracy, specificity, sensitivity, kap) %>%
  arrange(-f_meas) %>%
  knitr::kable()
