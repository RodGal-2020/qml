cat("The previous code returns the tuning_results object after selecting the best")

wflow_ids = all_workflows %>% pull(wflow_id)

tuning_results <- wflow_ids %>%
  map(~ extract_workflow_set_result(all_workflows, id = .x))

# We filter those that actually obtained results, something important when working
# with the test version of the file
drop_me = c()
for (model in 1:length(tuning_results)) {
  NA_metric = tuning_results[[model]]$.metrics[[1]] %>%
    filter(.metric == params$rank_metric) %$%
    .estimate %>%
    is.na() %>%
    all()

  if (NA_metric) {
    drop_me %<>% c(model)
  }
}

if (!is.null(drop_me)) {
  tuning_results[[drop_me]] <- NULL
  wflow_ids = wflow_ids[-drop_me]
  model_names = wflow_ids
}

tuning_results %<>%
  map(~ select_best(.x, metric = params$rank_metric)) %>%
  set_names(wflow_ids) %>%
  imap(
    ~ .x %>% mutate(wflow_id = .y)
  ) %>%
  bind_rows() %>%
  select(-.config) %>%
  rename(model = wflow_id) %>%
  relocate(model) %>%
  mutate(across(everything(), as.character)) %>%
  # Everything which is not "model" now is a parameter
  pivot_longer(cols = -model, names_to = "param", values_to = "value") %>%
  arrange(model) %>%
  filter(!is.na(value)) %>%
  mutate(value = round(as.numeric(value), 3) %>% as.character)
