aw_ranks <- all_workflows %>%
  workflowsets::rank_results(rank_metric = params$rank_metric, select_best = TRUE) %>%
  filter(.metric == params$rank_metric)

aw_autoplot <- all_workflows %>%
  autoplot(metric = params$rank_metric, select_best = TRUE) +
  geom_text(aes(y = mean - 0.04, label = wflow_id), angle = 90, hjust = 1) +
  theme(legend.position = "none")

# if (!params$test) {
#   aw_autoplot = aw_autoplot +
#     lims(y = c(0.8, 0.95))
# }
