# This could work if the event_level was the first, which is not in our case
my_metrics = metric_set(sensitivity, specificity, ppv, npv, accuracy, f_meas, kap, mcc)
# > **🧠 ROC AUC** is not a suitable metric for our problem

### Alternative with event_level = "second"
## Note: Not recommended, it's way easier to change the order of the levels with forcats::fct_relevel()
# We use the event_level functions to make sure that the event_level is the correct one
# Another option would be to change the 1s to 0s and vice versa in the truth column
# sensitivity_event_level <- metric_tweak("sensitivity_event_level", sensitivity, event_level = "second")
# specificity_event_level <- metric_tweak("specificity_event_level", specificity, event_level = "second")
# ppv_event_level <- metric_tweak("ppv_event_level", ppv, event_level = "second")
# npv_event_level <- metric_tweak("npv_event_level", npv, event_level = "second")
# accuracy_event_level <- metric_tweak("accuracy_event_level", accuracy, event_level = "second")
# f_meas_event_level <- metric_tweak("f_meas_event_level", f_meas, event_level = "second")
# kap_event_level <- metric_tweak("kap_event_level", kap, event_level = "second")
# mcc_event_level <- metric_tweak("mcc_event_level", mcc, event_level = "second")
#
#
# my_metrics = metric_set(
#   sensitivity_event_level,
#   specificity_event_level,
#   ppv_event_level,
#   npv_event_level,
#   accuracy_event_level,
#   f_meas_event_level,
#   kap_event_level,
#   mcc_event_level
# )

# An example of use
# iris %>%
#   mutate(estimate = factor("setosa", levels = c("setosa", "versicolor", "virginica"))) %>%
#   my_metrics(truth = Species, estimate = estimate)
