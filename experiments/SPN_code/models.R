# renv::install(c(
#   "ranger",
#   "kernlab",
#   "xgboost"
# ))

library(rules)
library(baguette)
library(parsnip)
library(rpart)

# Random forest
rf_spec <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Árbol de decisión
tree_spec <- decision_tree(min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# KNN
knn_spec <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

# Regresión logística
logistic_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>% # Escogido para el tuning
  set_mode("classification")

# MLP
nn_spec <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet", MaxNWts = 2600) %>%
  set_mode("classification")

### DIABLO

# mars_spec <-
#    mars(prod_degree = tune()) %>%  #<- use GCV to choose terms
#    set_engine("earth") %>%
#    set_mode("regression")

svm_r_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_p_spec <-
  svm_poly(cost = tune(), degree = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

cart_spec <-
  decision_tree(cost_complexity = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

bag_cart_spec <-
  bag_tree() %>%
  set_engine("rpart", times = 50L) %>%
  set_mode("classification")

xgb_spec <-
  boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
             min_n = tune(), sample_size = tune(), trees = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# cubist_spec <-
#    cubist_rules(committees = tune(), neighbors = tune()) %>%
#    set_engine("Cubist") %>%
#    set_mode("classification")


model_list = list(
  tree = tree_spec,
  rf = rf_spec,
  # pcc = pcc_spec, # Esto realmente es KNN
  logistic = logistic_spec,
  knn = knn_spec,
  nn = nn_spec,

  # DIABLO
  # mars = mars_spec,
  svm_r = svm_r_spec,
  svm_p = svm_p_spec,
  cart = cart_spec,
  bag_cart = bag_cart_spec,
  xgb = xgb_spec
  # cubist = cubist_spec
)

model_names = names(model_list)
