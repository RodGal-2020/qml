#🤖 Models----------------------------------------------------------------------
source(here("experiments/SPN_code/models.R"))

#📊 Metrics---------------------------------------------------------------------
source(here("experiments/SPN_code/metrics.R"))

#🧪 Train/Test------------------------------------------------------------------
Data_train <- Data %>% filter(test == 0) %>% select(-test)
Data_test <- Data %>% filter(test == 1) %>% select(-test)
# El método de remuestreo bootstrap
train_resamples <- bootstraps(Data_train, times = params$bootstrap_size)

#🍳 Recipes---------------------------------------------------------------------
# Tratándose de variables categóricas definimos una receta realmente sencilla, que posteriormente aplicaremos para generar un conjunto de datos apropiado para la matriz de Falcó.
rec1 <- recipe(y ~ ., data = Data_train) %>%
  step_novel(all_predictors()) %>% # Valores nuevos, aunque no se den
  step_other(all_predictors(), threshold = 0.01, other = "other") %>% # Categorías infrecuentes
  step_novel(all_predictors()) %>%
  step_dummy(all_nominal(), -y) %>%
  themis::step_smote(y, over_ratio = 0.5) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) # Near zero

## Simplificada para Falcó
rec1f <- recipe(y ~ ., data = Data_train) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors())


