#🤖 Models----------------------------------------------------------------------
source(here("experiments/SPN_code/models.R"))

#📊 Metrics---------------------------------------------------------------------
# Usaremos las mismas métricas que en el artículo original de Falcó.
my_metrics = metric_set(sensitivity, specificity, ppv, npv, accuracy, f_meas, kap, mcc)
# > **🧠 ROC AUC** no es una métrica que se pueda emplear, dado que no estamos considerando las probabilidades como posibles salidas de los modelos contemplados

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

#🔧 workflowset-----------------------------------------------------------------
all_workflows <- workflow_set(
  preproc = list("rec1" = rec1),
  models = model_list) %>%
  option_add(id = NULL, # Todos
             control = control_grid(extract = function(x) x))
