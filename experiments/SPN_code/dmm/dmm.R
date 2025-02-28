# Función para aplicar el modelo dmm a cada muestra de bootstrap
aplicar_dmm <- function(resample) {
  # resample$splits es el objeto que contiene las particiones de datos
  # resample = train_resamples$splits[[1]]

  data_resample_train <- rsample::analysis(resample) %>%   # extrae los datos de entrenamiento de la muestra bootstrap
    mutate(test = FALSE)
  data_resample_test <- rsample::assessment(resample) %>%
    mutate(test = TRUE)

  data_resample <- bind_rows(data_resample_train, data_resample_test)  # une los datos de entrenamiento y test

  modelo <- dmm(data_resample, n_breaks = 3, objective_var = "y", verbose = 0, test_var = "test")  # aplica tu modelo
  return(modelo)  # devuelve el modelo o resultados (predicciones, métricas, etc.)
}

# train_resamples$splits[[1]] %>% aplicar_dmm()
dmms <- map(train_resamples$splits, aplicar_dmm)

# Fix codification of the levels of the factor variables
# Example of the error:
#
# rsample::assessment(train_resamples$splits[[1]])$y %>% table()
# dmms[[1]]$.truth %>% table()
# dmms[[1]]$.truth %>% head()
#
# When launching dmm 1s turned to 1s and 0s to 2s. We fix that using the forcats package:
dmms %<>% map(
  ~ .x %>%
    mutate(across(c(.pred, .truth), ~ forcats::fct_recode(.x, "0" = "2")))
)

dmm_metrics <- dmms %>% map(
  ~ .x %>% my_metrics(
    # Que tengan los mismos niveles como factores, asumiendo que en .truth están
    mutate(.pred = factor(.pred, levels = levels(.truth))),
    truth = .truth, estimate = .pred
  )
)

# Ahora combinamos los resultados anteriores, promediando para cada métrica
dmm_metrics <- dmm_metrics %>%
  bind_rows() %>%
  group_by(.metric) %>%
  summarise(.estimate = mean(.estimate))
