n_cuts = 2

Data = list()

Data$original = iris %>%
  as_tibble() %>%
  filter(Species != "setosa") %>%
  mutate(Species = as.factor(Species)) %>%
  # select(Sepal.Length, Petal.Length, Petal.Width, Species) %>%
  rename(
    SL = Sepal.Length,
    SW = Sepal.Width,
    PL = Petal.Length,
    PW = Petal.Width,
    Class = Species
  ) %>%
  mutate(
    Class = forcats::fct_recode(Class,
                                NULL = "setosa",
                                "1" = "versicolor",
                                "2" = "virginica"
    ), .keep = "unused"
  )

Data$cut = Data$original %>%
  mutate(across(
    where(is.numeric) & !all_of("Class"),  # Exclude 'Class'
    ~ factor(cut(.x, n_cuts, labels = FALSE)) # Cut into 3 groups (numeric labels for consistency)
  ))

Data$uniclass = Data$cut %>%
  select(-Class) %>%  # Exclude Class before model.matrix()
  model.matrix(~ . - 1, data = .) %>%  # Design matrix without intercept
  as_tibble() %>%
  bind_cols(Class = Data$cut$Class)

Data$design = Data$cut %>%
  select(-Class) %>%
  model.matrix(~ . - 1, data = .) %>%
  as_tibble() %>%
  bind_cols(Class = Data$cut$Class) %>%
  mutate(
    across(
      where(is.numeric),
      ~ {
        attr(.x, "contrasts") <- NULL;
        attr(.x, "assign") <- NULL;
        # .x = as.factor(.x);
        .x }
    )
  )

Data$design_normalized = Data$design %>%
  mutate(across(
    where(is.numeric),
    ~ (.x - mean(.x)) / sd(.x),
    .names = "{.col}"), .keep = "unused")

