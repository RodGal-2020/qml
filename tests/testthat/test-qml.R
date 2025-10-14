# Basic tests for quantum matrix classifier

test_that("dm_fit works with iris data", {
  skip_if_not_installed("fasano.franceschini.test")
  
  model <- dm_fit(Species ~ ., data = iris, n_breaks = 3)
  
  expect_s3_class(model, "dm_fit")
  expect_equal(model$n_classes, 3)
  expect_equal(model$classes, levels(iris$Species))
  expect_equal(model$objective_var, "Species")
})

test_that("predict.dm_fit works", {
  skip_if_not_installed("fasano.franceschini.test")
  
  model <- dm_fit(Species ~ ., data = iris, n_breaks = 3)
  
  # Test class predictions
  pred_class <- predict(model, newdata = iris[1:5, ])
  expect_length(pred_class, 5)
  expect_s3_class(pred_class, "factor")
  expect_true(all(pred_class %in% levels(iris$Species)))
  
  # Test probability predictions
  pred_prob <- predict(model, newdata = iris[1:5, ], type = "prob")
  expect_equal(dim(pred_prob), c(5, 3))
  expect_equal(colnames(pred_prob), levels(iris$Species))
  expect_true(all(pred_prob >= 0 & pred_prob <= 1))
})

test_that("testify creates proper train/test split", {
  result <- testify(iris, "Species", test_prob = 0.3)
  
  expect_true("test" %in% names(result))
  expect_type(result$test, "logical")
  
  # Check proportions are roughly correct
  test_prop <- mean(result$test)
  expect_true(test_prop > 0.2 && test_prop < 0.4)
  
  # Check stratification worked
  test_counts <- table(result$Species, result$test)
  expect_true(all(test_counts > 0))  # All combinations should have some observations
})

test_that("K function works correctly", {
  # Test kernel function
  expect_equal(K(0, 0), 2)  # At origin with r=2
  expect_equal(K(1, 0), 0)  # Outside unit circle
  expect_equal(K(0.5, 0.5, r = 2), 1)  # Inside unit circle
  expect_gte(K(0.1, 0.1), 0)  # Non-negative
})

test_that("package can handle errors gracefully", {
  expect_error(dm_fit(), "formula is required")
  expect_error(dm_fit(Species ~ ., data = iris[1:5, ]), "Each class must have at least 2 observations")
})
