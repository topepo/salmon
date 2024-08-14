test_that("test model bridge function - formula method", {
  mod_1 <- quantile_model(mpg ~ ., data = mtcars, quantiles = .1)

  expect_equal(mod_1$quantiles, c(0.1, 0.5))
  expect_equal(names(mod_1), c("quantiles", "args", "model_list", "blueprint"))
  expect_s3_class(mod_1, c("quantile_model", "hardhat_model", "hardhat_scalar"))

  expect_snapshot(
    quantile_model(mpg ~ ., data = mtcars, quantiles = 0),
    error = TRUE
  )
  expect_snapshot(
    quantile_model(mpg ~ ., data = mtcars, quantiles = numeric(0)),
    error = TRUE
  )
  expect_snapshot(
    quantile_model(mpg ~ ., data = mtcars, quantiles = letters),
    error = TRUE
  )

  # TODO Add tests for your args related to `check_args()`

  # TODO Add tests for your model related to `check_model_list()`

})

test_that("test model bridge function - matrix method", {
  x <- as.matrix(mtcars[,-1])
  mod_1 <- quantile_model(x, mtcars$mpg, quantiles = .5)

  expect_equal(mod_1$quantiles, c(0.5))
  expect_equal(names(mod_1), c("quantiles", "args", "model_list", "blueprint"))
  expect_s3_class(mod_1, c("quantile_model", "hardhat_model", "hardhat_scalar"))

})

test_that("test model bridge function - data.frame method", {
  mod_1 <- quantile_model(mtcars[,-1], mtcars$mpg, quantiles = (1:9) / 10)

  expect_equal(mod_1$quantiles, (1:9) / 10)
  expect_equal(names(mod_1), c("quantiles", "args", "model_list", "blueprint"))
  expect_s3_class(mod_1, c("quantile_model", "hardhat_model", "hardhat_scalar"))

})

test_that("test model bridge function - recipe method", {
  skip_if_not_installed("recipes")

  library(recipes)
  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_normalize(all_predictors())

  mod_1 <- quantile_model(rec, data = mtcars, quantiles = 0.99999)

  expect_equal(mod_1$quantiles, c(0.5, 0.99999))
  expect_equal(names(mod_1), c("quantiles", "args", "model_list", "blueprint"))
  expect_s3_class(mod_1, c("quantile_model", "hardhat_model", "hardhat_scalar"))

})
