test_that("test prediction bridge function - formula method", {
  mod_1 <- quantile_model(mpg ~ ., data = mtcars, quantiles = .1)

  expect_snapshot(
    predict(mod_1, new_data = mtcars, type = "numeric"),
    error = TRUE
  )

  pred_1 <- predict(mod_1, new_data = mtcars[1:3,])
  expect_equal(length(pred_1), 3)

  num_rows <- unique(unlist(lapply(pred_1, nrow)))
  expect_equal(num_rows, 2)

  act_col_names <- lapply(pred_1, names)
  expt_col_names <- lapply(pred_1, function(x) c(".quantile_level", ".pred_quantile"))
  expect_equal(act_col_names, expt_col_names)

})
