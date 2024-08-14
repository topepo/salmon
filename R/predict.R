#' Predict from a `quantile_model`
#'
#' @param object A `quantile_model` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#' # TODO Add some examples
#'
#' @export
predict.quantile_model <- function(object, new_data, ...) {
  rlang::check_dots_empty()
  forged <- hardhat::forge(new_data, object$blueprint)
  predict_quantile_model_bridge(object, forged$predictors)
}

# ------------------------------------------------------------------------------
# Bridge

predict_quantile_model_bridge <- function(model, predictors) {
  # TODO Add you prediction code in `predict_function()`:
  # predictions <- predict_function(model, predictors)

  # TODO: 'predictions' should be a list column of tibbles. The tibble should
  # have as many rows as values of `quantiles` and have columns
  # c(".quantile_level", ".pred_quantile")

  # TODO: Some dummy code that you can replace
  tmplt <- tibble::tibble(.quantile_level = model$quantiles, .pred_quantile = NA_real_)
  predictions <- lapply(1:nrow(predictors), function(x) tmplt)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

# ------------------------------------------------------------------------------

# TODO Add your model prediction code here



