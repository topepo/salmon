#' Fit a `quantile_model`
#'
#' `quantile_model()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param quantiles A numeric vector of _at least_ one quantile (>0 and <1).
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `quantile_model` object.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#' # TODO Add some examples
#'
#' @export
quantile_model <- function(x, ...) {
  UseMethod("quantile_model")
}

#' @export
#' @rdname quantile_model
quantile_model.default <- function(x, ...) {
  stop("`quantile_model()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname quantile_model
quantile_model.data.frame <- function(x, y, quantiles, ...) {
  processed <- hardhat::mold(x, y)

  quantile_model_bridge(processed, quantiles, ...)
}

# XY method - matrix

#' @export
#' @rdname quantile_model
quantile_model.matrix <- function(x, y, quantiles, ...) {
  processed <- hardhat::mold(x, y)
  quantile_model_bridge(processed, quantiles, ...)
}

# Formula method

#' @export
#' @rdname quantile_model
quantile_model.formula <- function(formula, data, quantiles, ...) {
  processed <- hardhat::mold(formula, data)
  quantile_model_bridge(processed, quantiles, ...)
}

# Recipe method

#' @export
#' @rdname quantile_model
quantile_model.recipe <- function(x, data, quantiles, ...) {
  processed <- hardhat::mold(x, data)
  quantile_model_bridge(processed, quantiles, ...)
}

# ------------------------------------------------------------------------------
# Bridge

quantile_model_bridge <- function(processed, quantiles, ...) {
  quantiles <- check_quantiles(quantiles)
  # TODO Note that `check_quantiles()` will add 1/2 if it is not in the list;
  #      There is an option to change this in the check functions.

  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  # TODO: fit all of your quantile models for each quantile and populate
  # a list of models.
  model_list <- vector(mode = "list", length = length(quantiles))

  new_quantile_model(
    quantiles = quantiles,
    args = list(),
    model_list = model_list,
    blueprint = processed$blueprint
  )
}

# ------------------------------------------------------------------------------

# TODO Add your model fit code here
