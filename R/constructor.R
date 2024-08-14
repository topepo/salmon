# 'quantiles' is an double vector of 'quantiles' to be predicted
# 'args' is a list of model arguments
# 'model_list' is a collection of models that correspond to the ordered set of quantiles


new_quantile_model <- function(quantiles, args, model_list, blueprint) {
  quantiles <- check_quantiles(quantiles)
  args <- check_args(args)
  model_list <- check_model_list(model_list, quantiles)

  hardhat::new_model(
    quantiles = quantiles,
    args = args,
    model_list = model_list,
    blueprint = blueprint,
    class = "quantile_model"
  )
}

check_quantiles <- function(quantiles, include_center = TRUE) {
  quantiles <- unique(quantiles)
  if ( length(quantiles) == 0 ) {
    cli::cli_abort("{.code quantiles} must be a non-empty numeric vector.")
  }
  if ( !is.numeric(quantiles) ) {
    cli::cli_abort("{.code quantiles} must be a numeric vector.")
  }
  if ( any(quantiles <= 0) || any(quantiles >= 1) ) {
    cli::cli_abort("{.code quantiles}  must be > 0 and < 1.")
  }
  if ( include_center && !any(quantiles == 0.5) )  {
    quantiles <- c(quantiles, 1/2)
  }
  sort(unique(quantiles))
}

check_args <- function(x) {
  if ( !is.list(x) ) {
    cli::cli_abort("{.code args} must be a list.")
  }
  nms <- names(x)
  if ( length(nms) != length(x) ) {
    cli::cli_abort("The argument list has {length(x)} elements{?s} but \\
              only {length(nms)} names{?s}.")
  }
  x
}

check_model_list <- function(x, quantiles) {
  if ( !is.list(x) ) {
    cli::cli_abort("{.code model_list} must be a list.")
  }
  if ( length(x) != length(quantiles) ) {
    cli::cli_abort("The length of {.code model_list} must be the same as the \\
                   length of {.code quantiles}.")
  }
  x
}

