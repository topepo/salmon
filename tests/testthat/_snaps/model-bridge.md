# test model bridge function - formula method

    Code
      quantile_model(mpg ~ ., data = mtcars, quantiles = 0)
    Condition
      Error in `check_quantiles()`:
      ! `quantiles` must be > 0 and < 1.

---

    Code
      quantile_model(mpg ~ ., data = mtcars, quantiles = numeric(0))
    Condition
      Error in `check_quantiles()`:
      ! `quantiles` must be a non-empty numeric vector.

---

    Code
      quantile_model(mpg ~ ., data = mtcars, quantiles = letters)
    Condition
      Error in `check_quantiles()`:
      ! `quantiles` must be a numeric vector.

