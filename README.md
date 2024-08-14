
<!-- README.md is generated from README.Rmd. Please edit that file -->

# salmon

<!-- badges: start -->

[![R-CMD-check](https://github.com/topepo/salmon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/topepo/salmon/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/topepo/salmon/branch/main/graph/badge.svg)](https://app.codecov.io/gh/topepo/salmon?branch=main)
<!-- badges: end -->

salmon is a template repository for people who want to make quantile
regression models. It has the assumption that, under the hood, users
will have to create separate model objects that optimize quantile loss
for several quantile values (pre-specified by the users).

To get started, clone the repo and then:

- Search and replace `"quantile_model"` with the name of your model
  function.
- Search and replace `"salmod"` with the name of your package.
- Rename the `"salmon-package.R"` file with your package name.
- Update the DESCRIPTION file; choose a license that works for you (MIT
  is a placeholder).
- Populate your functions with whatever arguments that you need.
- Search for `"TODO"`:
  - Add your model training function and write the code in the bridge
    function to execute it
  - Add your model prediction function and write the code in the
    prediction bridge function to execute it.
  - Document and test as usual. Some initial works is done on these.
