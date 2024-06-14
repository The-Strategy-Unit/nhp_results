
# nhp.results

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/The-Strategy-Unit/nhp.results/branch/main/graph/badge.svg)](https://app.codecov.io/gh/The-Strategy-Unit/nhp.results?branch=main)
<!-- badges: end -->

The goal of nhp.results is to help analysts, parse, wrangle and plot the results of the New Hospital Programme model.

## Installation

You can install the development version of {nhp.results} from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("The-Strategy-Unit/nhp.results")
```

## Example

This is a basic example which shows you how to interact with a `results.json`.

``` r
library(nhp.results)
filename <- "path/to/sample_results.json"
results <- get_results_from_local(filename)
get_trust_sites(results)
```

