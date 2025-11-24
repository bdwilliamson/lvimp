# Create a Longitudinal Variable Importance Object

Create a longitudinal variable importance object from several
constituent cross-sectional variable importance objects.

## Usage

``` r
lvim(vim_list = list(), timepoints = numeric())
```

## Arguments

- vim_list:

  a list of individual, cross-sectional variable importance objects.
  Assumed to be in order over time.

- timepoints:

  a numeric vector of timepoints of interest

## Value

an object of class `lvim`
