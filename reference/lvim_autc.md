# Area Under the Variable Importance Trajectory

Compute a nonparametric estimate of (and efficient influence function
for) the area under the longitudinal variable importance trajectory
(AUTC) over a contiguous subset of the time series.

## Usage

``` r
lvim_autc(
  lvim,
  indices = 1:length(lvim),
  interpolator = "linear",
  delta = 0,
  ...
)
```

## Arguments

- lvim:

  an object of class `lvim` containing the cross-sectional variable
  importance objects

- indices:

  a numeric vector indicating the contiguous subset of the time series

- interpolator:

  a string indicating the type of interpolator used to take the area
  under the trajectory

- delta:

  null hypothesis value

- ...:

  other arguments to be passed to the interpolator function

## Value

The `lvim` object, with point estimates, CIs, and p-values related to
the area under the trend in variable importance filled in.
