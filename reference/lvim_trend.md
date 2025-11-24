# Linear Trend in the Longitudinal Variable Importance Trajectory

Compute a nonparametric estimate of (and efficient influence function
for) the linear trend in the longitudinal variable importance over a
contiguous subset of the time series.

## Usage

``` r
lvim_trend(lvim, indices = 1:length(lvim), delta = 0)
```

## Arguments

- lvim:

  an object of class `lvim` containing the cross-sectional variable
  importance objects

- indices:

  a numeric vector indicating the contiguous subset of the time series

- delta:

  null hypothesis value

## Value

The `lvim` object, with point estimates, CIs, and p-values related to
the linear trend in variable importance filled in.
