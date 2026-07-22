# Extract measurements that can function as date zero

This function scans the `tgt` object, and finds the observation pairs
used by the `calculate_advice_xxx()` functions.

## Usage

``` r
calculate_screening_doms(tgt, ynames = c("hgt", "wgt", "hdc"), na.omit = TRUE)
```

## Arguments

- tgt:

  A list with elements `psn` and `xyz`

- ynames:

  Character vector identifying the measures to be screened. By default,
  `ynames = c("hgt", "wgt", "hdc")`.

- na.omit:

  A logical indicating whether records with a missing `x` (age) or `y`
  (yname) should be removed. Defaults to `TRUE`.

## Value

A list with `length(ynames)` elements. Each list element is another
`list` with elements `dom0` (back-calculated dates of measurement,
vector, reverse time), `age0` (decimal age), `y0` (measurement), `z0`
(Z-score equivalent) and

- for weight - `h0`, as well as similar quantities `dom1`, `age1`, `y1`,
  `z1` and `h1` for the upper part of the pair.

## Details

This function implements the `"x1_last"` strategy for choosing pairs.
This strategy selects the last observation as `x1` and forms pairs with
every earlier observation.

## Note

Internal function. Not to be called directly.

## Examples

``` r
if (FALSE) { # \dontrun{
growthscreener:::calculate_screening_doms(target)
} # }
```
