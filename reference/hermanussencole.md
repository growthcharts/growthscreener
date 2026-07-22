# Target Height Calculation Using the Hermanussen-Cole Method

This function calculates the target height of a child using the method
proposed by Hermanussen & Cole (2003). The formula accounts for
assortative mating and parent-offspring height correlations, leading to
predictions that are more moderate than the traditional
"follow-the-centile" method.

## Usage

``` r
hermanussencole(
  hgtf,
  hgtm,
  sex,
  rPP = 0.19,
  rPO = 0.58,
  mu = c(183.8, 170.7),
  sigma = c(7.1, 6.3),
  pmu = c(184, 170.6),
  psigma = c(7.1, 6.5)
)
```

## Arguments

- hgtf:

  Numeric vector with the height of the biological father in cm.

- hgtm:

  Numeric vector with the height of the biological mother in cm. The
  length of `hgtm` must be the same as `hgtf`.

- sex:

  Character vector indicating the sex of the child for whom the target
  height is being calculated. Must be either `"male"` or `"female"` and
  have the same length as `hgtf`.

- rPP:

  Numeric value for the correlation between the heights of parents. The
  default value (0.19) is calculated for the Dutch population.
  Hermanussen & Cole (2003) used a value of 0.27.

- rPO:

  Numeric value for the correlation between parental height and
  offspring height. The default value (0.58) is based on the Dutch
  population, while Hermanussen & Cole (2003) used 0.57.

- mu:

  Numeric vector of length 2, representing the mean height in cm of boys
  and girls in the reference population. The default values
  `mu = c(183.8, 170.7)` come from the Fifth Dutch Growth Study (2009).

- sigma:

  Numeric vector of length 2, representing the standard deviation of
  height in cm for boys and girls in the reference population. The
  default values `sigma = c(7.1, 6.3)` are based on the Fifth Dutch
  Growth Study (2009).

- pmu:

  Numeric vector of length 2, representing the mean height in cm of
  fathers and mothers in the reference population. The default values
  `pmu = c(184, 170.6)` are from the Fourth Dutch Growth Study (1997).

- psigma:

  Numeric vector of length 2, representing the standard deviation of
  height in cm for fathers and mothers in the reference population. The
  default values `psigma = c(7.1, 6.5)` are from the Fourth Dutch Growth
  Study (1997).

## Value

A `data.frame` with the three columns:

- `y`:

  Target height in cm.

- `y.sd`:

  Standard deviation of the error term in cm.

- `z`:

  Target height in standard deviation score (SDS), relative to the child
  reference.

## References

Hermanussen M & Cole TJ (2003). "The Calculation of Target Height
Reconsidered." *Hormone Research*, **59**, 180–183.

## See also

[`targetheight`](https://growthcharts.org/growthscreener/reference/targetheight.md)

## Author

Stef van Buuren

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Target height for Dutch population, calculated using
# the Hermanussen & Cole method.
hermanussencole(hgtf = 157, hgtm = 154, sex = "male")

# Example 2: The same target height calculated using the simplified method.
targetheight(hgtf = 157, hgtm = 154, sex = "male")

# Example 3: Target height for case discussed in the appendix of
# Hermanussen & Cole (2003).
# A small discrepancy, 166.5 cm (published) versus 166.6 cm (calculated),
# is due to rounding differences.
hermanussencole(hgtf = c(157, 157), hgtm = c(154, 154),
                sex = c("male", "female"),
                rPP = 0.27, rPO = 0.57,
                mu = c(178.2, 163.8), sigma = c(6.8, 6.0),
                pmu = c(178.2, 163.8), psigma = c(6.8, 6.0))
} # }
```
