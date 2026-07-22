# Target Height Calculation for Dutch Population

This function calculates the target height of a child based on the
height of the biological parents, using the method proposed by
Hermanussen & Cole (2003) with a simplified formula for the Dutch
population.

## Usage

``` r
targetheight(hgtf, hgtm, sex, etn = NULL)
```

## Arguments

- hgtf:

  Numeric vector with the height of the biological father in cm. Can
  include `NA` for unknown heights.

- hgtm:

  Numeric vector with the height of the biological mother in cm. Must
  have the same length as `hgtf`.

- sex:

  Character vector indicating the sex of the child for which target
  height is calculated. Must be either `"male"` or `"female"`.

- etn:

  Ethnicity. Not used in this function, but included for consistency
  with other functions.

## Value

A `data.frame` with three columns:

- `y`:

  The calculated target height in cm.

- `y.sd`:

  The standard deviation of the error term in cm.

- `z`:

  The target height in SDS, relative to the child reference.

## Details

For more details, see Van Dommelen et al. (2012). The formulas are as
follows:

- For boys::

  TH = `44.5 + 0.376 * hgtf + 0.411 * hgtm`

- For girls::

  TH = `47.1 + 0.334 * hgtf + 0.364 * hgtm`

If the height of the biological father is unknown, we use:

- For boys::

  TH = `99.9 + 0.492 * hgtm`

- For girls::

  TH = `96.3 + 0.436 * hgtm`

If the mother's height is unknown, the function returns `NA` as the
target height cannot be calculated.

## References

Hermanussen M, Cole TJ (2003). "The Calculation of Target Height
Reconsidered." *Hormone Research*, **59**, 180–183.

Van Dommelen P, Schonbeck Y, Van Buuren S (2012). "A simple calculation
of the target height." *Archives of Disease in Childhood*, **97**, 182.

## See also

[`hermanussencole`](https://growthcharts.org/growthscreener/reference/hermanussencole.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Target height for Dutch children with father height 180 cm and
# mother height 170 cm
# For both a boy and a girl
targetheight(hgtf = c(180, 180), hgtm = c(170, 170), sex = c("male", "female"))

# Handling missing father height
targetheight(hgtf = c(NA, 180), hgtm = c(171, NA), sex = c("male", "male"))
} # }
```
