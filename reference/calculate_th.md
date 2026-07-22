# Calculate target height SDS for a single child

`calculate_th()` calculates the target height of a child based on the
height of the biological parents, using the method proposed by
Hermanussen & Cole (2003) with a simplified formula for the Dutch
population. The function is not vectorized, so it only works for a
single child at a time. Optionally, it can calculate the target height
based on the height of the mother only, although this only works for the
Dutch population.

## Usage

``` r
calculate_th(
  hgtf,
  hgtm,
  sex = NULL,
  etn = NULL,
  support_missing_hgtf = FALSE,
  dec = c(1L, 3L, 3L)
)
```

## Arguments

- hgtf:

  Length of biological father (cm)

- hgtm:

  Length of biological mother (cm)

- sex:

  Character, either `"male"` or `"female"`. If omitted, the functions
  returns c(`NA`, `NA`).

- etn:

  Ethnicity, one of `"NL"` (dutch), `"TU"` (turkish), `"MA"` (moroccan),
  `"HS"` (hindustani) or `"DS"` (Down syndrome). If omitted, the
  functions returns c(`NA`, `NA`).

- support_missing_hgtf:

  Logical, if `TRUE` and `hgtf` is missing, the function will calculate
  the target height based on `hgtm` only. This will work only for the
  Dutch population. If `hgtm` is missing, the function will always
  return `NA`. The default is `support_missing_hgt = FALSE`.

- dec:

  Integer vector, length 2, indicating rounding for th and th_z,
  respectively

## Value

     Numeric, length 3: 1) target height (cm), 2) target height

prediction error and 3) target height SDS.

## Details

For children with Down syndrome (`etn = "DS"`), the target height
formula is taken from Van Gameren-Oosterom et al. (2012), fitted
directly on final height of Dutch children with Down syndrome rather
than derived from parent-offspring/assortative-mating correlations. That
paper does not report a prediction error distinct from the final-height
SD used in the SDS formula, so `th_pe` and the denominator of `th_z`
both use that same SD (6.2 boys / 7.3 girls).

## References

Van Gameren-Oosterom HBM, Van Dommelen P, Oudesluys-Murphy AM,
Buitendijk SE, Van Buuren S, Van Wouwe JP (2012) Healthy Growth in
Children with Down Syndrome. PLoS ONE 7(2): e31079.

## Author

     Stef van Buuren, 2019, 2024

## Examples

``` r
calculate_th(180, 170, "male", "NL")
#> [1] 182.100   5.784  -0.246
calculate_th(NA, 170, "male", "NL", support_missing_hgtf = TRUE)
#> [1] 183.500   5.784  -0.037
calculate_th(180, 170, "male", "DS")
#> [1] 161.900   6.200  -0.247
```
