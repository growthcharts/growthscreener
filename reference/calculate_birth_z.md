# Calculate birth weight SDS relative to Dutch references

Calculate birth weight SDS relative to Dutch references

## Usage

``` r
calculate_birth_z(y, sex, ga, yname = "wgt", dec = 3)
```

## Arguments

- y:

  Birth weight (grammes) or birth length (cm). May be a vector.
  Converted to numeric.

- sex:

  Character, either `"M"` (male) or `"F"` (female)

- ga:

  Gestational age, completed week (Integer or character)

- yname:

  Either `"wgt"` (for birth weight) or `"hgt"` (for birth length)

- dec:

  Number of decimals for rounding

## Value

     Numeric vector of `length(bw)` elements with
             standard deviation scores relative to Dutch birth
             weight references

## Author

     Stef van Buuren, 2019

## Examples

``` r
calculate_birth_z(c(2500, 3000), sex = "M", ga = 36)
#> [1] NA NA
```
