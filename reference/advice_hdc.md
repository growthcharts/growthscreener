# Referral advice for head circumference

This function traverses a decision tree for head circumference for
children below the age of 1.

## Usage

``` r
calculate_advice_hdc(
  sex = NA_character_,
  ga = NA,
  dob = NA_character_,
  dom = NA_integer_,
  y = NA,
  test_gain = TRUE,
  verbose = FALSE
)
```

## Arguments

- sex:

  Character, either `"male"` or `"female"`

- ga:

  Gestational age, completed weeks (Integer or character)

- dob:

  Date of birth (`yyyymmdd`). Required if `dom` is supplied as a date
  string.

- dom:

  Vector with dates of measurements. Either supplied as age in decimal
  years or a date in the format `yyyymmdd`.

- y:

  Head circumference (cm)

- test_gain:

  Logical. Should the increase or decrease in Z-scores be tested? The
  default is `TRUE`.

- verbose:

  Set to `TRUE` to obtain warnings on reference finding.

## Value

`calculate_advice_hdc` returns an integer, the `msgcode`, between
3000-3999.

## Details

The decision tree assesses both single and paired measurements. The
observation corresponding to the oldest age is taken is the current
measurement.

## Author

Arjan Huizing, Stef van Buuren, 2020

## Examples

``` r
msg(calculate_advice_hdc())
#> [1] "Voer het geslacht in."
msgcode <- calculate_advice_hdc(sex = "female",
                                dom = c(0.2491, 0.6653),
                                y = c(36, 40))
msg(msgcode)
#> [1] "Zeer kleine hoofdomtrek (< -2 SD), advies: Uit onderzoek blijkt dat een zeer kleine hoofdomtrek kan duiden op microcefalie. Dit kan een overweging zijn om het kind te verwijzen."
```
