# Referral advice for weight for height

This function traverses the decision tree of the "JGZ-Richtlijn
overgewicht 2012" and "JGZ-Richtlijn Ondergewicht 2019"

## Usage

``` r
calculate_advice_wgt(
  sex = NA_character_,
  ga = NA,
  etn = NA,
  dom = NA_integer_,
  y = NA,
  dom_hgt = NA_integer_,
  hgt = NA,
  dob = NA_character_,
  test_gain = TRUE,
  y1 = NA,
  y0 = NA,
  hgt1 = NA,
  hgt0 = NA,
  dom1 = NA_integer_,
  dom0 = NA_integer_,
  verbose = FALSE
)
```

## Arguments

- sex:

  Character, either `"male"` or `"female"`

- ga:

  Gestational age, completed weeks (Integer or character)

- etn:

  Etnicity, one of `"NL"` (dutch), `"TU"` (turkish), `"MA"` (moroccan)
  or `"HS"` (hindustani). Currently not functional, always uses `"NL"`.
  Only used for target height.

- dom:

  Vector with dates of measurements. Either supplied as age in decimal
  years or a date in the format `yyyymmdd`.

- y:

  Vector with height measurements (cm)

- dom_hgt:

  Vector with date of measurement relating to height. Either supplied as
  age in decimal years or a date in the format `yyyymmdd`.

- hgt:

  Vector with height measurements (cm)

- dob:

  Date of birth (`yyyymmdd`). Required if `dom` is supplied as a date
  string.

- test_gain:

  Logical. Should the increase or decrease in Z-scores be tested? The
  default is `TRUE`.

- y1:

  Legacy parameter.

- y0:

  Legacy parameter.

- hgt1:

  Legacy parameter.

- hgt0:

  Legacy parameter.

- dom1:

  Legacy parameter.

- dom0:

  Legacy parameter.

- verbose:

  Set to `TRUE` to obtain warnings on reference finding.

## Value

`calculate_advice_wgt()` returns an integer, the `msgcode`

## Details

The decision tree assesses both single and paired measurements. The
observation corresponding to the oldest age is taken is the current
measurement.

## Author

Arjan Huizing, Stef van Buuren 2020

## Examples

``` r
msg(calculate_advice_wgt())
#> [1] "Voer het geslacht in."
msgcode <- calculate_advice_wgt(sex = "male", dob = "20200101",
                                dom = c("20200201", "20200601"),
                                dom_hgt = c("20200201", "20200601"),
                                y = c(5.4, 6.8),
                                hgt = c(54, 60),
                                ga = 35,
                                test_gain = FALSE)
msg(msgcode)
#> [1] "Het advies volgens de JGZ-richtlijn overgewicht is als volgt: In principe geen verwijzing nodig, naar eigen inzicht handelen."
```
