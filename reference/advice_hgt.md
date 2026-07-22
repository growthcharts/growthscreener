# Referral advice for body height

This function traverses the decision tree of the "JGZ-Richtlijn
Lengtegroei 2019".

## Usage

``` r
calculate_advice_hgt(
  sex = NA_character_,
  bw = NA,
  bl = NA,
  ga = NA,
  etn = "NL",
  hgtf = NA,
  hgtm = NA,
  dob = NA_character_,
  dom = NA_character_,
  y = NA,
  y1 = NA,
  y0 = NA,
  dom1 = NA_integer_,
  dom0 = NA_integer_,
  test_gain = TRUE,
  verbose = FALSE
)
```

## Arguments

- sex:

  Character, either `"male"` or `"female"`

- bw:

  Birth weight (grammes)

- bl:

  Birth length (cm)

- ga:

  Gestational age, completed weeks (Integer or character)

- etn:

  Etnicity, one of `"NL"` (dutch), `"TU"` (turkish), `"MA"` (moroccan)
  or `"HS"` (hindustani). Currently not functional, always uses `"NL"`.
  Only used for target height.

- hgtf:

  Height of father (cm)

- hgtm:

  Height of mother (cm)

- dob:

  Date of birth (`yyyymmdd`). Required if `dom` is supplied as a date
  string.

- dom:

  Vector with dates of measurements. Either supplied as age in decimal
  years or a date in the format `yyyymmdd`.

- y:

  Vector with height measurements (cm)

- y1:

  Legacy parameter.

- y0:

  Legacy parameter.

- dom1:

  Legacy parameter.

- dom0:

  Legacy parameter.

- test_gain:

  Logical. Should the increase or decrease in Z-scores be tested? The
  default is `TRUE`.

- verbose:

  Set to `TRUE` to obtain warnings on reference finding.

## Value

`calculate_advice_hgt` returns an integer, the `msgcode`

## Details

The decision tree assesses both single and paired measurements. The
observation corresponding to the oldest age is taken is the current
measurement.

## Author

Paula van Dommelen, Stef van Buuren, 2021

## Examples

``` r
msg(calculate_advice_hgt())
#> [1] "Voer het geslacht in."
msgcode <- calculate_advice_hgt(sex = "male", dob = "20200101",
                                dom = c("20200201", "20200601"),
                                y = c(54, 68),
                                ga = 35,
                                test_gain = FALSE)
msg(msgcode)
#> [1] "Het advies volgens de JGZ-richtlijn lengtegroei is als volgt: Zijn er duidelijke andere symptomen die samenhangen met aandoeningen die gepaard gaan met een grote lengte? Met name achterstanden in de ontwikkeling, gedragsproblemen en macrocefalie (groot hoofd). Indien ja, Verwijzen naar huisarts/kinderarts. Indien nee, in principe geen verwijzing nodig, naar eigen inzicht handelen."
```
