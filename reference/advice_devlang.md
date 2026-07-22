# Referral advice for language development

This function traverses the decision tree "JGZ-Richtlijn
taalontwikkeling 2012", which is based on the van Wiechen questionnaire
for child development.

## Usage

``` r
calculate_advice_devlang(
  dob = NA_character_,
  dom_vw41 = NA,
  vw41 = NA,
  dom_vw42 = NA,
  vw42 = NA,
  dom_vw43 = NA,
  vw43 = NA,
  dom_vw44 = NA,
  vw44 = NA,
  dom_vw45 = NA,
  vw45 = NA,
  dom_vw46 = NA,
  vw46 = NA,
  force = FALSE
)
```

## Arguments

- dob:

  Date of birth (`ddmmYYYY`). Required if `dom` is supplied as a date
  string.

- dom_vw41:

  Date of measurement for van Wiechen item 41. Either a vector of age in
  decimal years or a date in the format `yyyymmdd`

- vw41:

  Outcome of the van Wiechen item 41.

- dom_vw42:

  Date of measurement for van Wiechen item 42. Either a vector of age in
  decimal years or a date in the format `yyyymmdd`

- vw42:

  Outcome of the van Wiechen item 42.

- dom_vw43:

  Date of measurement for van Wiechen item 43. Either a vector of age in
  decimal years or a date in the format `yyyymmdd`

- vw43:

  Outcome of the van Wiechen item 43.

- dom_vw44:

  Date of measurement for van Wiechen item 44. Either a vector of age in
  decimal years or a date in the format `yyyymmdd`

- vw44:

  Outcome of the van Wiechen item 44.

- dom_vw45:

  Date of measurement for van Wiechen item 45. Either a vector of age in
  decimal years or a date in the format `yyyymmdd`

- vw45:

  Outcome of the van Wiechen item 45.

- dom_vw46:

  Date of measurement for van Wiechen item 46. Either a vector of age in
  decimal years or a date in the format `yyyymmdd`

- vw46:

  Outcome of the van Wiechen item 46.

- force:

  Should the repeat at 2.5 years be done regarldess of outcome at 2?
  Default value is `FALSE`.

## Value

`calculate_advice_devlang` returns an integer, the `msgcode`, between
4000-4999.

## Details

The observation corresponding to the oldest age is taken is the current
measurement.

## Author

Arjan Huizing, Stef van Buuren, 2020

## Examples

``` r
msg(calculate_advice_devlang())
#> [1] "Voer de meetdatum of leeftijd waarop de van Wiechen gemeten zijn in."
msgcode <- calculate_advice_devlang(dob = "20200101",
                                dom_vw41 = "20220101", vw41 = 2,
                                dom_vw42 = "20220101", vw42 = 1)
msg(msgcode)
#> [1] "Het advies volgens de JGZ-richtlijn taalontwikkeling is als volgt: Uitslag twijfel, aanbod begeleiding door preventief werkend logopedist of jeugdverpleegkundige en herbeoordeling op 2 jaar en 6 maanden."
```
