# Pick reference for evaluating weight guidelines

The function picks the reference for evaluation according to Dutch
referral guidelines for underweight and overweight.

## Usage

``` r
pick_reference_wgt(age = NA, sex = NA_character_, ga = NA, etn = NA_character_)
```

## Arguments

- age:

  Scalar, most recent decimal age (in years).

- sex:

  Character, either `"male"` or `"female"`

- ga:

  Gestational age, completed weeks (Integer or character)

- etn:

  Ethnicity, one of `"NL"` (dutch), `"TU"` (turkish), `"MA"` (moroccan)
  or `"HS"` (hindustani).

## Value

A list or `NULL`. Element `call` contains an executable string to the
proper reference. Element `ty` is a function to be applied to the
measurement before calculating the Z-score. Element `yname` is the type
of measurement. The function returns `NULL` if it cannot determine a
proper reference (e.g. for missing age or sex).

## Details

The Z-score calculation relies on normative references. The exact
reference used depends on the age of the child, sex, gestational age and
ethnicity.

The underweight guideline prescribes `wgt` references for ages 0-1
years, `wfh` references for ages 1-2 years. The overweight guidelines
prescribes `wfh` references for ages 1-2 years. For ages \> 2.0,
international and ethnic specific (for HS) BMI-cut offs are use. For
preterms (ga \< 37) Dutch preterm `wgt` and `wfh` references are used.
The function does not return BMI references.

Missing data policy: if `age` or `sex` are missing, the function returns
`NULL`. If `ga` is missing, the function assumes term birth. If `etn` is
missing the function assumes Dutch ethnicity.

## References

Dutch guideline overweight 2012:
<https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/overgewicht>

Dutch guidline underweight 2019:
<https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/ondergewicht-2019>

## Author

Stef van Buuren, 2020

## Examples

``` r
growthscreener:::pick_reference_wgt(age = 0.5, sex = "male")
#> [1] "nl_2009_wgt_male_nl"
```
