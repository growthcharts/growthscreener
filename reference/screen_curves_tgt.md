# Screen growth curves according to JGZ guidelines

Screen growth curves according to JGZ guidelines

## Usage

``` r
screen_curves_tgt(
  tgt,
  ynames = c("hgt", "wgt", "hdc"),
  na.omit = TRUE,
  recalculate_z = FALSE,
  ...
)
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

- recalculate_z:

  A logical indicating whether Z-scores should be recalculated.
  Currently not functional. Z-scores are always recalculated.

- ...:

  Not used, but required for extendibility

## Value

A data frame with the following columns

- `Categorie`:

  The category of the screening guidelines: `1000 = hgt`, `2000 = wgt`,
  `3000 = hdc`

- `CategorieOmschrijving`:

  A string indicating the measure

- `Code`:

  Integer message code

- `CodeOmschrijving`:

  A string with the message

- `Versie`:

  Version of `growthscreener` package

- `Leeftijd0`:

  First date of the evaluation pair

- `Leeftijd1`:

  Second date of the evaluation pair

## Examples

``` r
screen_curves_tgt(target)
#>   Categorie CategorieOmschrijving Code
#> 1      1000                Lengte 1031
#> 2      2000               Gewicht 2075
#> 3      3000           Hoofdomtrek 3021
#>                                                                                                                                                                                                                                                                                                                                                         CodeOmschrijving
#> 1                                                                                                                                                                                                                                          Het advies volgens de JGZ-richtlijn lengtegroei is als volgt: In principe geen verwijzing nodig, naar eigen inzicht handelen.
#> 2 Het advies volgens de JGZ-richtlijn ondergewicht is als volgt: Sterke gewichtsafname (-1 SD), advies: Is er sprake van een afwijkende voedingstoestand en/of klachten of symptomen die kunnen wijzen op onderliggende ziekte of problemen? Indien ja, Verwijzen naar kinderarts. Indien nee, dan is er in principe geen verwijzing nodig. Naar eigen inzicht handelen.
#> 3                                                                                                                                                                                                                                                                                                          De richtlijn hoofdomtrek is bedoeld voor kinderen tot 1 jaar.
#>   Versie Leeftijd
#> 1 1.27.0   2.0397
#> 2 1.27.0   2.0397
#> 3 1.27.0   2.0397
```
