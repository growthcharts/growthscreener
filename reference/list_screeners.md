# List the available screeners for JGZ guidelines

List the available screeners for JGZ guidelines

## Usage

``` r
list_screeners(ynames = c("hgt", "wgt", "hdc", "lgd"), ...)
```

## Arguments

- ynames:

  Character vector identifying the measures to be screened. By default,
  `ynames = c("hgt", "wgt", "hdc")`.

- ...:

  Not used, but required for extendibility

## Value

A data frame with the following columns

- `Versie`:

  Version of `growthscreener` package

- `yname`:

  JAMES name of outcome measure

- `Categorie`:

  The category of the screening guidelines, 1000/2000/3000/4000

- `CategorieOmschrijving`:

  A string indicating the measure

- `JGZRichtlijn`:

  Name of JGZ Richtlijn

- `Code`:

  Integer message code

- `CodeOmschrijving`:

  A string with the message

## Examples

``` r
screeners <- list_screeners()
table(screeners$Categorie)
#> 
#> 1000 2000 3000 4000 
#>   45   26   17   13 
```
