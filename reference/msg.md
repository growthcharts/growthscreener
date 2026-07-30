# Find message string

Find message string

## Usage

``` r
msg(msgcode, prefix = TRUE)
```

## Arguments

- msgcode:

  Integer vector with message codes

- prefix:

  Logical. If `TRUE` (default), prepend the standard "Het advies volgens
  de JGZ-richtlijn ... is als volgt: " lead-in to advice messages
  (msgcodes 1031-1082, 2031-2076 and 4031-4046).

## Value

A vector of strings with the message code

## Examples

``` r
msg(c(31, 41))
#> [1] "" ""
msg(c(31, 41), prefix = FALSE)
#> [1] "" ""
```
