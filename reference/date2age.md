# Calculate decimal age from date of birth and date of measurement

Calculate decimal age from date of birth and date of measurement

## Usage

``` r
date2age(dob = NA_character_, dom = NA_character_)
```

## Arguments

- dob:

  Date of birth

- dom:

  Date of measurement

## Note

Internal function. Not to be called directly.

## Examples

``` r
growthscreener:::date2age(dob = "20200217", dom = "20210604")
#> [1] 1.295
```
