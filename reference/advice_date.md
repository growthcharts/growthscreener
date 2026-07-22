# Pick ideal date for next visitation

The function picks the best date for a next visitation in the JGZ based
on what has already been done and what still needs doing.

## Usage

``` r
calculate_advice_date(age, bds_df = NULL)
```

## Arguments

- age:

  Age of child in decimal years

- bds_df:

  `data.frame` object with the named columns `bds` containing bds
  numbers and `time` containing decimal years.

## Author

Arjan Huizing, 2023
