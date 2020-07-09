
<!-- README.md is generated from README.Rmd. Please edit that file -->

# growthscreener

<!-- badges: start -->

<!-- badges: end -->

The `growthscreener` package implements tools to evaluate child growth
with respect to Dutch criteria for unusual growth. Application of these
tools helps to identify children that meet criteria for criteria for
referral from youth health care (JGZ) for follow-up with a general
physician or pediatrician.

The current version implements Dutch guidelines for

  - height
  - weight
  - head circumference

## Installation

The following statements will install the `growthscreener` package

``` r
install.packages("remotes")
remotes::install_github("stefvanbuuren/growthscreener")
```

## Example

Find the advice for a very short girl:

``` r
library(growthscreener)

# a very short girl, 4 months old
msgcode <- calculate_advice_hgt(sex = "female", bw = 3250, ga = 40,
                                dob = as.Date("2018-07-31"),
                                dom1 = as.Date("2018-12-12"), 
                                y1 = 55)
msgcode
#> [1] 1045
cat(fold(msg(msgcode)))
#> Het advies volgens de JGZ-richtlijn lengtegroei is als volgt: Verwijzen naar
#> huisarts/kinderarts, omdat de lengte < -3 SDS is en het geboortegewicht >= 2500
#> gram is.

# some more details
d <- calculate_helpers(yname = "hgt", sex = "female", bw = 3250, ga = 40,
                       dob = as.Date("2018-07-31"),
                       dom1 = as.Date("2018-12-12"), 
                       y1 = 55)
unlist(d)
#>   bw_z   bl_z     th   th_z   age1   age0     z1     z0 
#> -0.474     NA     NA     NA  0.367     NA -3.255     NA
```

The height SDS at the age of about 4 months is equal to -3.255, which is
the reason for referral.

## Background

The package implements the following guidelines:

  - **JGZ-Richtlijn Lengtegroei 2019**:
    <https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/lengtegroei-2019>
  - **JGZ-Richtlijn Overgewicht 2012**:
    <https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/overgewicht>
  - **JGZ-Richtlijn Ondergewicht 2019**:
    <https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/ondergewicht-2019>
  - **Beslisboom Hoofdomtrek**: A decision tree for head circumference
    for children below the age of 1 year.

There are 43 different messages for height, 21 messages for weight and
15 messages for head circumference. To see them all:

``` r
messages
```
