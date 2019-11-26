
<!-- README.md is generated from README.Rmd. Please edit that file -->

# growthscreener

<!-- badges: start -->

<!-- badges: end -->

The `growthscreener` package implements tools to evaluate child growth
with respect to Dutch criteria for unusual growth. Application of these
tools helps to identify children that meet criteria for criteria for
referral from youth health care (JGZ) for follow-up with a general
physician or pediatrician.

Dutch guidelines for unusual height growth are currently implemented.

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
msgcode <- calculate_advice_hgt(sex = "F", etn = "N", 
                                bw = 3250, ga = 40,
                                dob = as.Date("2018-07-31"),
                                dom1 = as.Date("2018-12-12"), 
                                y1 = 55)
msgcode
#> [1] 45
cat(fold(msg(msgcode)))
#> Het advies volgens de JGZ-richtlijn lengtegroei is als volgt: Verwijzen naar
#> huisarts/kinderarts, omdat de lengte < -3 SDS is en het geboortegewicht >= 2500
#> gram is.

# some more details
d <- calculate_helpers_hgt(sex = "F", etn = "N", 
                           bw = 3250, ga = 40,
                           dob = as.Date("2018-07-31"),
                           dom1 = as.Date("2018-12-12"), 
                           y1 = 55)
d
#> $bw_z
#> [1] -0.474
#> 
#> $bl_z
#> [1] NA
#> 
#> $th
#> [1] NA
#> 
#> $th_z
#> [1] NA
#> 
#> $age1
#> [1] 0.367
#> 
#> $age0
#> [1] NA
#> 
#> $z1
#> [1] -3.26
#> 
#> $z0
#> [1] NA
```

The height SDS at the age of 4 months is equal to -3.26, which is the
reason for referral. There are 40 different messages for height.

## Background

The calculations follow to the “JGZ-Richtlijn Lengtegroei 2019”. See
<https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/lengtegroei-2019>
for more details.
