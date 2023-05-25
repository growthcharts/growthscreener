
<!-- README.md is generated from README.Rmd. Please edit that file -->

# growthscreener

<!-- badges: start -->

[![R-CMD-check](https://github.com/growthcharts/growthscreener/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/growthcharts/growthscreener/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `growthscreener` package implements tools to evaluate child growth
with respect to Dutch criteria for unusual growth. Application of these
tools helps to identify children that meet criteria for criteria for
referral from youth health care (JGZ) for follow-up with a general
physician or paediatrician.

The current version implements Dutch guidelines for

- height
- weight
- head circumference
- language development

## Installation

The following statements will install the `growthscreener` package

``` r
install.packages("remotes")
remotes::install_github("growthcharts/growthscreener")
```

## Example

Find the advice for a very short girl:

``` r
library(growthscreener)
#> Loading required package: nlreferences

# a very short girl, 4 months old
msgcode <- calculate_advice_hgt(sex = "female", bw = 3250, ga = 40, dom = 0.367, y = 55)
msgcode
#> [1] 1045
cat(fold(msg(msgcode)))
#> Het advies volgens de JGZ-richtlijn lengtegroei is als volgt: Verwijzen naar
#> huisarts/kinderarts, omdat de lengte SDS < -3 is en het geboortegewicht >= 2500
#> gram is.
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
- **Beslisboom Hoofdomtrek**: A decision tree for head circumference for
  children below the age of 1 year.
- **JGZ-Richtlijn Taalontwikkeling 2018**:
  <https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/taalontwikkeling>

There are 45 different messages for height, 26 messages for weight and
17 messages for head circumference. To see them all:

``` r
messages
```
