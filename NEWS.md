# growthscreener 1.23.0

- Extends `calculate_th()` with an extra output (prediction error) useful for calculating the target height range

# growthscreener 1.22.0

- Extends the `calculate_th()` function with a `support_missing_hgtf` argument to allow for the calculation of target height from only the mother's height. This is useful when height from the biological father height is missing. It currently works only for Dutch populations. If mother's height is missing, the function will return `NA`, as before.
- Adds a silent safety trim of input arguments of `calculate_th()` to scalar values. This is to prevent the function from breaking when the input is a vector of length > 1. The function will now only use the first element of the input vector.
- Adds functions `hermanussencole()` and `targetheight()` from the `chartdesigner` package. In contrast to `calculate_th()` these functions are vectorised, so more suited for group applications rather than individual application.

# growthscreener 1.21.0

- Merges `srm` branch updates (v1.19.3) into main
- Undo the choice for weight for height guidelines for period 0-2 years 
  throughout (made SvB 13jul20)
	This choice causes issues in practice as our advice does not line up
  with what the professional sees on their own diagrams. Sticking with 2012
  guidelines even if they are inconsistent.
- Makes `list_screeners()` aware of the language development guidelines
- Introduces `lgd` as abbreviation for language development

# growthscreener 1.20.0

- Merges srm brnach into master
- Updates NEWS.md

# growthscreener 1.19.0

- Version increases 1.16.0 --> 1.19.0 were made in srm branch
- Adds advice algorithm for future visit date (remind date guideline)
- Adds screener for language development, 2 yrs
- Changes all dates to `yyyymmdd`	
- Merges with master

# growthscreener 1.16.1

- Extrapolates the BMI cutoff table beyond 18 years to evade error when the child age is between 18 and 19 years
- Silences the Rm CMD CHECK error on global variable `age`
- Deprecates `calculate_screening_doms()`
- Removes `lubridate` dependency
- Makes sure `ga` is in completed weeks 

# growthscreener 1.16.0

- Merges the srm branch
- Generalises screening functions from two time points to any number of time points
- Assumes that the last time point is current
- Returns one advice for all time points combined (rather than m-1 retrospective pairs)
- Adds a new screening algorithm for language based on six Van Wiechen items
- Contains numerous other improvements
- Updates to roxygen version to 7.2.3
- Updates GitHub actions

# growthscreener 1.15.0

- Update roxygen version to 7.2.1

# growthscreener 1.14.0

- Siplify data structure to list with elements `psn` and `xyz`

# growthscreener 1.13.0

- Adds support for the new `target` class of the `bdsreader` package

# growthscreener 1.12.0 

- Adds `\dots` argument to `screen_curves_ind()`, `screen_curves_tgt()` and `list_screeners()` for extendibility

# growthscreener 1.11.0 

- Adds `list_screeners()` to produce overview of screeners

# growthscreener 1.10.0 

* Relies now on new `auto_format` functionality from `bdsreader`
* Solves some smaller bugs
* Define stricter dependencies

# growthscreener 1.9.0 

* **Breaking changes**: 
* Replaces the `minihealth` package by `bdsreader`
* Replaces the `jamestest` package by `jamesdemodata`

# growthscreener 1.8.0 

* Uses `nlreferences` package

# growthscreener 1.7.0 

* Major rewrite to replace the `clopus` by the `centile` package for clearer, easier and extendable reference selection and calculation

# growthscreener 1.6.1

* Explicit declaration of `clopus`

# growthscreener 1.6.0

* This version improves consistency in sieves and messages
* Adds checks on missing data for `z1` in some edge cases where needed
* Adds messages `**24` and `**25` for edge cases where the Z-score cannot be calculated, even if all information is available
* Adds a `test_gain` argument to screening function that we may use to disable any checking on gain scores `z1 - z0` (e.g. when there is only one observation for a child)
* Uses stricter scalar comparison operators `&&` and `||`
* Makes `SD` and `SDS` consistent in messages
* Replaces text "voorlaatste" to the more generic "eerdere"
* Adds test code that runs all `json` files in `jamestest`

# growthscreener 1.5.0

* Introduces `pick_reference_wgt()` to select weight reference depending on sex, age, ga and etn
* Refreshes the sieve used for under- and overweight screening
* Solves a bug in `calculate_screening_doms()` that prevented weight screening
* For clarity, reverts the weight screening code from `wfh` to `wgt`

# growthscreener 1.4.2

* Updates to `roxygen 7.1.1`
* Evades testing problem by adding `jamestest` to `suggests:`

# growthscreener 1.4.1

* Updates to `roxygen 7.1.0`

# growthscreener 1.4.0

* Renames `wgt` to the more appropriate `wfh` guidelines
* Generalises `calculate_screening_doms()` to work with `wfh` and `hdc`
* Documents `calculate_screening_doms()`
* Renumbers messages according to type of advice
* Adds a more detailed BMI-table and applies `approx()` instead of `floor()`
* Adds an `na.omit()` argument to `screen_curves_ind()` to avoid boring NA messages
* Adds a `recalculate_z` argument to `screen_curves_ind()` to avoid superfluous calculation (not yet used)
* Adds the package version number to the results

# growthscreener 1.3.1

* Remove etnicity from `wgt` and `hdc` 
* Set default `etn = "NL"` for `hgt`

# growthscreener 1.3.0

* Restructures the computational flow to adapt to multiple screeners
* Renumbers the messages starting with 1000's
* Adds new screeners for weight and head circumference
* Replaces the `AGD` dependency by `clopus`
* Uses the updated preterm WFH references (in `clopus 0.43.0`)
* Adds Arjan Huizing as author

# growthscreener 1.2.0

* Adds test for height screening

# growthscreener 1.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added `convert_msgcode_json()`

# growthscreener 1.0.0

* First complete version for height screening.
* Define `calculate_advice_hgt()`
* Define `calculate_helpers_hgt()`
* Define message table `messages_hgt`
* Define `msg()` function
* Add datasets and helper functions
