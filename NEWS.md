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
