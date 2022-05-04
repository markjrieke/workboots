# workboots 0.2.0

### Function updates

* Rearranged column order output of `summarise_*` functions from `*_lower`, `*`, `*_upper` to `*`, `*_lower`, `*_upper`
* Deprecated `conf` parameter in `summarise_*` functions in favor of `interval_width`

# workboots 0.1.1

### Function updates

* Updates to `predict_boots()`
  + updated function to generate prediction interval (previously was generating a prediction's confidence interval)
  + updated default setting to assume residuals are normally distributed
  + updated default number of resamples
  + updated function to draw residuals based on the [632+ rule](https://stats.stackexchange.com/questions/96739/what-is-the-632-rule-in-bootstrapping) (previously was using training residuals)
* updated default number of resamples in `vi_boots()`
* added param `verbose` to both `predict_boots()` and `vi_boots()` to display progress in the console. 
* added new function `summarise_importance()`, as well alias `summarize_*` for `summarise_*`

### Bug fixes

* Fixed bug in `assert_pred_data()` that caused some workflows to be rejected by `predict_boots()`

# workboots 0.1.0

* Initial release
* Core functions:
  + `predict_boots()` for generating bootstrap prediction intervals from tidymodel workflows.
  + `summarise_predictions()` for summarizing bootstrap predictions with expected, lower bound, and upper bound values.
  + `vi_boots()` for generating bootstrap feature importance from tidymodel workflows.
* Vignette for Getting Started with workboots.
* Unit testing for core functions (via testthat).
