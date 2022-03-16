# workboots (development version)

### Function updates

* Updates to `predict_boots()`
  + updated function to generate prediction interval (previously was generating a prediction's confidence interval)
  + updated default setting to assume residuals are normally distributed (can draw from the actual residual distribution by setting `normal_resid = TRUE`)
  + updated default number of resamples
  + updated function to draw residuals from the out-of-bag (OOB) distribution (previously was using training residuals)
* updated default number of resamples in `vi_boots()`

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
