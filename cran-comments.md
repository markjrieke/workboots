## Release summary

This release moves the package to 0.2.0. In this version I have:

* Rearranged column order output of `summarise_*` functions from `*_lower`, `*`, `*_upper` to `*`, `*_lower`, `*_upper`
* Deprecated `conf` parameter in `summarise_*` functions in favor of `interval_width`.
* Added support for generating confidence intervals from `predict_boots()`. 

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

