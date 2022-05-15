#' Append a tibble of predictions returned by `predict_boots()` with upper and
#' lower bounds.
#'
#' @details Generates a summary of predictions with a upper and lower interval
#'  range. Presently, the `quantile()` function from the `{stats}` package is
#'  used to determine the lower, 50th percentile, and upper interval ranges.
#'
#' @return Appends the tibble of predictions returned by `predict_boots()` with
#'  three new columns: `.pred_lower`, `.pred`, and `.pred_upper`.
#'
#' @aliases `summarize_predictions()`
#'
#' @param .data a tibble of predictions returned by `predict_boots()`.
#' @param interval_width a value between (0, 1) specifying the interval range.
#' @param conf deprecated - please use `interval_width` instead.
#'
#' @export
#'
#' @importFrom lifecycle deprecate_soft
#'
#' @examples
#' \dontrun{
#' library(tidymodels)
#'
#' # setup a workflow without fitting
#' wf <-
#'   workflow() %>%
#'   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
#'   add_model(linear_reg())
#'
#' # fit and predict 2000 bootstrap resampled models to mtcars
#' set.seed(123)
#' preds <-
#'   wf %>%
#'   predict_boots(n = 2000, training_data = mtcars, new_data = mtcars)
#'
#' # append with prediction interval summary columns
#' preds %>%
#'   summarise_predictions(conf = 0.95)
#' }
summarise_predictions <- function(.data,
                                  interval_width = 0.95,
                                  conf = NULL) {

  # warn about parameter deprecation
  if (!is.null(conf)) {

    lifecycle::deprecate_soft(
      when = "0.2.0",
      what = "summarise_predictions(conf)",
      with = "summarise_predictions(interval_width)"
    )

    # reassign to interval width
    interval_width <- conf

  }

  # check arguments
  assert_pred_summary(.data)
  assert_interval(interval_width)

  # pass to summarise_generic
  summarise_generic(
    .data = .data,
    nest_col = ".preds",
    interval_width = interval_width
  )

}
#' @rdname summarise_predictions
#' @export
summarize_predictions <- summarise_predictions

#' Append a tibble of variable importances returned by `vi_boots()` with upper
#' and lower bounds.
#'
#' @details Generates a summary of variable importances with an upper and lower
#'  interval range. Uses the `vi()` function from the `{vip}` package to compute
#'  variable importances (not all model types are supported by `vip::vi()`; please
#'  refer to `{vip}` package documentation for supported model types). Presently,
#'  the `quantile()` function from the `{stats}` package is used to determine
#'  the lower, 50th percentile, and upper interval ranges.
#'
#' @param .data a tibble of variable importances returned by `vi_boots()`.
#' @param interval_width a value between (0, 1) specifying the interval range.
#' @param conf deprecated - please use `interval_width` instead.
#'
#' @export
#'
#' @importFrom lifecycle deprecate_soft
#'
#' @examples
#' \dontrun{
#' library(tidymodels)
#'
#' # setup a workflow without fitting
#' wf <-
#'   workflow() %>%
#'   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
#'   add_model(linear_reg())
#'
#' # evaluate variable importance from 2000 models fit to mtcars
#' set.seed(123)
#' importances <-
#'   wf %>%
#'   vi_boots(n = 2000, training_data = mtcars, new_data = mtcars)
#'
#' # append with lower and upper bound importance summary columns
#' importances %>%
#'   summarise_importance(interval_width = 0.95)
#' }
summarise_importance <- function(.data,
                                 interval_width = 0.95,
                                 conf = NULL) {

  # warn about parameter deprecation
  if (!is.null(conf)) {

    lifecycle::deprecate_soft(
      when = "0.2.0",
      what = "summarise_importance(conf)",
      with = "summarise_importance(interval_width)"
    )

    # reassign to interval width
    interval_width <- conf

  }

  # check arguments
  assert_importance_summary(.data)
  assert_interval(interval_width)

  # pass arguments to summarise_generic
  summarise_generic(
    .data = .data,
    nest_col = "importance",
    interval_width = interval_width
  )

}
#' @rdname summarise_importance
#' @export
summarize_importance <- summarise_importance

# ------------------------------internals---------------------------------------

#' (Internal) Function for generating generic summaries from either `predict_boots()`
#' or `vi_boots()`.
#'
#' @param .data passed from one of the summarise_* functions
#' @param nest_col passed from one of the summarise_* functions
#' @param interval_width passed from one of the summarise_* functions
#' @param conf passed from one of the summarise_* functions
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stats quantile
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr relocate
#'
#' @noRd
#'
summarise_generic <- function(.data,
                              nest_col,
                              interval_width) {

  # internal renaming
  summary <- .data

  # determine ci_lower & ci_upper values from conf
  int_lower <- (1 - interval_width)/2
  int_upper <- int_lower + interval_width

  # return max row
  n_rows <- nrow(summary)

  # map variable importances to quantile fn
  if (nest_col == ".preds") {

    # summarise predictions
    summary <-
      dplyr::mutate(
        summary,
        interval = purrr::map(summary$.preds, ~stats::quantile(.x$model.pred, probs = c(int_lower, 0.5, int_upper)))
      )

    # vector of lower/med/upper column names
    col_vec <- rep(c(".pred_lower", ".pred", ".pred_upper"), n_rows)

  } else {

    # adjust the importance cols if needed
    if (ncol(summary$.importances[[1]]) > 2) {

      summary <- tidyr::unnest(summary, .importances)
      summary <- dplyr::mutate(summary, model.importance = ifelse(sign == "NEG", -model.importance, model.importance))
      summary <- dplyr::select(summary, -sign)
      summary <- tidyr::nest(summary, .importances = -variable)

    }

    # summarise variable importances
    summary <-
      dplyr::mutate(
        summary,
        interval = purrr::map(summary$.importances, ~stats::quantile(.x$model.importance, probs = c(int_lower, 0.5, int_upper)))
      )

    # vector of lower/med/upper column names
    col_vec <- rep(c(".importance_lower", ".importance", ".importance_upper"), n_rows)

  }

  # add interval labels & display as their own cols
  summary <- tidyr::unnest(summary, interval)
  summary <-
    dplyr::mutate(
      summary,
      int_level = col_vec
    )

  summary <-
    tidyr::pivot_wider(
      summary,
      names_from = int_level,
      values_from = interval
    )

  # rearrange cols to be .mid .lower .upper
  if (nest_col == ".preds") {

    summary <- dplyr::relocate(summary, .pred_lower, .after = .pred)

  } else {

    summary <- dplyr::relocate(summary, .importance_lower, .after = .importance)

  }

  return(summary)

}
