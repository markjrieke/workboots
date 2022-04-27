#' Append a tibble of predictions returned by `predict_boots()` with a prediction
#' interval.
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
#' @param conf a value between (0, 1) specifying the interval range.
#'
#' @export
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
                                  conf = 0.95) {

  # check arguments
  assert_pred_summary(.data)
  assert_conf(conf)

  # pass to summarise_generic
  summarise_generic(
    .data = .data,
    nest_col = ".preds",
    conf = conf
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
#' @param conf a value between (0, 1) specifying the interval range.
#'
#' @export
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
#' # evaluate variable importance from 125 models fit to mtcars
#' set.seed(123)
#' importances <-
#'   wf %>%
#'   vi_boots(n = 125, training_data = mtcars, new_data = mtcars)
#'
#' # append with lower and upper bound importance summary columns
#' importances %>%
#'   summarise_importance(conf = 0.95)
#' }
summarise_importance <- function(.data,
                                 conf = 0.95) {

  # check arguments
  assert_importance_summary(.data)
  assert_conf(conf)

  # pass arguments to summarise_generic
  summarise_generic(
    .data = .data,
    nest_col = "importance",
    conf = conf
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
#' @param conf passed from one of the summarise_* functions
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stats quantile
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_wider
#'
#' @noRd
#'
summarise_generic <- function(.data,
                              nest_col,
                              conf) {

  # internal renaming
  summary <- .data

  # determine ci_lower & ci_upper values from conf
  ci_lower <- (1 - conf)/2
  ci_upper <- ci_lower + conf

  # return max row
  n_rows <- nrow(summary)

  # map variable importances to quantile fn
  if (nest_col == ".preds") {

    # summarise predictions
    summary <-
      dplyr::mutate(
        summary,
        interval = purrr::map(summary$.preds, ~stats::quantile(.x$model.pred, probs = c(ci_lower, 0.5, ci_upper)))
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
        interval = purrr::map(summary$.importances, ~stats::quantile(.x$model.importance, probs = c(ci_lower, 0.5, ci_upper)))
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

  return(summary)

}
