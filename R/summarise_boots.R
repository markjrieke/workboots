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
#' @param .data a tibble of predictions returned by `predict_boots()`.
#' @param conf an integer between (0, 1) specifying the interval range.
#' @param summary_type currently fixed as "quantile". More options may be added
#'  in later releases.
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stats quantile
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_wider
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
#' # fit and predict 125 bootstrap resampled models to mtcars
#' set.seed(123)
#' preds <-
#'   wf %>%
#'   predict_boots(n = 125, training_data = mtcars, new_data = mtcars)
#'
#' # append with prediction interval summary columns
#' preds %>%
#'   summarise_predictions(conf = 0.95)
#' }
summarise_predictions <- function(.data,
                                  conf = 0.95,
                                  summary_type = "quantile") {

  # check arguments
  assert_summary_data(.data)
  assert_conf(conf)

  # internal renaming
  pred_summary <- .data

  # determine ci_lower & ci_upper values from conf
  ci_lower <- (1 - conf)/2
  ci_upper <- ci_lower + conf

  # return max row
  n_rows <- nrow(pred_summary)

  # add interval - returns nested col
  if (summary_type == "quantile") {

    pred_summary <-
      dplyr::mutate(
        pred_summary,
        interval = purrr::map(pred_summary$.preds, ~stats::quantile(.x$model.pred, probs = c(ci_lower, 0.5, ci_upper)))
      )

  } else {

    message("Other summary_types will be added as part of new releases.")
    return()

  }

  # add interval labels & display as their own cols
  pred_summary <- tidyr::unnest(pred_summary, interval)
  pred_summary <-
    dplyr::mutate(
      pred_summary,
      pred_level = rep(c(".pred_lower", ".pred", ".pred_upper"), n_rows)
    )

  pred_summary <-
    tidyr::pivot_wider(
      pred_summary,
      names_from = pred_level,
      values_from = interval
    )

  return(pred_summary)

}
