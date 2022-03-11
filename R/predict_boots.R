#' Fit and predict from a workflow using many bootstrap resamples.
#'
#' Generate a prediction interval from arbitrary model types using bootstrap
#' resampling. `predict_boots()` generates `n` bootstrap resamples, fits a model
#' to each resample (creating `n` models), then creates `n` predictions for each
#' observation in `new_data`.
#'
#' @details Since `predict_boots()` fits a new model to each resample, the
#'  argument `workflow` must not yet be fit. Any tuned hyperparameters must be
#'  finalized prior to calling `predict_boots()`.
#'
#' @return A tibble with a column indicating the row index of each observation in
#'  `new_data` and a nested list of the model predictions for each observation.
#'
#' @param new_data A tibble or dataframe used to make predictions.
#' @inheritParams vi_boots
#'
#' @export
#'
#' @importFrom rsample bootstraps
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest
#' @importFrom purrr map_dfc
#' @importFrom rlang :=
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
#' wf %>%
#'   predict_boots(n = 125, training_data = mtcars, new_data = mtcars)
#' }
predict_boots <- function(workflow,
                          n = 100,
                          training_data,
                          new_data,
                          ...) {

  # check arguments
  assert_workflow(workflow)
  assert_n(n)
  assert_pred_data(workflow, training_data)
  assert_pred_data(workflow, new_data)

  # create resamples from training set
  training_boots <-
    rsample::bootstraps(
      training_data,
      times = n,
      ...
    )

  # map sequence of indices to `predict_single_boot()`
  # returns a column of predictions for each model
  preds <-
    purrr::map_dfc(
      seq(1, n),
      ~predict_single_boot(
        workflow = workflow,
        boot_splits = training_boots,
        new_data = new_data,
        index = .x
      )
    )

  # nest & return predictions in long format
  preds <- tibble::rowid_to_column(preds)

  preds <-
    tidyr::pivot_longer(
      preds,
      dplyr::starts_with(".pred_"),
      names_to = "model",
      values_to = "model.pred"
    )

  preds <-
    tidyr::nest(
      preds,
      .preds = c(model, model.pred)
    )

  return(preds)

}

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

# --------------------------------internals-------------------------------------

#' Fit a model and predict based on a single bootstrap resample
#'
#' @param workflow An un-fitted workflow object.
#' @param boot_splits A bootstrap split object created by `rsample::bootstraps()`.
#' @param new_data New data to make predictions
#' @param index Index of `boot_splits` to use for training
#'
#' @importFrom rsample training
#' @importFrom generics fit
#' @importFrom stats predict
#' @importFrom stats resid
#' @importFrom workflows extract_fit_engine
#' @importFrom dplyr rename
#' @importFrom rlang sym
#'
predict_single_boot <- function(workflow,
                                boot_splits,
                                new_data,
                                index) {

  # get training data from bootstrap resample split
  boot_train <-
    rsample::training(
      boot_splits$splits[[index]]
    )

  # fit workflow to training data
  model <- generics::fit(workflow, boot_train)

  # predict given model and new data
  preds <- stats::predict(model, new_data)

  # get predicted var name
  pred_name <- dplyr::filter(workflow$pre$actions$recipe$recipe$var_info, role == "outcome")
  pred_name <- dplyr::pull(pred_name, variable)

  # get actual dependent values
  actuals <- dplyr::pull(boot_train, rlang::sym(pred_name))

  # get resids
  resids <- dplyr::pull(stats::predict(model, boot_train), .pred) - actuals

  # add resid sample to each prediction
  preds <- tibble::add_column(preds, resid_sample = sample(resids, nrow(new_data), replace = TRUE))
  preds <- dplyr::mutate(preds, .pred = .pred + resid_sample)
  preds <- preds[, 1]

  # rename .pred col based on index number
  preds <- dplyr::rename(preds, !!rlang::sym(paste0(".pred_", index)) := .pred)

  return(preds)

}


