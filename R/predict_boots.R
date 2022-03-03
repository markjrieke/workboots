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
        index = .x,
        seed = seed
      )
    )

  # nest & return predictions in long format
  preds <- tibble::rowid_to_column(preds)

  preds <-
    tidyr::pivot_longer(
      preds,
      dplyr::starts_with(".pred_"),
      names_to = "model",
      values_to = ".pred"
    )

  preds <-
    tidyr::nest(
      preds,
      .preds = c(model, .pred)
    )

  return(preds)

}

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

  # rename .pred col based on index number
  preds <- dplyr::rename(preds, !!rlang::sym(paste0(".pred_", index)) := .pred)

  return(preds)

}
