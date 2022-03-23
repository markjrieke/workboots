#' Fit and estimate variable importance from a workflow using many bootstrap resamples.
#'
#' Generate a prediction interval from arbitrary model types using bootstrap
#' resampling. `predict_boots()` generates `n` bootstrap resamples, fits a model
#' to each resample (creating `n` models), then creates `n` estimates of variable
#' importance for each variable in the model.
#'
#' @details Since `vi_boots()` fits a new model to each resample, the
#'  argument `workflow` must not yet be fit. Any tuned hyperparameters must be
#'  finalized prior to calling `vi_boots()`.
#'
#' @return A tibble with a column indicating each variable in the model and a
#'  nested list of variable importances for each variable. The shape of the list
#'  may vary by model type. For example, linear models return two nested columns:
#'  the absolute value of each variable's importance and the sign (POS/NEG),
#'  whereas XGboost models return a single nested column of variable importance.
#'  Similarly, the number of nested rows may vary by model type as some models
#'  may not utilize every possible predictor.
#'
#' @param workflow An un-fitted workflow object.
#' @param training_data A tibble or dataframe of data to be resampled and used for training.
#' @param n An integer for the number of bootstrap resampled models that will be created.
#' @param verbose A logical. Defaults to `FALSE`. If set to `TRUE`, prints progress
#'   of training to console.
#' @param ... Additional params passed to `rsample::bootstraps()`.
#'
#' @export
#'
#' @importFrom rlang warn
#' @importFrom rsample bootstraps
#' @importFrom purrr map_dfr
#' @importFrom dplyr rename_with
#' @importFrom tidyr nest
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
#' # fit and estimate variable importance from 125 bootstrap resampled models
#' set.seed(123)
#' wf %>%
#'   vi_boots(n = 125, training_data = mtcars)
#' }
vi_boots <- function(workflow,
                     n = 2000,
                     training_data,
                     verbose = FALSE,
                     ...) {

  # check arguments
  assert_workflow(workflow)
  assert_n(n)
  assert_pred_data(workflow, training_data, "training")

  # warn if low n
  if (n < 2000) {

    rlang::warn(
      paste0("At least 2000 resamples recommended for stable results.")
    )

  }

  # create resamples from training set
  training_boots <-
    rsample::bootstraps(
      training_data,
      times = n,
      ...
    )

  # map sequence of indices to `vi_single_boot()`
  # returns a variable + importance for each model (number of cols may vary by model type)
  bootstrap_vi <-
    purrr::map_dfr(
      seq(1, n),
      ~vi_single_boot(
        workflow = workflow,
        boot_splits = training_boots,
        verbose = verbose,
        index = .x
      )
    )

  # rename cols
  bootstrap_vi <- dplyr::rename_with(bootstrap_vi, tolower)

  # return a nested tibble
  bootstrap_vi <- tidyr::nest(bootstrap_vi, importance = -variable)

  return(bootstrap_vi)

}

# -------------------------------internals--------------------------------------

#' Fit a model and get the variable importance based on a single bootstrap resample
#'
#' @param workflow passed from `vi_boots()`
#' @param boot_splits passed from `vi_boots()`
#' @param verbose passed from `vi_boots()`
#' @param index passed from `vi_boots()`
#'
#' @importFrom rsample training
#' @importFrom generics fit
#' @importFrom vip vi
#' @importFrom workflows extract_fit_engine
#'
vi_single_boot <- function(workflow,
                           boot_splits,
                           verbose,
                           index) {

  # get training data from bootstrap resample split
  boot_train <-
    rsample::training(
      boot_splits$splits[[index]]
    )

  # fit workflow to to the training data
  model <- generics::fit(workflow, boot_train)

  # get the variable importance from the model
  vi_boot <- vip::vi(workflows::extract_fit_engine(model))

  # print progress when verbose is set to TRUE
  verbose_print(
    verbose = verbose,
    index = index,
    total = nrow(boot_splits)
  )

  return(vi_boot)

}
