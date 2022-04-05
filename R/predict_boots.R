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
#' @importFrom rlang warn
#' @importFrom rsample bootstraps
#' @importFrom purrr map_dfc
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr pivot_longer
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
#' # fit and predict 125 bootstrap resampled models to mtcars
#' set.seed(123)
#' wf %>%
#'   predict_boots(n = 125, training_data = mtcars, new_data = mtcars)
#' }
predict_boots <- function(workflow,
                          n = 2000,
                          training_data,
                          new_data,
                          verbose = FALSE,
                          ...) {

  # check arguments
  assert_workflow(workflow)
  assert_n(n)
  assert_pred_data(workflow, training_data, "training")
  assert_pred_data(workflow, new_data, "new")

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

  # map sequence of indices to `predict_single_boot()`
  # returns a column of predictions for each model
  preds <-
    purrr::map_dfc(
      seq(1, n),
      ~predict_single_boot(
        workflow = workflow,
        boot_splits = training_boots,
        new_data = new_data,
        verbose = verbose,
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

# --------------------------------internals-------------------------------------

#' (Internal) Generate a column of predictions on new data based on a model fit
#' to a single training bootstrap.
#'
#' @param workflow passed from `predict_boots()`
#' @param boot_splits passed from `predict_boots()`
#' @param new_data passed from `predict_boots()`
#' @param verbose passed from `predict_boots()`
#' @param index passed from `predict_boots()`
#'
#' @importFrom rsample training
#' @importFrom rsample testing
#' @importFrom generics fit
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom stats predict
#' @importFrom rlang sym
#' @importFrom tidyr crossing
#' @importFrom Metrics rmse
#' @importFrom stats sd
#' @importFrom tibble add_column
#' @importFrom stats rnorm
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom rlang :=
#'
predict_single_boot <- function(workflow,
                                boot_splits,
                                new_data,
                                verbose,
                                index) {

  # get training data from bootstrap resample split
  boot_train <-
    rsample::training(
      boot_splits$splits[[index]]
    )

  # get oob sample
  boot_oob <-
    rsample::testing(
      boot_splits$splits[[index]]
    )

  # fit workflow to training data
  model <- generics::fit(workflow, boot_train)

  # get predicted var name
  pred_name <- dplyr::filter(workflow$pre$actions$recipe$recipe$var_info, role == "outcome")
  pred_name <- dplyr::pull(pred_name, variable)

  # get training residuals
  preds_train <- dplyr::pull(stats::predict(model, boot_train), .pred)
  actuals_train <- dplyr::pull(boot_train, rlang::sym(pred_name))
  resids_train <- actuals_train - preds_train
  resids_train <- resids_train - mean(resids_train)

  # get oob residuals
  preds_oob <- dplyr::pull(stats::predict(model, boot_oob), .pred)
  actuals_oob <- dplyr::pull(boot_oob, rlang::sym(pred_name))
  resids_oob <- actuals_oob - preds_oob
  resids_oob <- resids_oob - mean(resids_oob)

  # calculate no-information error rate (rmse_ni) with RMSE as loss function
  combos <- tidyr::crossing(actuals_train, preds_train)
  rmse_ni <- Metrics::rmse(combos$actuals_train, combos$preds_train)

  # calculate overfit rate
  rmse_oob <- Metrics::rmse(actuals_oob, preds_oob)
  rmse_train <- Metrics::rmse(actuals_train, preds_train)
  overfit <- (rmse_oob - rmse_train)/(rmse_ni - rmse_train)

  # calculate weight (if overfit = 0, weight = .632 & residual used will just be .632)
  # uses the actual proportion of distinct training/oob samples, rather than average of 0.632/0.368
  prop_368 <- nrow(boot_oob)/nrow(boot_train)
  prop_632 <- 1 - prop_368
  weight <- prop_632/(1 - (prop_368 * overfit))

  # determine residual std.dev based on weight
  sd_oob <- stats::sd(resids_oob)
  sd_train <- stats::sd(resids_train)
  sd_resid <- weight * sd_oob + (1 - weight) * sd_train

  # predict given model and new data
  preds <- stats::predict(model, new_data)

  # add residuals to fit
  preds <- tibble::add_column(preds, resid_add = stats::rnorm(nrow(new_data), 0, sd_resid))
  preds <- dplyr::mutate(preds, .pred = .pred + resid_add)
  preds <- preds[, 1]

  # rename .pred col based on index number
  preds <- dplyr::rename(preds, !!rlang::sym(paste0(".pred_", index)) := .pred)

  # print progress when verbose is set to TRUE
  verbose_print(verbose, index, nrow(boot_splits))

  return(preds)

}


