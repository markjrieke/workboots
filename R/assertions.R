# Util function for checking workflow passed to predict/vi boots
assert_workflow <- function(workflow) {

  # assert that object is actually a workflow
  assertthat::assert_that(
    class(workflow) == "workflow",
    msg = "argument `workflow` must be of class \"workflow\"."
  )

  # assert that there are no remaining tuning parameters
  n_params <- length(workflow$fit$actions$model$spec$args)

  purrr::walk(
    seq(1, n_params, 1),
    ~assert_tune(workflow, .x)
  )

}

# Util function for checking that workflow argument is not set to tune()
assert_tune <- function(workflow, index) {

  # get model arg (need to know if it's NULL or not)
  model_arg <- rlang::eval_tidy(workflow$fit$actions$model$spec$args[[index]])

  # don't check if NULL
  if (!is.null(model_arg)) {

    assertthat::assert_that(
      rlang::eval_tidy(workflow$fit$actions$model$spec$args[[index]]) != tune(),
      msg = "all tuning parameters must be final."
    )

  }

}

# Util function for checking n param
assert_n <- function(n) {

  # >= 1
  assertthat::assert_that(
    n >= 1,
    msg = "argument `n` must be >= 1."
  )

  # don't pass double
  assertthat::assert_that(
    n == as.integer(n),
    msg = "argmuent `n` must be an integer."
  )

}

# Util function for checking training_data and new_data params
assert_pred_data <- function(workflow, data) {

  # get colnames from workflow
  cols_wf <- workflow$pre$actions$recipe$recipe$var_info$variable

  # get colnames from data
  cols_dat <- colnames(data)

  # check that all cols in wf appear in data
  assertthat::assert_that(
    sum(cols_wf %in% cols_dat) == length(cols_wf),
    msg = paste0("missing cols in training_data or new_data.\n",
                 "All predictors used in workflow must be present.")
  )

}

# Util function for checking .data passed to summary functions
assert_summary_data <- function(data) {

  # check for col .preds
  assertthat::assert_that(
    ".preds" %in% names(data),
    msg = "col `.preds` missing."

  )

  # check that .preds is list
  assertthat::assert_that(
    class(data$.preds) == "list",
    msg = "col `.preds` must be a list-col."
  )

  # check for col model.pred within .preds
  # (checks the first row as proxy for others)
  assertthat::assert_that(
    !is.null(data$.preds[[1]]$model.pred),
    msg = "col `model.pred` missing from list `.preds`."
  )

}

# Util function for checking conf interval
assert_conf <- function(conf) {

  # numeric
  assertthat::assert_that(
    is.numeric(conf),
    msg = "argument `conf` must be numeric."
  )

  # must be between [0, 1]
  assertthat::assert_that(
    conf >= 0 && conf <= 1,
    msg = "argument `conf` must be between [0, 1]."
  )

}
