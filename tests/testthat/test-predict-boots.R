
test_that("predict_boots() returns prediction interval in expected format", {
  skip_if_not_installed("xgboost")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  test_wf_fit <- fit(test_wf, test_train)

  # generate predictions
  expect_warning(
    x <-
      predict_boots(
        workflow = test_wf_fit,
        n = 5,
        training_data = test_train,
        new_data = test_test
      ),

    "At least 2000 resamples recommended for stable results."
  )

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("rowid", ".preds"))
  expect_named(x$.preds[[1]], c("model", "model.pred"))
  expect_type(x$rowid, "integer")
  expect_type(x$.preds, "list")
  expect_type(x$.preds[[1]]$model, "character")
  expect_type(x$.preds[[1]]$model.pred, "double")
  expect_equal(nrow(x), nrow(test_test))

})

test_that("predict_boots() returns confidence interval in expected format", {
  skip_if_not_installed("xgboost")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  test_wf_fit <- fit(test_wf, test_train)

  # generate predictions
  expect_warning(
    x <-
      predict_boots(
        workflow = test_wf_fit,
        n = 5,
        training_data = test_train,
        new_data = test_test,
        interval = "confidence"
      ),

    "At least 2000 resamples recommended for stable results."
  )

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("rowid", ".preds"))
  expect_named(x$.preds[[1]], c("model", "model.pred"))
  expect_type(x$rowid, "integer")
  expect_type(x$.preds, "list")
  expect_type(x$.preds[[1]]$model, "character")
  expect_type(x$.preds[[1]]$model.pred, "double")
  expect_equal(nrow(x), nrow(test_test))

})

test_that("predict_boots() throws an error when not passed a workflow", {

  expect_error(
    predict_boots(
      workflow = test_train,
      n = 1,
      training_data = test_train,
      new_data = test_test
    ),

    "argument `workflow` must be of class \"workflow\"."
  )

})

test_that("predict_boots() throws an error when workflow is not fitted", {

  expect_error(
    predict_boots(
      workflow = test_wf_bad,
      n = 1,
      training_data = test_train,
      new_data = test_test
    ),

    "all tuning parameters must be final."
  )

})

test_that("predict_boots() throws an error when bad n is specified", {
  skip_if_not_installed("xgboost")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  test_wf_fit <- fit(test_wf, test_train)

  expect_error(
    predict_boots(
      workflow = test_wf_fit,
      n = 0,
      training_data = test_train,
      new_data = test_test
    ),

    "argument `n` must be >= 1."
  )

  expect_error(
    predict_boots(
      workflow = test_wf,
      n = 1.5,
      training_data = test_train,
      new_data = test_test
    ),

    "argmuent `n` must be an integer."
  )

})

test_that("predict_boots() throws an error when training_data/new_data doesn't match expected format", {
  skip_if_not_installed("xgboost")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  test_wf_fit <- fit(test_wf, test_train)


  # predictors & outcome missing from training_data
  expect_snapshot(
    predict_boots(
      workflow = test_wf_fit,
      n = 1,
      training_data = test_train[, 3],
      new_data = test_test
    ),
    error = TRUE
  )

  # predictors missing from new_data
  expect_snapshot(
    predict_boots(
      workflow = test_wf_fit,
      n = 1,
      training_data = test_train,
      new_data = test_test[, 3]
    ),
    error = TRUE
  )

})

