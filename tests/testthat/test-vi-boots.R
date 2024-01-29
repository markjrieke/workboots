test_that("vi_boots() returns importances in expected format", {

  skip_if_not_installed("xgboost")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  test_wf_fit <- fit(test_wf, test_train)

  # generate predictions
  expect_warning(
    x <-
      vi_boots(
        workflow = test_wf_fit,
        n = 5,
        training_data = test_train
      ),

    "At least 2000 resamples recommended for stable results."

  )

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("variable", ".importances"))
  expect_type(x$variable, "character")
  expect_type(x$.importances, "list")

})

test_that("vi_boots() throws an error when not passed a workflow", {

  expect_snapshot(
    vi_boots(
      workflow = test_train,
      n = 1,
      training_data = test_train
    ),
    error = TRUE
  )

})

test_that("vi_boots() throws an error when workflow is not fit", {

  expect_snapshot(
    vi_boots(
      workflow = test_wf_bad,
      n = 1,
      training_data = test_train
    ),
    error = TRUE
  )

})

test_that("vi_boots() throws an error when bad n is specified", {

  skip_if_not_installed("xgboost")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  test_wf_fit <- fit(test_wf, test_train)

  expect_snapshot(
    vi_boots(
      workflow = test_wf_fit,
      n = 0,
      training_data = test_train
    ),
    error = TRUE
  )

  expect_snapshot(
    vi_boots(
      workflow = test_wf_fit,
      n = 1.5,
      training_data = test_train
    ),
    error = TRUE
  )

})

test_that("vi_boots() throws an error when training_data doesn't match expected format", {

  skip_if_not_installed("xgboost")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  test_wf_fit <- fit(test_wf, test_train)

  # predictors & outcome missing from training_data
  expect_snapshot(
    vi_boots(
      workflow = test_wf_fit,
      n = 1,
      training_data = test_train[, 3]
    ),
    error = TRUE
  )

})



