
test_that("summarise_predictions() returns predictions in expected format", {

  # generate summary
  x <- summarise_predictions(test_preds)

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("rowid", ".preds", ".pred", ".pred_lower", ".pred_upper"))
  expect_type(x$.preds, "list")
  expect_type(x$.pred_lower, "double")
  expect_type(x$.pred, "double")
  expect_type(x$.pred_upper, "double")
  expect_type(x$.preds[[1]]$model, "character")
  expect_type(x$.preds[[1]]$model.pred, "double")
  expect_equal(nrow(x), nrow(test_test))

})

test_that("summarise_importances() returns importances in expected format", {

  # generate summary
  x <- summarise_importance(test_importances)

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("variable", ".importances", ".importance", ".importance_lower", ".importance_upper"))
  expect_type(x$.importances, "list")
  expect_type(x$.importance_lower, "double")
  expect_type(x$.importance, "double")
  expect_type(x$.importance_upper, "double")
  expect_type(x$.importances[[1]]$model, "character")
  expect_type(x$.importances[[1]]$model.importance, "double")

})


test_that("summarise_predictions() can use formula interface", {
  skip_if_not_installed("parsnip")
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(parsnip))

  car_subset <- mtcars[, c("mpg", "disp", "wt")]
  lm_wflow <- workflow(mpg ~ ., parsnip::linear_reg())
  lm_fit <- fit(lm_wflow, car_subset)

  new_car <- data.frame(disp = 150.0, wt = 2.5)

  # generate predictions
  expect_warning(
    x <-
      predict_boots(
        workflow = lm_fit,
        n = 5,
        training_data = car_subset,
        new_data = new_car
      ),

    "At least 2000 resamples recommended for stable results."
  )

  # tests
  # generate summary
  x <- summarise_predictions(x)

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("rowid", ".preds", ".pred", ".pred_lower", ".pred_upper"))
  expect_type(x$.preds, "list")
  expect_type(x$.pred_lower, "double")
  expect_type(x$.pred, "double")
  expect_type(x$.pred_upper, "double")
  expect_type(x$.preds[[1]]$model, "character")
  expect_type(x$.preds[[1]]$model.pred, "double")
  expect_equal(nrow(x), nrow(new_car))


})

