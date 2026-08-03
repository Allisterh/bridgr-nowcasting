test_that("summary.mf_model returns a classed summary object", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    h = 1
  )

  model_summary <- summary(model)

  expect_s3_class(model_summary, "summary.mf_model")
  expect_false(inherits(model_summary, "mf_model"))
  expect_equal(model_summary$target_name, model$target_name)
  expect_equal(model_summary$nobs, nrow(model$estimation_set))
  expect_equal(model_summary$h, model$h)
})

test_that("summary.mf_model exposes a standard coefficient matrix", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    h = 1
  )

  coefficient_matrix <- coef(summary(model))

  expect_true(is.matrix(coefficient_matrix))
  expect_equal(
    colnames(coefficient_matrix),
    c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  )
  expect_equal(rownames(coefficient_matrix), names(coef(model)))
  expect_equal(
    coefficient_matrix[, "Estimate"],
    coef(model),
    ignore_attr = TRUE
  )
  expect_equal(
    coefficient_matrix[, "Std. Error"],
    sqrt(diag(vcov(model))),
    ignore_attr = TRUE
  )
  expect_true(all(coefficient_matrix[, "Pr(>|t|)"] >= 0))
  expect_true(all(coefficient_matrix[, "Pr(>|t|)"] <= 1))
})

test_that("summary standard errors follow the requested uncertainty method", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    h = 1
  )

  model_summary <- summary(model)

  expect_false(is.null(model_summary$coefficient_method))
  expect_equal(
    model_summary$coefficients[, "Std. Error"],
    sqrt(diag(vcov(model))),
    ignore_attr = TRUE
  )
})

test_that("summary.mf_model does not print and print.summary.mf_model does", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  # Assigning the result suppresses auto-printing, so `summary()` itself must
  # emit nothing; the report comes from the print method.
  expect_silent(model_summary <- summary(model))

  output <- capture.output(print(model_summary))
  expect_true(any(grepl("Mixed-frequency model summary", output, fixed = TRUE)))
  expect_true(any(grepl("Target equation coefficients:", output, fixed = TRUE)))

  capture.output(returned <- print(model_summary))
  expect_identical(returned, model_summary)
})
