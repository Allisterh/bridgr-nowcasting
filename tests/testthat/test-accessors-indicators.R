test_that("indicators() returns the indicator ids in model order", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  expect_equal(indicators(model), model$indic_name)
  expect_type(indicators(model), "character")
})

test_that("weights() returns user-supplied aggregation weights", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = list(c(0.2, 0.3, 0.5)),
    h = 1
  )

  all_weights <- weights(model)

  expect_type(all_weights, "list")
  expect_named(all_weights, indicators(model))
  expect_equal(all_weights[[1]], c(0.2, 0.3, 0.5))

  # Selecting by name and by position must agree.
  expect_equal(weights(model, indicator = 1), c(0.2, 0.3, 0.5))
  expect_equal(
    weights(model, indicator = indicators(model)[[1]]),
    c(0.2, 0.3, 0.5)
  )
})

test_that("weights() returns estimated parametric aggregation weights", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- suppressWarnings(mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "beta",
    solver_options = list(
      start_values = c(2, 3),
      seed = 42,
      n_starts = 1,
      maxiter = 100
    ),
    h = 1
  ))

  indicator_id <- indicators(model)[[1]]

  # This replaces reaching into `model$parametric_weights[[...]]` directly.
  expect_equal(
    weights(model, indicator = indicator_id),
    as.numeric(model$parametric_weights[[indicator_id]])
  )
  expect_equal(
    aggregation_parameters(model, indicator = indicator_id),
    model$parametric_parameters[[indicator_id]]
  )
  expect_named(aggregation_parameters(model), indicator_id)
})

test_that("weights() returns the weights implied by deterministic rules", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  fit <- function(aggregator) {
    mf_model(
      target = target,
      indic = indic,
      indic_predict = "last",
      indic_aggregators = aggregator,
      h = 1
    )
  }

  # A monthly indicator on a quarterly target implies three within-period
  # slots, so the deterministic rules of `mf_model()` imply these weights.
  expect_equal(weights(fit("mean"), indicator = 1), rep(1 / 3, 3))
  expect_equal(weights(fit("sum"), indicator = 1), rep(1, 3))
  expect_equal(weights(fit("last"), indicator = 1), c(0, 0, 1))

  # Deterministic rules imply no parametric parameters.
  expect_null(aggregation_parameters(fit("mean"), indicator = 1))
})

test_that("weights() is NULL when no single weight vector applies", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  # `unrestricted` estimates one coefficient per within-period observation
  # rather than aggregating, so no weight vector is implied.
  unrestricted <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "unrestricted",
    h = 1
  )

  expect_null(weights(unrestricted, indicator = 1))
  expect_null(aggregation_parameters(unrestricted, indicator = 1))
})

test_that("variable.names() reports the bridge-equation regressors", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "mean",
    target_lags = 1,
    h = 1
  )

  # This replaces reaching into `model$xreg_names` directly.
  expect_equal(variable.names(model), model$regressor_names)
  expect_equal(variable.names(model, which = "xreg"), model$xreg_names)
  expect_equal(
    variable.names(model, which = "target_lags"),
    model$target_lag_names
  )

  # The `xreg` group is exactly the estimation regressors that are not
  # autoregressive target lags, which is what a custom `xreg` must supply.
  expect_setequal(
    c(
      variable.names(model, which = "xreg"),
      variable.names(model, which = "target_lags")
    ),
    variable.names(model)
  )
  expect_error(variable.names(model, which = "nope"), "should be one of")
})

test_that("indicator selectors validate their input", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  expect_error(weights(model, indicator = "nope"), "not found")
  expect_error(weights(model, indicator = 99), "out of range")
  expect_error(weights(model, indicator = c(1, 2)), "exactly one")
  expect_error(aggregation_parameters(model, indicator = "nope"), "not found")
})

test_that("model.frame() returns the estimation and forecast frames", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 2
  )

  expect_equal(model.frame(model), model$estimation_set)
  expect_equal(model.frame(model, which = "estimation"), model$estimation_set)
  expect_equal(model.frame(model, which = "forecast"), model$forecast_base_set)
  expect_equal(nrow(model.frame(model, which = "forecast")), 2)

  # The forecast frame carries the regressors, not the forecast target column.
  expect_false(model$target_name %in% names(model.frame(model, "forecast")))
})
