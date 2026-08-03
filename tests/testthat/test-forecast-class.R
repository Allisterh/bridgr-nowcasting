test_that("mf_model_forecast does not claim forecast inheritance", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  fcst <- forecast(model)

  expect_s3_class(fcst, "mf_model_forecast")
  expect_false(inherits(fcst, "forecast"))
})

test_that("plot and autoplot work on a mixed-frequency forecast", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 2
  )

  fcst <- forecast(model)

  expect_s3_class(plot(fcst), "ggplot")
  expect_s3_class(ggplot2::autoplot(fcst), "ggplot")
})

test_that("plot.mf_model_forecast draws stored prediction intervals", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8),
    h = 2
  )

  fcst <- forecast(model, level = c(80, 95))

  expect_s3_class(plot(fcst, level = 80), "ggplot")
  expect_s3_class(plot(fcst, level = 95), "ggplot")
  expect_error(plot(fcst, level = 50), "computed with")
})

test_that("as.forecast returns a working forecast object for regular targets", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8),
    h = 2
  )

  converted <- as.forecast(forecast(model))

  expect_s3_class(converted, "forecast")
  expect_s3_class(converted$mean, "ts")
  expect_s3_class(converted$x, "ts")
  expect_s3_class(converted$fitted, "ts")
  expect_s3_class(converted$residuals, "ts")
  expect_equal(stats::frequency(converted$mean), 4)
  expect_equal(length(converted$mean), 2)

  # The methods that failed under the previous, unhonoured inheritance.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot(converted))
  expect_s3_class(ggplot2::autoplot(converted), "ggplot")
  expect_true(is.numeric(forecast::accuracy(converted)))
})

test_that("as.forecast preserves point forecasts and intervals", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8),
    h = 2
  )

  fcst <- forecast(model, level = c(80, 95))
  converted <- as.forecast(fcst)

  expect_equal(as.numeric(converted$mean), as.numeric(fcst$mean))
  expect_equal(as.numeric(converted$lower), as.numeric(fcst$lower))
  expect_equal(as.numeric(converted$upper), as.numeric(fcst$upper))
  expect_equal(converted$level, fcst$level)
  expect_equal(converted$series, model$target_name)
})

test_that("as.forecast rejects target frequencies ts cannot represent", {
  fixture <- make_daily_week_fixture(n_weeks = 8, h = 1)

  model <- mf_model(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = "last",
    h = 1
  )

  fcst <- forecast(model)

  expect_error(as.forecast(fcst), "no exact `stats::ts\\(\\)` representation")

  # The native methods must still work at this frequency.
  expect_s3_class(plot(fcst), "ggplot")
})

test_that("ts_frequency maps only exactly representable target frequencies", {
  frequency_for <- function(unit, step) {
    bridgr:::ts_frequency(dplyr::tibble(unit = unit, step = step))
  }

  expect_equal(frequency_for("year", 1), 1L)
  expect_equal(frequency_for("month", 6), 2L)
  expect_equal(frequency_for("quarter", 1), 4L)
  expect_equal(frequency_for("month", 2), 6L)
  expect_equal(frequency_for("month", 1), 12L)

  expect_null(frequency_for("month", 5))
  expect_null(frequency_for("quarter", 3))
  expect_null(frequency_for("day", 1))
  expect_null(frequency_for("week", 1))
  expect_null(frequency_for("hour", 1))
})

test_that("ts_start locates the period within the calendar year", {
  expect_equal(bridgr:::ts_start(as.Date("2020-01-01"), 12), c(2020, 1))
  expect_equal(bridgr:::ts_start(as.Date("2020-07-01"), 12), c(2020, 7))
  expect_equal(bridgr:::ts_start(as.Date("2020-07-01"), 4), c(2020, 3))
  expect_equal(bridgr:::ts_start(as.Date("2020-10-01"), 4), c(2020, 4))
  expect_equal(bridgr:::ts_start(as.Date("2020-03-01"), 1), c(2020, 1))
})
