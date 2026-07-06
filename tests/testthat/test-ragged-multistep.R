test_that("weekly indicators support multi-step horizons despite calendar drift", {
  # A calendar quarter holds 13 weekly observations while the regular ladder
  # expects 12. Completion must fill each future quarter by period, not by a
  # fixed count of grid steps, or h > 1 fails for sub-monthly indicators.
  set.seed(42)
  target <- dplyr::tibble(
    time = seq(as.Date("2015-01-01"), as.Date("2019-10-01"), by = "quarter"),
    values = rnorm(20)
  )
  weekly <- dplyr::tibble(
    time = seq(as.Date("2015-01-03"), as.Date("2019-12-28"), by = "7 days"),
    values = rnorm(261)
  )

  fit <- mf_model(
    target = target, indic = weekly,
    indic_predict = "mean", indic_aggregators = "mean",
    target_lags = 1, h = 2
  )
  fc <- forecast(fit)
  expect_length(as.numeric(fc$mean), 2L)
  expect_true(all(is.finite(as.numeric(fc$mean))))
})

test_that("indicator observations beyond the forecast horizon are ignored", {
  set.seed(43)
  target <- dplyr::tibble(
    time = seq(as.Date("2015-01-01"), as.Date("2019-10-01"), by = "quarter"),
    values = rnorm(20)
  )
  # weekly data run one partial quarter past the h = 1 forecast period
  weekly <- dplyr::tibble(
    time = seq(as.Date("2015-01-03"), as.Date("2020-05-02"), by = "7 days"),
    values = rnorm(279)
  )

  fit <- mf_model(
    target = target, indic = weekly,
    indic_predict = "mean", indic_aggregators = "mean",
    target_lags = 1, h = 1
  )
  fc <- forecast(fit)
  expect_true(is.finite(as.numeric(fc$mean)[1]))

  # identical result when the beyond-horizon tail is removed by hand
  weekly_trunc <- dplyr::filter(weekly, time <= as.Date("2020-03-31"))
  fit_trunc <- mf_model(
    target = target, indic = weekly_trunc,
    indic_predict = "mean", indic_aggregators = "mean",
    target_lags = 1, h = 1
  )
  expect_equal(as.numeric(fc$mean), as.numeric(forecast(fit_trunc)$mean))
})
