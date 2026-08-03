#' Accessor Methods for Mixed-Frequency Models
#'
#' Access standard model summaries from a fitted [mf_model()] object.
#'
#' @param object,x,formula A fitted `"mf_model"` object returned by
#'   [mf_model()]. The `formula` spelling is required by the
#'   [stats::model.frame()] generic and carries the same meaning.
#' @param ... Unused.
#'
#' @return The requested model summary, usually delegated from the stored target
#'   regression fit.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   h = 1
#' )
#'
#' coef(model)
#' @name mf_model-accessors
NULL

#' @rdname mf_model-accessors
#' @srrstats {RE4.2} Returns the fitted target-regression coefficients.
#' @method coef mf_model
#' @export
coef.mf_model <- function(object, ...) {
  stats::coef(object$model, ...)
}

#' @rdname mf_model-accessors
#' @param parm,level Passed to [confint()]. Confidence intervals are computed
#'   from the coefficient covariance matrix returned by [stats::vcov()], which
#'   may be the HAC or Delta-HAC covariance when `se = TRUE`. Critical values
#'   use a t-distribution with residual degrees of freedom from the fitted
#'   target equation; this is conservative relative to asymptotic normal
#'   critical values but is common practice in applied econometrics.
#' @srrstats {RE4.3} Returns coefficient intervals from the stored covariance.
#' @method confint mf_model
#' @export
confint.mf_model <- function(object, parm = NULL, level = 0.95, ...) {
  coefficients <- stats::coef(object)
  covariance <- stats::vcov(object)

  if (is.null(parm)) {
    parm_names <- names(coefficients)
  } else if (is.numeric(parm)) {
    parm_names <- names(coefficients)[parm]
    if (anyNA(parm_names)) {
      rlang::abort(
        paste0(
          "`parm` indices out of range: ",
          paste(parm[is.na(parm_names)], collapse = ", "),
          "."
        )
      )
    }
  } else {
    parm_names <- parm
    invalid <- setdiff(parm_names, names(coefficients))
    if (length(invalid) > 0) {
      rlang::abort(
        paste0(
          "`parm` names not found in model coefficients: ",
          paste(invalid, collapse = ", "),
          "."
        )
      )
    }
  }

  standard_errors <- sqrt(diag(covariance))[parm_names]
  degrees_freedom <- tryCatch(
    stats::df.residual(object$model),
    error = function(...) NA_real_
  )
  critical_value <- if (is.finite(degrees_freedom) && degrees_freedom > 0) {
    stats::qt((1 + level) / 2, df = degrees_freedom)
  } else {
    stats::qnorm((1 + level) / 2)
  }

  intervals <- cbind(
    coefficients[parm_names] - critical_value * standard_errors,
    coefficients[parm_names] + critical_value * standard_errors
  )
  colnames(intervals) <- paste0(
    format(100 * c((1 - level) / 2, 1 - (1 - level) / 2), trim = TRUE),
    " %"
  )

  intervals
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.4} Returns the stored target-regression formula.
#' @method formula mf_model
#' @export
formula.mf_model <- function(x, ...) {
  x$formula
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.5} Returns the number of fitted target-regression rows.
#' @method nobs mf_model
#' @export
nobs.mf_model <- function(object, ...) {
  stats::nobs(object$model, ...)
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.6} Returns stored covariance or falls back to the target fit.
#' @method vcov mf_model
#' @export
vcov.mf_model <- function(object, ...) {
  covariance <- object$uncertainty$coefficient_covariance

  if (!is.null(covariance)) {
    return(covariance)
  }

  stats::vcov(object$model, ...)
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.9} Returns in-sample fitted target values.
#' @method fitted mf_model
#' @export
fitted.mf_model <- function(object, ...) {
  stats::fitted(object$model, ...)
}

#' @rdname mf_model-accessors
#' @details `residuals.mf_model()` returns target-equation residuals on the same
#'   standardized scale as the fitted target series, so they can be passed
#'   directly to downstream residual diagnostics.
#' @srrstats {RE4.10} Returns target residuals on the fitted-model scale.
#' @method residuals mf_model
#' @export
residuals.mf_model <- function(object, ...) {
  stats::residuals(object$model, ...)
}

#' @rdname mf_model-accessors
#' @details `model.frame.mf_model()` returns the aligned modelling data. Use
#'   `which = "estimation"` for the in-sample bridge-equation data, and
#'   `which = "forecast"` for the future target-period regressor path the
#'   forecast is produced from. The latter is the frame to modify and pass back
#'   as `xreg` when constructing scenarios.
#' @param which For `model.frame()`, which modelling frame to return,
#'   `"estimation"` (default) or `"forecast"`. For `variable.names()`, which
#'   group of regressor names to return, `"all"` (default), `"xreg"` or
#'   `"target_lags"`.
#' @srrstats {RE4.8} Returns the aligned estimation and forecast data frames.
#' @method model.frame mf_model
#' @export
model.frame.mf_model <- function(
  formula,
  which = c("estimation", "forecast"),
  ...
) {
  which <- match.arg(which)

  if (identical(which, "estimation")) {
    return(formula$estimation_set)
  }

  formula$forecast_base_set
}

#' @rdname mf_model-accessors
#' @details `variable.names.mf_model()` returns the names of the
#'   bridge-equation regressors. `which = "xreg"` returns the non-target-lag
#'   regressors, which are exactly the series a custom `xreg` must supply when
#'   forecasting a scenario, and pairs with
#'   `model.frame(object, which = "forecast")`.
#' @srrstats {RE4.8} Names the regressors of the fitted target equation.
#' @method variable.names mf_model
#' @export
variable.names.mf_model <- function(
  object,
  which = c("all", "xreg", "target_lags"),
  ...
) {
  which <- match.arg(which)

  switch(
    which,
    all = object$regressor_names,
    xreg = object$xreg_names,
    target_lags = object$target_lag_names
  )
}

#' Resolve an indicator selector to a single indicator id
#'
#' Only called from `weights.mf_model()` and `aggregation_parameters.mf_model()`
#' below, to turn a user-supplied name or position into a validated indicator
#' id so both accessors report the same errors.
#'
#' @keywords internal
#' @noRd
resolve_indicator_id <- function(object, indicator) {
  indicator_ids <- object$indic_name

  if (length(indicator) != 1) {
    rlang::abort("`indicator` must select exactly one indicator.")
  }

  if (is.numeric(indicator)) {
    if (indicator < 1 || indicator > length(indicator_ids)) {
      rlang::abort(
        paste0(
          "`indicator` index out of range: ",
          indicator,
          ". The model has ",
          length(indicator_ids),
          " indicator(s)."
        )
      )
    }
    return(indicator_ids[[as.integer(indicator)]])
  }

  if (!indicator %in% indicator_ids) {
    rlang::abort(
      paste0(
        "`indicator` not found: ",
        indicator,
        ". Available indicators: ",
        paste(indicator_ids, collapse = ", "),
        "."
      )
    )
  }

  indicator
}

#' Number of indicator observations inside one target period
#'
#' Only called from `weights.mf_model()` below, to size the deterministic
#' weight vectors. Wraps `observations_per_target_period()`
#' (`utils-frequency.R`) with the metadata already stored on the fitted
#' object, and returns `NULL` rather than failing when the ratio cannot be
#' resolved, so the accessor degrades to `NULL` instead of erroring.
#'
#' @keywords internal
#' @noRd
indicator_block_size <- function(object, indicator_id) {
  indicator_meta <- object$indicator_frequencies[
    match(indicator_id, object$indicator_frequencies$id),
    ,
    drop = FALSE
  ]

  block_size <- tryCatch(
    observations_per_target_period(
      indicator_meta = indicator_meta,
      target_meta = object$target_frequency,
      frequency_conversions = object$frequency_conversions
    ),
    error = function(...) NULL
  )

  if (is.null(block_size) || !is.finite(block_size) || block_size < 1) {
    return(NULL)
  }

  as.integer(round(block_size))
}


#' Fixed within-period weights implied by a deterministic aggregator
#'
#' Only called from `weights.mf_model()` below. Returns `NULL` for aggregators
#' that imply no single weight vector (`"unrestricted"`, and the parametric
#' rules before their weights have been estimated).
#'
#' @keywords internal
#' @noRd
deterministic_aggregation_weights <- function(aggregator, n_obs) {
  if (is.null(n_obs)) {
    return(NULL)
  }

  switch(
    aggregator,
    "mean" = rep(1 / n_obs, n_obs),
    "sum" = rep(1, n_obs),
    "last" = c(rep(0, n_obs - 1L), 1),
    NULL
  )
}


#' Indicator Names of a Mixed-Frequency Model
#'
#' Returns the identifiers of the high-frequency indicators used by a fitted
#' [mf_model()] object, in the order they enter the bridge equation.
#'
#' @param object A fitted `"mf_model"` object returned by [mf_model()].
#' @param ... Unused.
#'
#' @return A character vector of indicator identifiers.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   h = 1
#' )
#'
#' indicators(model)
#' @export
indicators <- function(object, ...) {
  UseMethod("indicators")
}

#' @rdname indicators
#' @method indicators mf_model
#' @export
indicators.mf_model <- function(object, ...) {
  object$indic_name
}

#' Aggregation Weights of a Mixed-Frequency Model
#'
#' Extracts the within-period aggregation weights applied to each indicator.
#' For parametric aggregators (`"expalmon"`, `"beta"`) these are the weights
#' implied by the estimated parameters; for user-supplied numeric aggregators
#' they are the supplied weights.
#'
#' @param object A fitted `"mf_model"` object returned by [mf_model()].
#' @param indicator Optional indicator selector, either an indicator name or a
#'   position. When supplied, the weights for that single indicator are
#'   returned directly; when `NULL` (default), a named list covering every
#'   indicator is returned.
#' @param ... Unused.
#'
#' @details The deterministic rules imply the fixed weight vectors of
#'   \eqn{\tilde x_t = \sum_m w_m x_{t,m}}: `"mean"` gives \eqn{w_m = 1/M},
#'   `"last"` gives \eqn{w_M = 1} and zero elsewhere, and `"sum"` gives
#'   \eqn{w_m = 1}. These are returned alongside the estimated and
#'   user-supplied weights, so the accessor reports the weights actually
#'   applied whichever aggregator was chosen. `"unrestricted"` has no single
#'   weight vector, because it estimates one coefficient per within-period
#'   observation instead of aggregating; it returns `NULL`, as does an
#'   indicator aligned with `indic_predict = "direct"`.
#'
#' @return When `indicator` is `NULL`, a named list with one element per
#'   indicator, each either a numeric weight vector or `NULL` for indicators
#'   whose aggregator does not imply a fixed weight vector (`"unrestricted"`).
#'   When `indicator` is supplied, that single element.
#'
#' @seealso [aggregation_parameters()] for the underlying parametric
#'   parameters, and [indicators()] for the available indicator names.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_aggregators = "expalmon",
#'   h = 1
#' )
#'
#' weights(model)
#' weights(model, indicator = 1)
#' @method weights mf_model
#' @export
weights.mf_model <- function(object, indicator = NULL, ...) {
  indicator_ids <- object$indic_name

  all_weights <- lapply(
    seq_along(indicator_ids),
    function(index) {
      indicator_id <- indicator_ids[[index]]
      parametric <- object$parametric_weights[[indicator_id]]
      if (!is.null(parametric)) {
        return(as.numeric(parametric))
      }

      aggregator <- object$indic_aggregators[[index]]
      if (is.null(aggregator) || length(aggregator) == 0) {
        return(NULL)
      }
      if (!is.character(aggregator)) {
        return(as.numeric(aggregator))
      }

      deterministic_aggregation_weights(
        aggregator = aggregator,
        n_obs = indicator_block_size(object, indicator_id)
      )
    }
  )
  names(all_weights) <- indicator_ids

  if (is.null(indicator)) {
    return(all_weights)
  }

  all_weights[[resolve_indicator_id(object, indicator)]]
}

#' Estimated Parametric Aggregation Parameters
#'
#' Extracts the estimated parameters of parametric aggregation schemes
#' (`"expalmon"`, `"beta"`) from a fitted [mf_model()] object. These are the
#' parameters whose implied weights are returned by [weights.mf_model()].
#'
#' @param object A fitted `"mf_model"` object returned by [mf_model()].
#' @param indicator Optional indicator selector, either an indicator name or a
#'   position. When supplied, the parameters for that single indicator are
#'   returned directly; when `NULL` (default), a named list is returned.
#' @param ... Unused.
#'
#' @return When `indicator` is `NULL`, a named list with one element per
#'   indicator that used a parametric aggregator, each a numeric parameter
#'   vector. When `indicator` is supplied, that single element, or `NULL` when
#'   the indicator did not use a parametric aggregator.
#'
#' @seealso [weights.mf_model()] for the implied aggregation weights.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_aggregators = "expalmon",
#'   h = 1
#' )
#'
#' aggregation_parameters(model)
#' @export
aggregation_parameters <- function(object, indicator = NULL, ...) {
  UseMethod("aggregation_parameters")
}

#' @rdname aggregation_parameters
#' @method aggregation_parameters mf_model
#' @export
aggregation_parameters.mf_model <- function(object, indicator = NULL, ...) {
  if (is.null(indicator)) {
    return(object$parametric_parameters)
  }

  object$parametric_parameters[[resolve_indicator_id(object, indicator)]]
}

#' @rdname mf_model-accessors
#' @return `x`, invisibly.
#' @srrstats {RE4.17} Prints a concise model header; use `summary()` for the
#'   detailed report.
#' @method print mf_model
#' @export
print.mf_model <- function(x, ...) {
  cat("Mixed-frequency model\n")
  cat("Target series:    ", x$target_name, "\n", sep = "")
  cat("Target frequency: ", x$target_frequency$unit[[1]], "\n", sep = "")
  cat("Forecast horizon: ", x$h, "\n", sep = "")
  cat("Estimation rows:  ", nrow(x$estimation_set), "\n", sep = "")
  cat("Formula:          ", deparse1(x$formula), "\n", sep = "")
  cat("\nCoefficients:\n")
  print(stats::coef(x))
  cat("\nUse `summary()` for the full model report.\n")
  invisible(x)
}
