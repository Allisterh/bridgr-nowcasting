#' Summarize a Mixed-Frequency Model
#'
#' Computes the summary quantities for a fitted [mf_model()] object and returns
#' them as a `"summary.mf_model"` object. Following the convention of
#' [summary.lm()], the summary is a data object in its own right: the report is
#' rendered by [print.summary.mf_model()] rather than by `summary()` itself, so
#' the individual quantities can be extracted programmatically.
#'
#' @param object A `"mf_model"` object returned by [mf_model()].
#' @param ... Unused.
#'
#' @return An object of class `"summary.mf_model"`, a list with components:
#'   \describe{
#'     \item{`target_name`, `target_frequency`, `h`, `nobs`}{Target series name,
#'       inferred target frequency unit, forecast horizon, and number of
#'       estimation rows.}
#'     \item{`regressor_names`}{Character vector of bridge-equation regressors.}
#'     \item{`coefficients`}{Numeric matrix with columns `"Estimate"`,
#'       `"Std. Error"`, `"t value"` and `"Pr(>|t|)"`. Standard errors come from
#'       [vcov.mf_model()], so they are the HAC, Delta-HAC or bootstrap standard
#'       errors when the model was fitted with `se = TRUE`.}
#'     \item{`coefficient_method`}{Method used for the coefficient standard
#'       errors, or `NULL` when the model was fitted without uncertainty.}
#'     \item{`r.squared`, `adj.r.squared`, `sigma`, `df.residual`}{Fit
#'       measures for the target equation.}
#'     \item{`indicators`}{Data frame of per-indicator frequency, completion
#'       method and aggregation scheme, one row per indicator.}
#'     \item{`custom_weights`}{Named list of user-supplied numeric aggregation
#'       weights, empty when none were used.}
#'     \item{`parametric_weights`, `parametric_parameters`}{Named lists of
#'       estimated parametric aggregation weights and their underlying
#'       parameters, empty when no parametric aggregator was used.}
#'     \item{`uncertainty`, `bootstrap`, `optimization`}{Uncertainty settings,
#'       bootstrap diagnostics, and joint parametric-optimization diagnostics.}
#'   }
#'
#' @seealso [coef.mf_model()], [confint.mf_model()] and [vcov.mf_model()] for
#'   extracting individual quantities directly from the fitted model.
#'
#' @srrstats {RE4.18} Provides a dedicated `summary.mf_model()` method.
#' @srrstats {RE4.11} Reports GOF metrics with the coefficient table.
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
#' model_summary <- summary(model)
#' model_summary
#'
#' # The coefficient matrix is available programmatically, as for `lm()`.
#' coef(model_summary)
#' @method summary mf_model
#' @export
summary.mf_model <- function(object, ...) {
  structure(
    list(
      target_name = object$target_name,
      target_frequency = object$target_frequency$unit[[1]],
      h = object$h,
      nobs = nrow(object$estimation_set),
      regressor_names = object$regressor_names,
      coefficients = summary_coefficient_matrix(object),
      coefficient_method = object$uncertainty$coefficient_method,
      r.squared = unname(summary(object$model)$r.squared),
      adj.r.squared = unname(summary(object$model)$adj.r.squared),
      sigma = unname(summary(object$model)$sigma),
      df.residual = stats::df.residual(object$model),
      indicators = summary_indicator_table(object),
      custom_weights = summary_custom_weights(object),
      parametric_weights = object$parametric_weights,
      parametric_parameters = object$parametric_parameters,
      uncertainty = object$uncertainty,
      bootstrap = object$bootstrap,
      optimization = object$parametric_optimization
    ),
    class = "summary.mf_model"
  )
}


#' Print a Mixed-Frequency Model Summary
#'
#' @param x A `"summary.mf_model"` object returned by [summary.mf_model()].
#' @param ... Unused.
#'
#' @return `x`, invisibly.
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
#' print(summary(model))
#' @method print summary.mf_model
#' @export
print.summary.mf_model <- function(x, ...) {
  print_model_header(x)
  print_coefficient_table(x)
  print_gof_table(x)
  print_indicator_summary(x)
  print_custom_weights_block(x)
  print_parametric_block(x)
  print_uncertainty_block(x)
  print_optimization_block(x)
  cat("-----------------------------------\n")

  invisible(x)
}


#' Build the `Estimate`/`Std. Error`/`t value`/`Pr(>|t|)` coefficient matrix
#'
#' Only called from `summary.mf_model()` above. Standard errors come from
#' `vcov.mf_model()` (`accessors.R`), so they respect the HAC, Delta-HAC or
#' bootstrap covariance stored on the model when `se = TRUE`; without
#' uncertainty this falls back to the ordinary `lm()` covariance.
#'
#' @keywords internal
#' @noRd
summary_coefficient_matrix <- function(object) {
  estimates <- stats::coef(object)
  covariance <- stats::vcov(object)

  standard_errors <- rep(NA_real_, length(estimates))
  names(standard_errors) <- names(estimates)
  shared <- intersect(names(estimates), colnames(covariance))
  if (length(shared) > 0) {
    standard_errors[shared] <- sqrt(diag(covariance)[shared])
  }

  degrees_freedom <- tryCatch(
    stats::df.residual(object$model),
    error = function(...) NA_real_
  )
  t_values <- as.numeric(estimates) / as.numeric(standard_errors)
  p_values <- if (is.finite(degrees_freedom) && degrees_freedom > 0) {
    2 * stats::pt(abs(t_values), df = degrees_freedom, lower.tail = FALSE)
  } else {
    2 * stats::pnorm(abs(t_values), lower.tail = FALSE)
  }

  matrix(
    c(
      as.numeric(estimates),
      as.numeric(standard_errors),
      t_values,
      p_values
    ),
    ncol = 4L,
    dimnames = list(
      names(estimates),
      c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    )
  )
}


#' Build the per-indicator frequency/predict/aggregation table
#'
#' Only called from `summary.mf_model()` above, to assemble the data frame
#' later rendered by `print_indicator_summary()` below.
#'
#' @keywords internal
#' @noRd
summary_indicator_table <- function(object) {
  indicator_summary <- lapply(
    seq_along(object$indic_name),
    function(index) {
      indicator_id <- object$indic_name[[index]]
      indicator_meta <- object$indicator_frequencies[
        match(indicator_id, object$indicator_frequencies$id),
        ,
        drop = FALSE
      ]
      requested <- object$indic_aggregators_requested[[index]]
      aggregation_label <- if (is.character(requested)) {
        requested
      } else {
        "custom_weights"
      }
      data.frame(
        Frequency = indicator_meta$unit[[1]],
        Predict = object$indic_predict[[index]],
        Aggregation = aggregation_label,
        check.names = FALSE,
        row.names = indicator_id
      )
    }
  )

  do.call(rbind, indicator_summary)
}


#' Collect user-supplied numeric aggregation weights, keyed by indicator
#'
#' Only called from `summary.mf_model()` above. Returns an empty named list
#' when every indicator used a character aggregator.
#'
#' @keywords internal
#' @noRd
summary_custom_weights <- function(object) {
  custom <- list()
  for (index in seq_along(object$indic_aggregators)) {
    aggregator <- object$indic_aggregators[[index]]
    if (!is.character(aggregator)) {
      custom[[object$indic_name[[index]]]] <- aggregator
    }
  }

  custom
}


#' Format a number for summary/print output (3 decimal places)
#'
#' Called from six of the `print_*` blocks below, wherever
#' `print.summary.mf_model()` needs consistent numeric formatting.
#'
#' @keywords internal
#' @noRd
format_summary_number <- function(x) {
  formatC(x, format = "f", digits = 3)
}


#' Print the model header block (target, frequency, horizon, rows)
#'
#' One of eight `print_*` render blocks called in sequence from
#' `print.summary.mf_model()` above, each responsible for one section of the
#' printed report; each is called exactly once, from there only.
#'
#' @keywords internal
#' @noRd
print_model_header <- function(x) {
  cat("Mixed-frequency model summary\n")
  cat("-----------------------------------\n")
  cat("Target series: ", x$target_name, "\n", sep = "")
  cat("Target frequency: ", x$target_frequency, "\n", sep = "")
  cat("Forecast horizon: ", x$h, "\n", sep = "")
  cat("Estimation rows: ", x$nobs, "\n", sep = "")
  cat(
    "Regressors: ",
    paste(x$regressor_names, collapse = ", "),
    "\n",
    sep = ""
  )
}


#' Print the target-equation coefficient table
#'
#' Called once from `print.summary.mf_model()` above. Calls
#' `format_summary_number()` above. Renders the `Estimate` column of
#' `x$coefficients`, plus a method-labelled standard-error column when the
#' model was fitted with `se = TRUE`.
#'
#' @keywords internal
#' @noRd
print_coefficient_table <- function(x) {
  coefficient_table <- data.frame(
    Estimate = format_summary_number(x$coefficients[, "Estimate"]),
    check.names = FALSE
  )
  rownames(coefficient_table) <- rownames(x$coefficients)

  if (!is.null(x$uncertainty$coefficient_se)) {
    coefficient_label <- switch(
      x$coefficient_method %||% "hac",
      "delta_hac" = "Delta-HAC SE",
      "block_bootstrap" = "Bootstrap SE",
      "HAC SE"
    )
    coefficient_table[[coefficient_label]] <- format_summary_number(
      x$coefficients[, "Std. Error"]
    )
  }

  cat("-----------------------------------\n")
  cat("Target equation coefficients:\n")
  print(
    noquote(format(coefficient_table, justify = "right")),
    quote = FALSE,
    right = TRUE
  )
}


#' Print the goodness-of-fit table (R-squared, adjusted R-squared, sigma)
#'
#' Called once from `print.summary.mf_model()` above. Calls
#' `format_summary_number()` above.
#'
#' @keywords internal
#' @noRd
print_gof_table <- function(x) {
  gof_table <- data.frame(
    Statistic = c(
      "R-squared",
      "Adjusted R-squared",
      "Residual standard error"
    ),
    Value = format_summary_number(c(
      x$r.squared,
      x$adj.r.squared,
      x$sigma
    )),
    check.names = FALSE
  )

  cat("-----------------------------------\n")
  cat("Model fit:\n")
  print(
    noquote(format(gof_table, justify = "left")),
    quote = FALSE,
    right = FALSE,
    row.names = FALSE
  )
}


#' Print the per-indicator frequency/predict/aggregation summary table
#'
#' Called once from `print.summary.mf_model()` above.
#'
#' @keywords internal
#' @noRd
print_indicator_summary <- function(x) {
  cat("-----------------------------------\n")
  cat("Indicator summary:\n")
  print(
    noquote(format(x$indicators, justify = "left")),
    quote = FALSE,
    right = FALSE
  )
}


#' Print user-supplied numeric aggregation weights, if any were used
#'
#' Called once from `print.summary.mf_model()` above; no-ops (returns
#' `invisible(NULL)`) when no indicator used numeric weights. Calls
#' `format_summary_number()` above.
#'
#' @keywords internal
#' @noRd
print_custom_weights_block <- function(x) {
  if (length(x$custom_weights) == 0) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Custom aggregation weights:\n")
  for (indicator_id in names(x$custom_weights)) {
    cat(
      indicator_id,
      ": ",
      paste(
        format_summary_number(x$custom_weights[[indicator_id]]),
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
  }
}


#' Print estimated parametric aggregation weights and parameters, if any
#'
#' Called once from `print.summary.mf_model()` above; no-ops when no indicator
#' used parametric (`"expalmon"`/`"beta"`) aggregation. Calls
#' `format_summary_number()` above.
#'
#' @keywords internal
#' @noRd
print_parametric_block <- function(x) {
  parametric_ids <- names(x$parametric_parameters)
  if (length(parametric_ids) == 0) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Estimated parametric aggregation:\n")
  for (indicator_id in parametric_ids) {
    cat(
      indicator_id,
      " weights: ",
      paste(
        format_summary_number(x$parametric_weights[[indicator_id]]),
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
    if (!is.null(x$parametric_parameters[[indicator_id]])) {
      cat(
        indicator_id,
        " parameters: ",
        paste(
          format_summary_number(x$parametric_parameters[[indicator_id]]),
          collapse = ", "
        ),
        "\n",
        sep = ""
      )
    }
  }
}


#' Print the uncertainty method and diagnostics block, if `se = TRUE`
#'
#' Called once from `print.summary.mf_model()` above; no-ops when neither
#' coefficient nor prediction uncertainty was requested.
#'
#' @keywords internal
#' @noRd
print_uncertainty_block <- function(x) {
  uncertainty <- x$uncertainty
  coefficient_uncertainty_enabled <- !is.null(uncertainty$coefficient_se)
  prediction_uncertainty_enabled <- !is.null(uncertainty$prediction_method)
  if (!coefficient_uncertainty_enabled && !prediction_uncertainty_enabled) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Uncertainty:\n")
  if (coefficient_uncertainty_enabled) {
    cat(
      "Coefficient SEs: ",
      uncertainty$coefficient_method,
      "\n",
      sep = ""
    )
  }
  if (identical(uncertainty$prediction_method, "block_bootstrap")) {
    cat("Prediction intervals: full-system block bootstrap\n")
    cat(
      "Bootstrap draws: ",
      x$bootstrap$valid_N,
      " / ",
      x$bootstrap$N,
      "\n",
      sep = ""
    )
    cat("Block length: ", x$bootstrap$block_length, "\n", sep = "")
  } else if (
    identical(uncertainty$prediction_method, "residual_resampling")
  ) {
    cat("Prediction intervals: residual resampling\n")
    cat(
      "Simulation paths: ",
      uncertainty$simulation_paths,
      "\n",
      sep = ""
    )
  } else if (isTRUE(x$bootstrap$requested)) {
    cat("Prediction intervals: unavailable\n")
  }
}


#' Print the joint parametric-optimization diagnostics block, if applicable
#'
#' Called once from `print.summary.mf_model()` above; no-ops when
#' `x$optimization` is `NULL` (no parametric indicators). Calls
#' `format_summary_number()` above.
#'
#' @keywords internal
#' @noRd
print_optimization_block <- function(x) {
  if (is.null(x$optimization)) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Joint parametric aggregation optimization:\n")
  cat("Method: ", x$optimization$method, "\n", sep = "")
  cat(
    "Objective value: ",
    format_summary_number(x$optimization$value),
    "\n",
    sep = ""
  )
  cat(
    "Convergence code: ",
    x$optimization$convergence,
    "\n",
    sep = ""
  )
  cat(
    "Best start: ",
    x$optimization$best_start,
    " / ",
    x$optimization$n_starts,
    "\n",
    sep = ""
  )
}
