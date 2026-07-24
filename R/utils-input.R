
#' Validate a single non-negative (or positive) integer-like scalar
#'
#' A shared low-level validator. Called directly from `validate_mf_inputs()`
#' in `mf_model.R` for `indic_lags`/`target_lags`/`h`, and from three places
#' within this file: `normalize_parametric_solver_options()` (`maxiter`,
#' `n_starts`, `trace`) and `normalize_mf_bootstrap()` (`N`) below.
#'
#' @keywords internal
#' @noRd
check_scalar_count <- function(
  x,
  arg,
  min = 0L,
  integer = TRUE,
  call = rlang::caller_env()
) {
  ok <- is.numeric(x) &&
    length(x) == 1 &&
    is.finite(x) &&
    x >= min &&
    (!integer || x == as.integer(x))

  if (!ok) {
    rlang::abort(
      paste0("`", arg, "` must be a single integer >= ", min, "."),
      call = call
    )
  }

  invisible(x)
}


#' Validate a single `TRUE`/`FALSE` scalar
#'
#' Called from `validate_mf_inputs()` in `mf_model.R` for `se` and
#' `full_system_bootstrap`, and from `normalize_parametric_solver_options()`
#' below for `solver_options$warn`.
#'
#' @keywords internal
#' @noRd
check_scalar_flag <- function(x, arg, call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    rlang::abort(
      paste0("`", arg, "` must be either `TRUE` or `FALSE`."),
      call = call
    )
  }
  invisible(x)
}


#' Validate a single finite number, optionally bounded below
#'
#' Only called from `normalize_parametric_solver_options()` below, to check
#' `solver_options$reltol`.
#'
#' @keywords internal
#' @noRd
check_scalar_number <- function(
  x,
  arg,
  min = NULL,
  strict = FALSE,
  call = rlang::caller_env()
) {
  ok <- is.numeric(x) && length(x) == 1 && is.finite(x) &&
    (is.null(min) || (if (strict) x > min else x >= min))

  if (!ok) {
    descriptor <- if (is.null(min)) {
      "a single finite number"
    } else if (strict) {
      paste0("a single finite number > ", min)
    } else {
      paste0("a single finite number >= ", min)
    }
    rlang::abort(
      paste0("`", arg, "` must be ", descriptor, "."),
      call = call
    )
  }

  invisible(x)
}


#' Collapse a default-id candidate into one non-empty string
#'
#' Called from `mf_argument_label()` below (twice) and from `as_mf_tbl()`
#' below, to turn whatever label was derived for an unnamed `target`/`indic`
#' argument (or the fallback `"series"`) into a single clean id string.
#'
#' @keywords internal
#' @noRd
normalize_mf_default_id <- function(default_id) {
  default_id <- as.character(default_id)
  default_id <- default_id[nzchar(default_id)]

  if (length(default_id) == 0) {
    return("series")
  }

  paste(default_id, collapse = " ")
}


#' Validate and normalize the `missing` argument to one of the allowed values
#'
#' Called once from `mf_model()` (`mf_model.R`) up front, and once more from
#' `apply_missing_value_policy()` below (which is called separately from
#' `as_mf_tbl()` for both `target` and `indic`, so it re-validates rather
#' than trusting the value it is passed).
#'
#' @keywords internal
#' @noRd
normalize_missing_action <- function(
  missing = "error",
  call = rlang::caller_env()
) {
  rlang::arg_match0(
    arg = missing,
    values = c("error", "drop", "impute"),
    error_call = call
  )
}


#' Validate and normalize the `stationarity` argument
#'
#' Called once from `mf_model()` (`mf_model.R`) up front, and once more from
#' `warn_stationarity_diagnostics()` below, for the same reason as
#' `normalize_missing_action()` above: the latter is called separately for
#' `target` and `indic` and re-validates independently.
#'
#' @keywords internal
#' @noRd
normalize_stationarity_action <- function(
  stationarity = "none",
  call = rlang::caller_env()
) {
  rlang::arg_match0(
    arg = stationarity,
    values = c("none", "warn"),
    error_call = call
  )
}


#' Linearly interpolate missing values within one series, with end carry
#'
#' Only called from `apply_missing_value_policy()` below, once per series
#' group, on the `missing = "impute"` path.
#'
#' @keywords internal
#' @noRd
interpolate_series_values <- function(values) {
  observed <- !is.na(values)
  if (sum(observed) == 0) {
    return(values)
  }
  if (sum(observed) == 1) {
    return(rep(values[observed][[1]], length(values)))
  }

  stats::approx(
    x = which(observed),
    y = values[observed],
    xout = seq_along(values),
    method = "linear",
    rule = 2
  )$y
}


#' Apply the `missing = "error"/"drop"/"impute"` policy to standardized data
#'
#' Only called from `as_mf_tbl()` below, as its last step, once per
#' `target`/`indic` conversion. Calls `normalize_missing_action()` above and
#' `interpolate_series_values()` above for the impute path.
#'
#' @srrstats {G2.14b} `missing = "drop"` removes explicit missing values.
#' @srrstats {G2.14c} `missing = "impute"` replaces missing values.
#' @srrstats {G2.15} Resolves missing values before later summary statistics.
#' @srrstats {TS2.1b} Drop mode warns and proceeds on the non-missing subset.
#' @srrstats {TS2.1c} Impute mode uses linear interpolation with end carry.
#' @keywords internal
#' @noRd
apply_missing_value_policy <- function(
  data,
  arg,
  missing = "error",
  call = rlang::caller_env()
) {
  missing <- normalize_missing_action(missing, call = call)

  if (anyNA(data$time)) {
    rlang::abort(
      paste0("`", arg, "` contains missing timestamps."),
      call = call
    )
  }

  missing_n <- sum(is.na(data$values))
  if (missing_n == 0) {
    return(data)
  }

  if (identical(missing, "error")) {
    rlang::abort(paste0("`", arg, "` contains missing values."), call = call)
  }

  if (identical(missing, "drop")) {
    rlang::warn(
      paste0(
        "`", arg, "` contains ", missing_n, " missing value",
        if (missing_n == 1) "" else "s",
        "; dropping them before analysis."
      ),
      call = call
    )
    return(dplyr::filter(data, !is.na(.data$values)))
  }

  all_missing_ids <- data |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(
      all_missing = all(is.na(.data$values)),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$all_missing) |>
    dplyr::pull(.data$id)
  if (length(all_missing_ids) > 0) {
    rlang::abort(
      paste0(
        "`", arg,
        "` cannot be imputed because the following series contain only ",
        "missing values: ",
        paste0("`", all_missing_ids, "`", collapse = ", "),
        "."
      ),
      call = call
    )
  }

  rlang::warn(
    paste0(
      "`", arg, "` contains ", missing_n, " missing value",
      if (missing_n == 1) "" else "s",
      "; imputing them by linear interpolation within each series."
    ),
    call = call
  )

  data |>
    dplyr::group_by(.data$id) |>
    dplyr::group_modify(
      ~ dplyr::mutate(.x, values = interpolate_series_values(.data$values))
    ) |>
    dplyr::ungroup()
}


#' Is this an unevaluated argument expression a bare variable reference?
#'
#' Only called from `mf_argument_label()` immediately below, to decide
#' whether `substitute(target)`/`substitute(indic)` inside `mf_model()`
#' (`mf_model.R`) looks like a nameable object (`gdp`, `df$gdp`, `x[["gdp"]]`)
#' worth deparsing into a default series id, versus an arbitrary expression
#' that should fall back to `"target"`/`"indic"` instead.
#'
#' @keywords internal
#' @noRd
is_mf_reference_expr <- function(expr) {
  if (is.symbol(expr)) {
    return(TRUE)
  }

  if (!is.call(expr)) {
    return(FALSE)
  }

  head <- expr[[1]]
  if (!is.symbol(head)) {
    return(FALSE)
  }

  as.character(head) %in% c("$", "[[", "@", "::", ":::")
}


#' Derive a default series id from an unevaluated call argument
#'
#' Only called from `mf_model()` in `mf_model.R`, on
#' `substitute(target)`/`substitute(indic)`, to label a single unnamed series
#' after the variable/expression the user passed in (e.g. `gdp` becomes id
#' `"gdp"`) rather than a generic placeholder. Calls
#' `is_mf_reference_expr()` above and `normalize_mf_default_id()` above.
#'
#' @keywords internal
#' @noRd
mf_argument_label <- function(expr, fallback) {
  if (is_mf_reference_expr(expr)) {
    return(normalize_mf_default_id(rlang::as_label(expr)))
  }

  normalize_mf_default_id(fallback)
}


#' Standardize any ts-boxable `target`/`indic`/`xreg` input to one long tibble
#'
#' The main input-standardization entry point: called from `mf_model()`
#' (`mf_model.R`) for both `target` and `indic`, and from `as_forecast_xreg()`
#' below for `xreg`. Everything downstream (frequency inference, alignment,
#' aggregation) assumes the `id`/`time`/`values` tibble shape this function
#' produces. Calls `standardize_ts_tbl()` below and
#' `apply_missing_value_policy()` above.
#'
#' @srrstats {G2.4} Converts supported inputs to one internal form.
#' @srrstats {G2.4b} Standardizes series values with `as.numeric()`.
#' @srrstats {G2.4c} Standardizes series ids with `as.character()`.
#' @srrstats {G2.10} Selects columns after standard tibble conversion.
#' @srrstats {G2.6} Standardizes supported series through tsbox.
#' @srrstats {G2.7} Accepts multiple ts-boxable input forms.
#' @srrstats {G2.8} Uses one shared preprocessing step for supported inputs.
#' @srrstats {G2.12} Rejects list-valued data columns before modeling.
#' @srrstats {TS1.0} Only ts-boxable time-series inputs enter preprocessing.
#' @srrstats {TS1.2} Validates ts-boxable input status in `as_mf_tbl()`.
#' @srrstats {TS1.3} Converts accepted inputs to one table format.
#' @srrstats {TS1.6} Rejects per-series time disorder in tabular inputs.
#' @srrstats {TS1.5} Sorts rows by series id and time.
#' @keywords internal
#' @noRd
as_mf_tbl <- function(
  x,
  arg,
  default_id,
  missing = "error",
  call = rlang::caller_env()
) {
  default_id <- normalize_mf_default_id(default_id)

  if (!tsbox::ts_boxable(x)) {
    rlang::abort(
      paste0("`", arg, "` must be a ts-boxable object."),
      call = call
    )
  }

  # Reject list-valued value columns and check per-series time ordering on
  # the raw data frame, because `tsbox::ts_tbl()` silently sorts rows and
  # would mask any reordering violations.
  if (inherits(x, "data.frame")) {
    raw_values <- x[["values"]] %||% x[["value"]]
    if (is.list(raw_values)) {
      rlang::abort(
        paste0(
          "`", arg,
          "` must contain a numeric `value`/`values` column; list columns are ",
          "not supported."
        ),
        call = call
      )
    }
    if (!is.null(raw_values) && !is.numeric(raw_values)) {
      rlang::abort(
        paste0("`", arg, "` must contain a numeric `value`/`values` column."),
        call = call
      )
    }

    if ("time" %in% names(x) && !anyNA(x[["time"]])) {
      raw_id <- if ("id" %in% names(x)) {
        as.character(x[["id"]])
      } else {
        rep(default_id, length(x[["time"]]))
      }
      series_rows <- split(seq_along(x[["time"]]), raw_id)
      if (any(vapply(
        series_rows,
        function(index) is.unsorted(x[["time"]][index]),
        logical(1)
      ))) {
        rlang::abort(
          paste0("`", arg, "` must be ordered by time within each series."),
          call = call
        )
      }
    }
  }

  out <- suppressMessages(tsbox::ts_tbl(x)) |>
    standardize_ts_tbl()
  if (nrow(out) == 0) {
    rlang::abort(
      paste0("`", arg, "` must contain at least one observation."),
      call = call
    )
  }

  if (!"time" %in% names(out) || !"values" %in% names(out)) {
    rlang::abort(
      paste0("`", arg, "` must contain `time` and `value`/`values` columns."),
      call = call
    )
  }
  if (!is.numeric(out$values)) {
    rlang::abort(
      paste0("`", arg, "` must contain a numeric `value`/`values` column."),
      call = call
    )
  }
  if (!"id" %in% names(out)) {
    out$id <- rep(default_id, nrow(out))
  }

  out |>
    dplyr::transmute(
      id = as.character(.data$id),
      time = .data$time,
      values = as.numeric(.data$values)
    ) |>
    dplyr::arrange(.data$id, .data$time) |>
    apply_missing_value_policy(
      arg = arg,
      missing = missing,
      call = call
    )
}


#' Convert any ts-boxable object to a `time`/`values` tibble
#'
#' Only called from `as_mf_tbl()` above, right after the ts-boxable and
#' list-column checks, to hand off to `tsbox::ts_tbl()` for the actual format
#' conversion and standardize the value column's name.
#'
#' @keywords internal
#' @noRd
standardize_ts_tbl <- function(ts_data) {
  suppressMessages(tsbox::ts_tbl(ts_data)) |>
    dplyr::rename(values = dplyr::any_of(c("value", "values")))
}


#' Floor timestamps to the start of the given calendar unit
#'
#' Only called from `fallback_period_start_times()` immediately below, once
#' per candidate unit (`"year"`, `"quarter"`, `"month"`), while probing for a
#' calendar-aligned reading of otherwise irregular timestamps.
#'
#' @keywords internal
#' @noRd
as_period_start <- function(times, unit) {
  converted <- lubridate::floor_date(times, unit = unit)

  if (inherits(times, "Date")) {
    return(as.Date(converted))
  }

  as.POSIXct(converted, tz = lubridate::tz(times[[1]]) %||% "UTC")
}


#' Snap mid-period timestamps to period-start form when that helps detection
#'
#' Only called from `normalize_period_start_data()` immediately below, once
#' per series group in `mf_model()` (`mf_model.R`), right after `as_mf_tbl()`
#' standardizes `target`/`indic`. Calls `detect_month_frequency()` /
#' `detect_time_frequency()` (`utils-frequency.R`) to check whether the raw
#' timestamps are already regular, and `as_period_start()` above to try
#' period-start candidates otherwise (e.g. mid-month dates for an otherwise
#' monthly series).
#'
#' @keywords internal
#' @noRd
fallback_period_start_times <- function(times) {
  if (!(inherits(times, "Date") || inherits(times, "POSIXt"))) {
    return(times)
  }

  raw_times <- sort(unique(times))
  if (!is.null(detect_month_frequency(raw_times)) ||
    !is.null(detect_time_frequency(raw_times))) {
    return(times)
  }

  for (unit in c("year", "quarter", "month")) {
    candidate_times <- as_period_start(times, unit = unit)
    if (length(unique(candidate_times)) != length(unique(times))) {
      next
    }
    candidate_frequency <- detect_month_frequency(sort(unique(candidate_times)))

    if (
      !is.null(candidate_frequency) &&
        identical(candidate_frequency$unit, unit)
    ) {
      return(candidate_times)
    }
  }

  times
}


#' Apply `fallback_period_start_times()` to every series in a tibble
#'
#' Only called from `mf_model()` in `mf_model.R`, once for `target` and once
#' for `indic`, right after `as_mf_tbl()` standardizes each input.
#'
#' @keywords internal
#' @noRd
normalize_period_start_data <- function(data) {
  data |>
    dplyr::group_by(.data$id) |>
    dplyr::group_modify(
      ~ dplyr::mutate(.x, time = fallback_period_start_times(.data$time))
    ) |>
    dplyr::ungroup()
}


#' Reject missing values, duplicate timestamps, and too-short series
#'
#' Only called from `validate_mf_inputs()` in `mf_model.R`, once for
#' `target_tbl` and once for `indic_tbl`, after `as_mf_tbl()` has already
#' applied the `missing` policy — so any missing values still present here
#' are internal-logic bugs, not user data issues (hence the terser error
#' messages than `as_mf_tbl()`'s user-facing checks).
#'
#' @srrstats {G2.13} Rejects missing timestamps and values early.
#' @srrstats {G2.14a} Errors on explicit missing values during validation.
#' @srrstats {TS2.1a} Explicit missing timestamps or values error early.
#' @keywords internal
#' @noRd
check_mf_series <- function(
  data,
  arg,
  call = rlang::caller_env()
) {
  if (anyNA(data$time)) {
    rlang::abort(
      paste0("`", arg, "` contains missing timestamps."),
      call = call
    )
  }
  if (anyNA(data$values)) {
    rlang::abort(paste0("`", arg, "` contains missing values."), call = call)
  }

  duplicate_rows <- data |>
    dplyr::count(.data$id, .data$time) |>
    dplyr::filter(.data$n > 1)

  if (nrow(duplicate_rows) > 0) {
    rlang::abort(
      paste0(
        "`", arg,
        "` contains duplicate timestamps within at least one series."
      ),
      call = call
    )
  }

  series_sizes <- data |>
    dplyr::count(.data$id)
  if (any(series_sizes$n < 2)) {
    rlang::abort(
      paste0(
        "Each series in `", arg, "` must contain at least two observations."
      ),
      call = call
    )
  }

  invisible(data)
}


#' Heuristic stationarity flags (KPSS differencing signal, variance shift)
#'
#' Only called from `warn_stationarity_diagnostics()` below, once per series
#' group, when `stationarity = "warn"`.
#'
#' @keywords internal
#' @noRd
stationarity_series_issues <- function(values) {
  issues <- character()
  n_obs <- length(values)

  if (n_obs >= 6) {
    ndiffs_required <- tryCatch(
      forecast::ndiffs(values, alpha = 0.05, test = "kpss"),
      error = function(...) NA_integer_
    )
    if (isTRUE(is.finite(ndiffs_required) && ndiffs_required > 0)) {
      issues <- c(issues, "KPSS differencing signal")
    }
  }

  split_index <- floor(n_obs / 2)
  if (split_index >= 3 && (n_obs - split_index) >= 3) {
    first_sd <- stats::sd(values[seq_len(split_index)])
    second_sd <- stats::sd(values[(split_index + 1):n_obs])
    if (all(is.finite(c(first_sd, second_sd)))) {
      min_sd <- min(first_sd, second_sd)
      max_sd <- max(first_sd, second_sd)
      variance_ratio <- if (min_sd == 0) {
        Inf
      } else {
        max_sd / min_sd
      }
      if (variance_ratio >= 2.5) {
        issues <- c(issues, "variance shift")
      }
    }
  }

  unique(issues)
}


#' Warn about heuristic stationarity issues, if `stationarity = "warn"`
#'
#' Only called from `mf_model()` in `mf_model.R`, once for `target` and once
#' for `indic`, right after input validation. Calls
#' `normalize_stationarity_action()` above and, on the `"warn"` path,
#' `stationarity_series_issues()` above per series.
#'
#' @srrstats {TS2.4} The package provides one optional preprocessing routine
#' that checks lower-order stationarity heuristics relevant for bridge-style
#' forecasting before fitting.
#' @srrstats {TS2.4a} When `stationarity = "warn"`, those heuristics emit a
#' diagnostic warning naming the affected series before model fitting.
#' @keywords internal
#' @noRd
warn_stationarity_diagnostics <- function(
  data,
  arg,
  stationarity = "none",
  call = rlang::caller_env()
) {
  stationarity <- normalize_stationarity_action(
    stationarity = stationarity,
    call = call
  )
  if (!identical(stationarity, "warn")) {
    return(invisible(NULL))
  }

  flagged <- data |>
    dplyr::group_by(.data$id) |>
    dplyr::group_modify(
      ~ {
        issues <- stationarity_series_issues(.x$values)
        if (length(issues) == 0) {
          return(dplyr::tibble(issues = character()))
        }
        dplyr::tibble(issues = paste(issues, collapse = ", "))
      }
    ) |>
    dplyr::ungroup()

  if (nrow(flagged) == 0) {
    return(invisible(NULL))
  }

  issue_text <- paste0("`", flagged$id, "` (", flagged$issues, ")")
  rlang::warn(
    paste0(
      "Heuristic stationarity checks flagged ", arg, " series ",
      paste(issue_text, collapse = "; "),
      ". Consider differences, growth rates, log changes, demeaning, or ",
      "other variance-stabilizing transformations before fitting."
    ),
    call = call
  )

  invisible(flagged)
}


#' Recycle and validate a per-indicator control against an allowed set
#'
#' Only called from `validate_mf_inputs()` in `mf_model.R`, for
#' `indic_predict`. A generic recycle-and-validate routine, distinct from
#' `normalize_indicator_aggregators()` below because aggregators also accept
#' numeric weight vectors and lists, which this simpler character-only
#' version does not need to handle.
#'
#' @srrstats {G2.3} Normalizes character controls case-insensitively.
#' @srrstats {G2.3a} Restricts forecasting methods to an allowed set.
#' @srrstats {G2.3b} Uses `tolower()` for case-insensitive matching.
#' @keywords internal
#' @noRd
normalize_indicator_methods <- function(
  methods,
  n_series,
  default,
  arg,
  valid,
  call = rlang::caller_env()
) {
  if (is.null(methods)) {
    return(rep(default, n_series))
  }

  if (length(methods) == 1) {
    methods <- rep(methods, n_series)
  }

  if (length(methods) != n_series) {
    rlang::abort(
      paste0(
        "`", arg,
        "` must have length 1 or match the number of indicator series."
      ),
      call = call
    )
  }

  methods <- tolower(unname(methods))
  invalid <- setdiff(methods, valid)
  if (length(invalid) > 0) {
    rlang::abort(
      paste0(
        "Unsupported values in `", arg, "`: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  unname(methods)
}


#' Recycle and validate `indic_aggregators` (character, numeric, or list)
#'
#' Only called from `validate_mf_inputs()` in `mf_model.R`. More involved
#' than `normalize_indicator_methods()` above because each entry may be a
#' named aggregation method, a numeric weight vector, or a list mixing both
#' across indicators.
#'
#' @srrstats {G2.3b} Uses `tolower()` for aggregation names too.
#' @keywords internal
#' @noRd
normalize_indicator_aggregators <- function(
  aggregators,
  n_series,
  call = rlang::caller_env()
) {
  if (is.null(aggregators)) {
    return(rep(list("mean"), n_series))
  }

  if (is.character(aggregators) && length(aggregators) == 1) {
    aggregators <- rep(list(aggregators), n_series)
  } else if (is.character(aggregators)) {
    if (length(aggregators) != n_series) {
      rlang::abort(
        paste(
          "`indic_aggregators` must have length 1 or match the number of",
          "indicator series."
        ),
        call = call
      )
    }
    aggregators <- as.list(unname(aggregators))
  } else if (is.numeric(aggregators)) {
    aggregators <- rep(list(aggregators), n_series)
  } else if (is.list(aggregators) && length(aggregators) == 1) {
    aggregators <- rep(aggregators, n_series)
  }

  if (!is.list(aggregators) || length(aggregators) != n_series) {
    rlang::abort(
      paste(
        "`indic_aggregators` must have length 1 or match the number of",
        "indicator series."
      ),
      call = call
    )
  }

  valid_names <- c("mean", "last", "sum", "expalmon", "beta", "unrestricted")

  aggregators <- lapply(
    aggregators,
    function(aggregator) {
      if (is.character(aggregator)) {
        return(tolower(aggregator))
      }

      aggregator
    }
  )

  for (aggregator in aggregators) {
    if (is.character(aggregator)) {
      if (length(aggregator) != 1 || !aggregator %in% valid_names) {
        rlang::abort(
          paste0(
            "Character values in `indic_aggregators` must be one of: ",
            paste(valid_names, collapse = ", "), "."
          ),
          call = call
        )
      }
    } else if (!is.numeric(aggregator)) {
      rlang::abort(
        "`indic_aggregators` entries must be characters or numeric weights.",
        call = call
      )
    }
  }

  aggregators
}


#' Fill in and validate `solver_options` for the parametric-weight optimizer
#'
#' Called from `validate_mf_inputs()` in `mf_model.R`, and used as the
#' default value of the `solver_options` parameter in
#' `build_indicator_features()` (`mf_model.R`) so that function also works
#' when called directly with `solver_options = NULL`. Calls
#' `check_scalar_count()`, `check_scalar_flag()`, and `check_scalar_number()`
#' above.
#'
#' @srrstats {G2.3a} Validates `solver_options$method` with `match.arg()`.
#' @srrstats {G2.4a} Normalizes integer solver controls explicitly.
#' @keywords internal
#' @noRd
normalize_parametric_solver_options <- function(
  solver_options,
  call = rlang::caller_env()
) {
  defaults <- list(
    method = "L-BFGS-B",
    maxiter = 1000L,
    n_starts = 5L,
    seed = NULL,
    trace = 0L,
    warn = TRUE,
    reltol = 1e-8,
    start_values = NULL
  )

  if (is.null(solver_options)) {
    return(defaults)
  }

  if (!is.list(solver_options)) {
    rlang::abort("`solver_options` must be a list.", call = call)
  }

  invalid <- setdiff(names(solver_options), names(defaults))
  if (length(invalid) > 0) {
    rlang::abort(
      paste0(
        "Invalid `solver_options`: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  defaults[names(solver_options)] <- solver_options
  defaults$method <- rlang::arg_match0(
    defaults$method,
    values = c("L-BFGS-B", "BFGS", "Nelder-Mead", "nlminb"),
    arg_nm = "solver_options$method",
    error_call = call
  )

  check_scalar_count(
    defaults$maxiter,
    "solver_options$maxiter",
    min = 1L,
    integer = FALSE,
    call = call
  )
  check_scalar_count(
    defaults$n_starts,
    "solver_options$n_starts",
    min = 1L,
    integer = FALSE,
    call = call
  )
  check_scalar_count(
    defaults$trace,
    "solver_options$trace",
    min = 0L,
    integer = FALSE,
    call = call
  )
  check_scalar_flag(defaults$warn, "solver_options$warn", call = call)
  if (!is.null(defaults$seed)) {
    seed_ok <- is.numeric(defaults$seed) &&
      length(defaults$seed) == 1 &&
      is.finite(defaults$seed)
    if (!seed_ok) {
      rlang::abort(
        "`solver_options$seed` must be `NULL` or a single finite number.",
        call = call
      )
    }
  }
  check_scalar_number(
    defaults$reltol,
    "solver_options$reltol",
    min = 0,
    strict = TRUE,
    call = call
  )

  defaults$maxiter <- as.integer(round(defaults$maxiter))
  defaults$n_starts <- as.integer(round(defaults$n_starts))
  defaults$trace <- as.integer(round(defaults$trace))
  if (!is.null(defaults$seed)) {
    defaults$seed <- as.integer(round(defaults$seed))
  }
  defaults$reltol <- as.numeric(defaults$reltol)

  defaults
}


#' Validate `solver_options$start_values` against the parametric specs
#'
#' Only called from `validate_mf_inputs()` in `mf_model.R`, right after
#' `normalize_parametric_solver_options()` above and after the parametric
#' indicators are identified, since it needs to know how many parameters
#' each parametric indicator expects. Calls `split_parameter_vector()`
#' (`utils-aggregation.R`) to convert a flat numeric `start_values` into
#' per-indicator blocks.
#'
#' @keywords internal
#' @noRd
validate_parametric_solver_start <- function(
  solver_options,
  parametric_specs,
  call = rlang::caller_env()
) {
  if (length(parametric_specs) == 0 || is.null(solver_options$start_values)) {
    return(solver_options)
  }

  spec_names <- names(parametric_specs)
  required_lengths <- vapply(
    parametric_specs,
    function(spec) parametric_parameter_count(spec$aggregator),
    FUN.VALUE = integer(1)
  )

  if (is.numeric(solver_options$start_values)) {
    expected_total <- sum(required_lengths)
    if (length(solver_options$start_values) != expected_total) {
      rlang::abort(
        paste0(
          "`solver_options$start_values` must contain exactly ",
          expected_total,
          " values across the parametric indicators."
        ),
        call = call
      )
    }

    if (any(!is.finite(solver_options$start_values))) {
      rlang::abort(
        "`solver_options$start_values` must contain only finite numbers.",
        call = call
      )
    }

    solver_options$start_values <- split_parameter_vector(
      parameters = as.numeric(solver_options$start_values),
      specs = parametric_specs
    )
    names(solver_options$start_values) <- spec_names
  }

  if (!is.list(solver_options$start_values)) {
    rlang::abort(
      paste(
        "`solver_options$start_values` must be a numeric vector or a named",
        "list of numeric vectors."
      ),
      call = call
    )
  }

  if (length(solver_options$start_values) != length(parametric_specs)) {
    rlang::abort(
      paste0(
        "`solver_options$start_values` must provide exactly ",
        length(parametric_specs),
        " parametric indicator start vector",
        if (length(parametric_specs) == 1) "" else "s",
        "."
      ),
      call = call
    )
  }

  if (is.null(names(solver_options$start_values))) {
    if (length(parametric_specs) > 1) {
      rlang::abort(
        paste0(
          paste0(
            "`solver_options$start_values` must be named for the ",
            "parametric indicators: "
          ),
          paste(spec_names, collapse = ", "),
          "."
        ),
        call = call
      )
    }
    names(solver_options$start_values) <- spec_names
  }

  invalid_names <- setdiff(names(solver_options$start_values), spec_names)
  if (length(invalid_names) > 0) {
    rlang::abort(
      paste0(
        "Unknown entries in `solver_options$start_values`: ",
        paste(invalid_names, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  missing_names <- setdiff(spec_names, names(solver_options$start_values))
  if (length(missing_names) > 0) {
    rlang::abort(
      paste0(
        "`solver_options$start_values` is missing values for: ",
        paste(missing_names, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  solver_options$start_values <- lapply(
    spec_names,
    function(indicator_id) {
      start_values <- solver_options$start_values[[indicator_id]]
      required_length <- required_lengths[[indicator_id]]
      aggregator <- parametric_specs[[indicator_id]]$aggregator

      if (!is.numeric(start_values) ||
        any(!is.finite(start_values)) ||
        length(start_values) != required_length) {
        rlang::abort(
          paste0(
            "`solver_options$start_values$", indicator_id,
            "` must contain exactly ",
            required_length,
            " value",
            if (required_length == 1) "" else "s",
            "."
          ),
          call = call
        )
      }

      if (identical(aggregator, "beta") && any(start_values <= 0)) {
        rlang::abort(
          paste0(
            "`solver_options$start_values$", indicator_id,
            "` must be strictly positive for `beta` aggregation."
          ),
          call = call
        )
      }

      as.numeric(start_values)
    }
  )
  names(solver_options$start_values) <- spec_names

  solver_options
}


#' Fill in and validate the `bootstrap` control list
#'
#' Only called from `validate_mf_inputs()` in `mf_model.R`. Calls
#' `check_scalar_count()` above for `bootstrap$N`.
#'
#' @keywords internal
#' @noRd
normalize_mf_bootstrap <- function(
  bootstrap,
  call = rlang::caller_env()
) {
  defaults <- list(
    N = 100L,
    block_length = NULL
  )

  if (is.null(bootstrap)) {
    return(defaults)
  }
  if (!is.list(bootstrap)) {
    rlang::abort("`bootstrap` must be a list.", call = call)
  }

  invalid <- setdiff(names(bootstrap), names(defaults))
  if (length(invalid) > 0) {
    rlang::abort(
      paste0(
        "Invalid `bootstrap` entries: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  defaults[names(bootstrap)] <- bootstrap

  check_scalar_count(
    defaults$N, "bootstrap$N", min = 1L, integer = FALSE, call = call
  )
  defaults$N <- as.integer(round(defaults$N))

  if (!is.null(defaults$block_length)) {
    block_length_ok <- is.numeric(defaults$block_length) &&
      length(defaults$block_length) == 1 &&
      is.finite(defaults$block_length) &&
      defaults$block_length >= 1
    if (!block_length_ok) {
      rlang::abort(
        "`bootstrap$block_length` must be `NULL` or a single integer >= 1.",
        call = call
      )
    }
    defaults$block_length <- as.integer(round(defaults$block_length))
  }

  defaults
}


#' Standardize a user-supplied `xreg` into the wide forecast regressor shape
#'
#' Only called from `forecast.mf_model()` in `forecast.R`, when the caller
#' supplies `xreg` explicitly instead of relying on the regressors stored on
#' the fitted `"mf_model"` object. Calls `as_mf_tbl()` above to standardize
#' `xreg` the same way `target`/`indic` are standardized at fit time.
#'
#' @keywords internal
#' @noRd
as_forecast_xreg <- function(
  xreg,
  regressor_names,
  call = rlang::caller_env()
) {
  if (length(regressor_names) == 0) {
    return(NULL)
  }

  xreg_tbl <- as_mf_tbl(
    x = xreg,
    arg = "xreg",
    default_id = regressor_names[[1]],
    call = call
  )
  xreg_wide <- suppressMessages(tsbox::ts_wide(xreg_tbl))

  missing_columns <- setdiff(regressor_names, colnames(xreg_wide))
  if (length(missing_columns) > 0) {
    rlang::abort(
      paste0(
        "`xreg` is missing required regressors: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  xreg_wide[, c("time", regressor_names), drop = FALSE]
}
