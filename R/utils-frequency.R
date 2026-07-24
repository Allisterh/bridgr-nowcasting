#' Default number of lower-level observations per higher-level unit
#'
#' Only called from `normalize_frequency_conversions()` below, as the base
#' the user's `frequency_conversions` overrides are merged into. See the
#' `spm`/`mph`/`hpd`/`dpw`/`wpm`/`mpq`/`qpy` documentation in [mf_model()]
#' (`mf_model.R`) for what each name means.
#'
#' @keywords internal
#' @noRd
default_frequency_conversions <- function() {
  c(
    "spm" = 60,
    "mph" = 60,
    "hpd" = 24,
    "dpw" = 7,
    "wpm" = 4,
    "mpq" = 3,
    "qpy" = 4
  )
}


#' The regular frequency ladder, coarsest last
#'
#' Only called from `observations_per_target_period()` below, to look up
#' where the indicator's and target's units fall on the
#' second/minute/.../year ladder.
#'
#' @keywords internal
#' @noRd
frequency_levels <- function() {
  c("second", "minute", "hour", "day", "week", "month", "quarter", "year")
}


#' Names of the conversion factors between adjacent frequency-ladder levels
#'
#' Only called from `observations_per_target_period()` below, to look up
#' which `frequency_conversions` entries (`spm`, `mph`, ...) apply between
#' two ladder positions. Order matches `frequency_levels()` above.
#'
#' @keywords internal
#' @noRd
frequency_edges <- function() {
  c("spm", "mph", "hpd", "dpw", "wpm", "mpq", "qpy")
}


#' Merge user-supplied `frequency_conversions` overrides into the defaults
#'
#' Only called from `validate_mf_inputs()` in `mf_model.R`. Calls
#' `default_frequency_conversions()` above.
#'
#' @keywords internal
#' @noRd
normalize_frequency_conversions <- function(
  frequency_conversions,
  call = rlang::caller_env()
) {
  defaults <- default_frequency_conversions()
  if (is.null(frequency_conversions)) {
    return(defaults)
  }

  if (!is.numeric(frequency_conversions)) {
    rlang::abort("`frequency_conversions` must be numeric.", call = call)
  }

  if (is.null(names(frequency_conversions))) {
    if (length(frequency_conversions) != length(defaults)) {
      rlang::abort(
        paste0(
          "`frequency_conversions` must be named, or it must provide all ",
          length(defaults), " default values in order."
        ),
        call = call
      )
    }
    names(frequency_conversions) <- names(defaults)
  }

  invalid_names <- setdiff(names(frequency_conversions), names(defaults))
  if (length(invalid_names) > 0) {
    rlang::abort(
      paste0(
        "Invalid names in `frequency_conversions`: ",
        paste(invalid_names, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  if (any(frequency_conversions <= 0)) {
    rlang::abort(
      "All `frequency_conversions` values must be strictly positive.",
      call = call
    )
  }

  defaults[names(frequency_conversions)] <- as.numeric(frequency_conversions)
  defaults
}


#' Infer the regular frequency of every series id in a long tibble
#'
#' Only called from `validate_mf_inputs()` in `mf_model.R`, once for
#' `target_tbl` (using only the first row of the result) and once for
#' `indic_tbl`. Calls `infer_series_frequency()` below per series group.
#'
#' @keywords internal
#' @noRd
infer_frequency_table <- function(data, call = rlang::caller_env()) {
  metadata <- data |>
    dplyr::group_by(.data$id) |>
    dplyr::group_map(
      ~ infer_series_frequency(.x$time, .y$id[[1]], call = call)
    ) |>
    dplyr::bind_rows()

  list(
    target = metadata[1, , drop = FALSE],
    indicators = metadata
  )
}


#' Infer the regular frequency of one series' timestamps
#'
#' Only called from `infer_frequency_table()` above, once per series group.
#' Tries `detect_month_frequency()` below first (calendar-aligned month
#' /quarter/year detection), then falls back to `detect_time_frequency()`
#' below (fixed-duration week/day/hour/minute/second detection).
#'
#' @keywords internal
#' @noRd
infer_series_frequency <- function(times, id, call = rlang::caller_env()) {
  times <- sort(unique(times))

  # Prefer calendar-aligned month/quarter/year detection when possible.
  month_candidate <- detect_month_frequency(times)
  if (!is.null(month_candidate)) {
    return(dplyr::tibble(
      id = id,
      unit = month_candidate$unit,
      step = month_candidate$step
    ))
  }

  time_candidate <- detect_time_frequency(times)
  if (!is.null(time_candidate)) {
    return(dplyr::tibble(
      id = id,
      unit = time_candidate$unit,
      step = time_candidate$step
    ))
  }

  rlang::abort(
    paste0(
      "Could not infer a supported regular frequency for series `", id, "`."
    ),
    call = call
  )
}


#' Detect a calendar-aligned month/quarter/year frequency
#'
#' Called from `infer_series_frequency()` above (as the primary detector) and
#' from `fallback_period_start_times()` in `utils-input.R` (to check whether
#' floored candidate timestamps yield a clean calendar frequency). Returns
#' `NULL` when timestamps are not month-floored or the month step is
#' irregular; the caller falls back to `detect_time_frequency()` below.
#'
#' @keywords internal
#' @noRd
detect_month_frequency <- function(times) {
  month_index <- lubridate::year(times) * 12L + lubridate::month(times)
  month_diff <- unique(diff(month_index))

  if (length(month_diff) != 1 || month_diff <= 0) {
    return(NULL)
  }

  aligned_to_month <- all(times == lubridate::floor_date(times, unit = "month"))
  if (!aligned_to_month) {
    return(NULL)
  }

  if (month_diff %% 12 == 0 &&
    all(times == lubridate::floor_date(times, unit = "year"))) {
    return(list(unit = "year", step = month_diff / 12))
  }

  if (month_diff %% 3 == 0 &&
    all(times == lubridate::floor_date(times, unit = "quarter"))) {
    return(list(unit = "quarter", step = month_diff / 3))
  }

  list(unit = "month", step = month_diff)
}


#' Detect a fixed-duration week/day/hour/minute/second frequency
#'
#' Called from `infer_series_frequency()` above (as the fallback detector,
#' after `detect_month_frequency()` returns `NULL`) and from
#' `fallback_period_start_times()` in `utils-input.R` (to check whether raw
#' timestamps are already regular before trying calendar-floored
#' candidates).
#'
#' @keywords internal
#' @noRd
detect_time_frequency <- function(times) {
  times_posix <- as.POSIXct(times, tz = lubridate::tz(times[[1]]) %||% "UTC")
  second_diff <- unique(as.numeric(diff(times_posix), units = "secs"))

  if (length(second_diff) != 1 || second_diff <= 0) {
    return(NULL)
  }

  if (second_diff %% (7 * 24 * 60 * 60) == 0) {
    return(list(unit = "week", step = second_diff / (7 * 24 * 60 * 60)))
  }
  if (second_diff %% (24 * 60 * 60) == 0) {
    return(list(unit = "day", step = second_diff / (24 * 60 * 60)))
  }
  if (second_diff %% (60 * 60) == 0) {
    return(list(unit = "hour", step = second_diff / (60 * 60)))
  }
  if (second_diff %% 60 == 0) {
    return(list(unit = "minute", step = second_diff / 60))
  }

  list(unit = "second", step = second_diff)
}


#' Trim target and indicator data to their common starting time
#'
#' Only called from `prepare_estimation_inputs()` in `mf_model.R`, right
#' after frequency inference, so no target period is ever missing indicator
#' history that predates the indicator's first observation.
#'
#' @keywords internal
#' @noRd
align_mf_inputs <- function(
  target_tbl,
  indic_tbl,
  target_meta,
  indic_meta
) {
  common_start <- max(min(target_tbl$time), min(indic_tbl$time))

  target_tbl <- target_tbl |>
    dplyr::filter(.data$time >= common_start)
  indic_tbl <- indic_tbl |>
    dplyr::filter(.data$time >= common_start)

  list(
    target = target_tbl,
    indic = indic_tbl,
    target_anchor = min(target_tbl$time)
  )
}


#' Number of indicator observations expected within each target period
#'
#' Called from `validate_mf_inputs()` and `build_indicator_features()` in
#' `mf_model.R`, and from `resample_mf_inputs()` in `utils-aggregation.R`.
#' Uses `frequency_levels()` and `frequency_edges()` above to walk the
#' regular ladder between the indicator's and target's inferred units and
#' multiply the relevant `frequency_conversions` factors.
#'
#' @keywords internal
#' @noRd
observations_per_target_period <- function(
  indicator_meta,
  target_meta,
  frequency_conversions,
  call = rlang::caller_env()
) {
  levels <- frequency_levels()
  indicator_unit <- indicator_meta$unit[[1]]
  target_unit <- target_meta$unit[[1]]

  indicator_index <- match(indicator_unit, levels)
  target_index <- match(target_unit, levels)

  if (indicator_index > target_index) {
    return(0)
  }

  ratio <- 1
  if (indicator_index < target_index) {
    edges <- frequency_edges()[indicator_index:(target_index - 1)]
    ratio <- prod(frequency_conversions[edges])
  }

  ratio <- ratio * target_meta$step[[1]] / indicator_meta$step[[1]]

  # Only integer ratios can be mapped cleanly into target-period blocks.
  if (!isTRUE(all.equal(ratio, round(ratio)))) {
    rlang::abort(
      paste0(
        "The inferred frequencies `", indicator_unit, "` and `", target_unit,
        "` are not aligned under the supplied `frequency_conversions`."
      ),
      call = call
    )
  }

  as.integer(round(ratio))
}


#' `h` future target-period times after the last observed target time
#'
#' Called from `prepare_estimation_inputs()` in `mf_model.R` and from
#' `build_forecast_target_times()` below (a thin identically-named-argument
#' wrapper used elsewhere for clarity). Calls `shift_time_vec()` below.
#'
#' @keywords internal
#' @noRd
target_future_times <- function(last_time, target_meta, h) {
  shift_time_vec(
    time = last_time,
    n = seq_len(h) * target_meta$step[[1]],
    unit = target_meta$unit[[1]]
  )
}


#' Map each timestamp to the start time of its enclosing target period
#'
#' Called throughout the package wherever indicator observations need to be
#' bucketed by target period: `extend_indicator_series()`,
#' `prepare_indicator_period_blocks()`, and `resample_mf_inputs()` in
#' `utils-aggregation.R`, and `prepare_future_indicator_blocks()` below.
#' Calls `unit_distance()` and `shift_time_vec()` below.
#'
#' @keywords internal
#' @noRd
compute_target_periods <- function(times, target_anchor, target_meta) {
  # Map each timestamp into the target period it belongs to.
  period_index <- floor(
    unit_distance(times, target_anchor, target_meta$unit[[1]]) /
      target_meta$step[[1]]
  )

  shift_time_vec(
    time = target_anchor,
    n = period_index * target_meta$step[[1]],
    unit = target_meta$unit[[1]]
  )
}


#' Signed distance from `origin` to each time, in units of `unit`
#'
#' Only called from `compute_target_periods()` above, to measure how many
#' whole target-period steps separate each timestamp from the anchor.
#'
#' @keywords internal
#' @noRd
unit_distance <- function(times, origin, unit) {
  if (unit %in% c("second", "minute", "hour")) {
    scale <- c("second" = 1, "minute" = 60, "hour" = 3600)[[unit]]
    return(as.numeric(difftime(times, origin, units = "secs")) / scale)
  }
  if (unit == "day") {
    return(as.numeric(difftime(times, origin, units = "days")))
  }
  if (unit == "week") {
    return(as.numeric(difftime(times, origin, units = "days")) / 7)
  }

  origin_month <- lubridate::year(origin) * 12L + lubridate::month(origin)
  time_month <- lubridate::year(times) * 12L + lubridate::month(times)
  month_diff <- time_month - origin_month

  if (unit == "month") {
    return(month_diff)
  }
  if (unit == "quarter") {
    return(month_diff / 3)
  }
  if (unit == "year") {
    return(month_diff / 12)
  }

  rlang::abort(
    paste0("Unsupported unit `", unit, "`."),
    call = rlang::caller_env()
  )
}


#' Shift one timestamp forward (or backward) by `n` units of `unit`
#'
#' Only called from `shift_time_vec()` below, once per shift amount, via
#' `lapply()`. Kept as a separate scalar function (rather than vectorizing
#' `n` directly) because `%m+%` and `lubridate::period()` need a
#' single-length step to shift months/quarters/years consistently.
#'
#' @keywords internal
#' @noRd
shift_time <- function(time, n, unit) {
  if (unit == "second") {
    return(time + lubridate::seconds(n))
  }
  if (unit == "minute") {
    return(time + lubridate::minutes(n))
  }
  if (unit == "hour") {
    return(time + lubridate::hours(n))
  }
  if (unit == "day") {
    return(time + lubridate::days(n))
  }
  if (unit == "week") {
    return(time + lubridate::weeks(n))
  }
  if (unit == "month") {
    return(time %m+% lubridate::period(num = n, units = "month"))
  }
  if (unit == "quarter") {
    return(time %m+% lubridate::period(num = 3 * n, units = "month"))
  }
  if (unit == "year") {
    return(time %m+% lubridate::period(num = n, units = "year"))
  }

  rlang::abort(
    paste0("Unsupported unit `", unit, "`."),
    call = rlang::caller_env()
  )
}


#' Vectorized wrapper around `shift_time()` above over a vector of steps `n`
#'
#' Called throughout this file (`target_future_times()`,
#' `compute_target_periods()`, `within_target_period_times()` below) and from
#' `extend_indicator_series()` in `utils-aggregation.R`, wherever a single
#' `time` origin needs to be shifted by several different step counts at
#' once.
#'
#' @keywords internal
#' @noRd
shift_time_vec <- function(time, n, unit) {
  shifted <- lapply(
    as.list(n),
    function(step_count) shift_time(time = time, n = step_count, unit = unit)
  )
  do.call(c, shifted)
}


#' Thin `target_future_times()` wrapper with forecast-specific argument names
#'
#' Only called from `resample_mf_inputs()` in `utils-aggregation.R`, to build
#' the forecast-horizon target times for one bootstrap resample. Kept
#' separate from `target_future_times()` above only for a clearer call-site
#' argument name (`last_target_time` vs. `last_time`).
#'
#' @keywords internal
#' @noRd
build_forecast_target_times <- function(
  last_target_time,
  target_meta,
  h
) {
  target_future_times(
    last_time = last_target_time,
    target_meta = target_meta,
    h = h
  )
}


#' The `n_obs` indicator-frequency timestamps making up one target period
#'
#' Called from `as_indicator_period_long()` below and from
#' `resample_mf_inputs()` in `utils-aggregation.R`, to reconstruct plausible
#' high-frequency timestamps for a block of aggregated/resampled values that
#' only carries a target-period start time.
#'
#' @keywords internal
#' @noRd
within_target_period_times <- function(period_start, indicator_meta, n_obs) {
  shift_time_vec(
    time = period_start,
    n = (seq_len(n_obs) - 1) * indicator_meta$step[[1]],
    unit = indicator_meta$unit[[1]]
  )
}


#' Expand a resampled indicator block matrix back into long-format rows
#'
#' Only called from `resample_mf_inputs()` in `utils-aggregation.R`, to turn
#' the block-bootstrapped indicator blocks (one row per target period) back
#' into the `id`/`time`/`values` long format the rest of the fitting
#' pipeline expects. Calls `within_target_period_times()` above.
#'
#' @keywords internal
#' @noRd
as_indicator_period_long <- function(
  indicator_id,
  target_times,
  blocks,
  indicator_meta
) {
  rows <- lapply(
    seq_len(nrow(blocks)),
    function(period_index) {
      dplyr::tibble(
        id = indicator_id,
        time = within_target_period_times(
          period_start = target_times[[period_index]],
          indicator_meta = indicator_meta,
          n_obs = ncol(blocks)
        ),
        values = as.numeric(blocks[period_index, ])
      )
    }
  )

  dplyr::bind_rows(rows)
}


#' @keywords internal
#' @noRd
prepare_future_indicator_blocks <- function(
  indicator_tbl,
  target_meta,
  target_anchor,
  future_target_times
) {
  if (length(future_target_times) == 0) {
    return(list(
      periods = future_target_times,
      values = list()
    ))
  }

  periods <- compute_target_periods(
    indicator_tbl$time,
    target_anchor = target_anchor,
    target_meta = target_meta
  )
  future_tbl <- indicator_tbl |>
    dplyr::mutate(period = periods) |>
    dplyr::filter(.data$period %in% future_target_times) |>
    dplyr::group_by(.data$period) |>
    dplyr::arrange(.data$time, .by_group = TRUE) |>
    dplyr::summarise(values = list(.data$values), .groups = "drop")

  list(
    periods = future_tbl$period,
    values = future_tbl$values
  )
}
