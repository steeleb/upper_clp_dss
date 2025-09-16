#' Gap fill short periods of missing df with linear interpolation
#'
#' This function uses [zoo::na.approx()] to linearly interpolate short gaps of
#' missing data in a time series. Interpolation is performed separately for each
#' combination of `site_col` and `parameter_col`. It is designed to fill only
#' short gaps and should be used cautiously to avoid introducing artifacts into
#' long missing periods.
#'
#' @param df A data frame containing time series data with missing values
#'   represented as `NA`. Data should be padded to regular time intervals (e.g.,
#'   using [padr::pad()]) before applying this function.
#' @param site_col A string giving the name of the column containing site
#'   identifiers.
#' @param parameter_col A string giving the name of the column containing
#'   parameter identifiers.
#' @param time_col A string giving the name of the column containing datetime
#'   information. Must be a `POSIXct` or numeric type, or otherwise convertible
#'   to numeric.
#' @param value_col A string giving the name of the column containing the values
#'   to be gap filled.
#' @param max_gap An integer giving the maximum number of consecutive `NA`
#'   values to interpolate. The interpretation depends on the data’s time
#'   interval (e.g., `max_gap = 4` corresponds to 1 hour of gaps in 15-minute
#'   data).
#' @param new_value_col A string giving the name of the new column to store the
#'   gap-filled values. Default is `"mean_filled"`. If this matches `value_col`,
#'   the original values will be overwritten.
#'
#' @return A data frame with the original columns plus an additional column
#'   containing the linearly interpolated values (or with the original column
#'   replaced if `new_value_col` = `value_col`).
#'
#' @details
#' This function uses grouped operations (`dplyr::group_by()`) by `site_col` and
#' `parameter_col`. Within each group, linear interpolation is applied to the
#' `value_col` based on the numeric representation of `time_col` for the number
#' of time intervals specified by `max_gap`
#'
#' @importFrom zoo na.approx
#'
#' @examples
#' \dontrun{
#' result <- linear_interp_missing_data(
#'   df = your_sensor_data,
#'   site_col = "site",
#'   parameter_col = "parameter",
#'   time_col = "DT_round",
#'   value_col = "mean",
#'   max_gap = 4, # e.g., fill up to 1 hour of gaps in 15-minute data
#'   new_value_col = "mean_filled"
#' )
#' }
#'
apply_linear_interpolation_missing_data <- function(df, site_col = "site", parameter_col = "parameter",
                                       time_col = "DT_round", value_col = "mean",
                                       max_gap = 4, new_value_col = "mean_filled") {
  df %>%
    arrange(!!sym(site_col), !!sym(parameter_col), !!sym(time_col)) %>%
    group_by(!!sym(site_col), !!sym(parameter_col)) %>%
    mutate(
      !!sym(new_value_col) := zoo::na.approx(
        x = as.numeric(!!sym(time_col)),
        object = !!sym(value_col),
        maxgap = max_gap,
        na.rm = FALSE
      )
    ) %>%
    ungroup()
}

