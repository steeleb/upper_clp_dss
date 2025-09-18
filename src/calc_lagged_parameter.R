#' Calculate lagged values for time series data
#'
#' This function computes lagged values for a specified column in a data frame
#' containing time series data. It allows for grouping by site and parameter
#' columns, and creates a new column with the lagged values.
#'
#'
#' @param df A data frame containing time series data with missing values
#'   represented as `NA`.
#' @param site_col A string giving the name of the column containing site
#'   identifiers.
#' @param parameter_col A string giving the name of the column containing
#'   parameter identifiers.
#' @param dt_col A string giving the name of the column containing datetime
#'   information. Must be a `POSIXct` or numeric type, or otherwise convertible
#'   to numeric.
#' @param value_col A string giving the name of the column containing the values
#'   to be gap filled.
#' @param parameter A string specifying the parameter to apply lagging to. Default
#'  is "Temperature".
#'
#' @param lag_fun A string specifying the lagging function to use. Should be one of
#'  "mean", "median", "min", "max", "sd", or "var". Default is "median".
#'
#' @param time_period A string specifying the time period for lagging. Should be one of
#'  "min", "hour", "day" & "week" with a number specifying the number that unit. Default is "4 hour".
#'
#' @param new_value_col A string giving the name of the new column to store the
#'   gap-filled values. Default is `"lagged_mean"`.
#'
#' @return A data frame with the original columns plus an additional column
#'   containing the linearly interpolated values (or with the original column
#'   replaced if `new_value_col` = `value_col`).
#'
#' @details
#' This function uses grouped operations (`dplyr::group_by()`) by `site_col` and
#' `parameter_col`. Within each group, linear interpolation is applied to the
#' `value_col` based on the numeric representation of `dt_col` for the number
#' of time intervals specified by `max_gap`
#'
#' @importFrom zoo na.approx
#'
#'
#'

calc_lagged_parameter <- function(wide_df, site_col = "site", parameter_col = "Temperature",
                                  dt_col = "DT_round", lag_fun = "median",
                                  time_period = "12 hour", new_value_col = "lagged_mean") {

  #seperate time period by space
  time_period_split <- strsplit(time_period, " ")[[1]]
  time_period_num <- as.numeric(time_period_split[1])
  # #parsing function
  fn <- switch(lag_fun,
               "var" = stats::var,
               "mean" = mean,
               "median" = median,
               "max" = max,
               "min" = min,
               "sd" = sd,
               stop("Unsupported lag_fun: ", lag_fun)
  )

  fn_used <- if (lag_fun %in% c("mean", "median", "max", "min", "sum")) {
    function(x) fn(x, na.rm = TRUE)
  } else if(lag_fun %in% c("var")) {
    function(x) fn(x, use = "na.or.complete")
  } else {
    function(x) fn(x)
  }

  wide_df %>%
    arrange(!!sym(site_col), !!sym(dt_col)) %>% # arrange by site and time
    group_by(!!sym(site_col)) %>% # group by site only
    mutate(
      !!sym(new_value_col) := slide_period_dbl(
        .x = !!sym(parameter_col), # use the specified parameter column
        .i = !!sym(dt_col), # use the time column for indexing
        .period = time_period_split[2], # use the specified time unit
        .f = fn_used, # use match.fun to convert string to function
        .before = time_period_num, # look back the specified time period
        .complete = FALSE # do not require complete windows
      )
    ) %>%
    ungroup()

}

