#' @title Apply timestep median aggregation
#' @export
#'
#' @description
#' Aggregates water quality data to a user-defined timestep (e.g., hourly, daily) by
#' computing the median of measurements within each timestep interval. This approach
#' reduces the influence of short-term variability and outliers, while retaining the
#' overall temporal patterns in the data.
#' Users can submit either a single long-format dataframe containing multiple sites and parameters,
#' or a list of dataframes where each dataframe contains data for a single site-parameter combination.
#'
#' @param df Input data containing water quality measurements. Must include columns for
#' site, datetime, parameter, and value.
#' @param data_format The format of the input data. Options are either `"dataframe"` or `"list"`.
#' If `"dataframe"` is selected, all data (multiple site-parameter combinations) are stored in
#' a single long dataframe. If `"list"` is selected, data are stored in a list of dataframes,
#' with each dataframe containing data for a single site-parameter combination.
#' Default is `"dataframe"`.
#' @param site_col The name of the column containing site identifiers. Default is `"site"`.
#' @param parameter_col The name of the column containing parameter identifiers. Default is `"parameter"`.
#' @param time_col The name of the column containing datetime information. Must be a `POSIXct`
#' or otherwise compatible with `lubridate::round_date()`. Default is `"DT_round"`.
#' @param value_col The name of the column containing the values to be aggregated. Default is `"mean"`.
#' @param new_value_col The name of the new column to store median-aggregated values.
#' Default is `"smoothed_mean"`.
#' @param timestep The time resolution for aggregation, passed to `lubridate::floor_date()`.
#' Common values include `"1 hour"`, `"1 day"`, `"15 minutes"`, etc. Default is `"1 hour"`.
#'
#' @return
#' If `data_format = "dataframe"`, returns a dataframe with one row per `(site, parameter, timestep)`
#' combination and a column containing the median values.
#' If `data_format = "list"`, returns a list of dataframes aggregated in the same way.
#'
#' @examples
#' \dontrun{
#'
#' # Aggregate to hourly median
#' result <- apply_timestep_median(
#'   df = your_sensor_data,
#'   data_format = "dataframe",
#'   site_col = "site",
#'   parameter_col = "parameter",
#'   time_col = "DT_round",
#'   value_col = "mean",
#'   new_value_col = "smoothed_mean",
#'   timestep = "1 hour"
#' )
#'
#' # Aggregate to daily median with list input
#' result_list <- apply_timestep_median(
#'   df = your_sensor_data_list,
#'   data_format = "list",
#'   timestep = "1 day"
#' )
#' }


apply_timestep_median <- function(df,
                                  data_format = "dataframe",
                                  site_col = "site",
                                  parameter_col = "parameter",
                                  time_col = "DT_round",
                                  value_col = "mean",
                                  new_value_col = "smoothed_mean",
                                  timestep = "1 hour") {

  # Function to add column if it doesn't exist
  add_column_if_not_exists <- function(df, column_name, default_value = NA) {
    if (!column_name %in% colnames(df)) {
      df <- df %>% dplyr::mutate(!!sym(column_name) := default_value)
    }
    return(df)
  }

  if (data_format == "dataframe") {
    data_smoothed <- df %>%
      mutate(
        .time_group = lubridate::round_date(!!sym(time_col), unit = timestep)
      ) %>%
      group_by(!!sym(site_col), !!sym(parameter_col), .time_group) %>%
      summarise(!!sym(new_value_col) := median(!!sym(value_col), na.rm = TRUE),
                .groups = "drop") %>%
      rename(!!sym(time_col) := .time_group)

    return(data_smoothed)

  } else if (data_format == "list") {
    data_smoothed <- purrr::map(df, ~ {
      .x <- add_column_if_not_exists(.x, new_value_col)

      .x %>%
        mutate(.time_group = lubridate::floor_date(!!sym(time_col), unit = timestep)) %>%
        group_by(!!sym(site_col), !!sym(parameter_col), .time_group) %>%
        summarise(!!sym(new_value_col) := median(!!sym(value_col), na.rm = TRUE),
                  .groups = "drop") %>%
        rename(!!sym(time_col) := .time_group)
    })

    return(data_smoothed)

  } else {
    stop("Invalid data_format. Choose either 'dataframe' or 'list'.")
  }
}
