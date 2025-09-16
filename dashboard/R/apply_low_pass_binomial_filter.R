#' @title Apply low-pass binomial filter to reduce sensor noise
#' @export
#'
#' @description
#' Reduces high-frequency noise in turbidity measurements using a 5-point binomial kernel
#' low-pass filter applied three times in succession. This function specifically addresses
#' the noise characteristics of optical turbidity sensors, which are prone to rapid
#' fluctuations due to suspended particles, biofouling, and electronic interference.
#'
#' The filter uses a binomial kernel with weights [1,4,6,4,1] normalized by dividing by 16, which provides
#' a Gaussian-like smoothing effect. This kernel gives the greatest influence to the
#' current measurement, moderate influence to the measurements 15 minutes before and
#' after, and least influence to the measurements 30 minutes before and after. The
#' triple application of the filter creates a more aggressive smoothing effect equivalent
#' to a higher-order binomial filter.
#'
#' This function currently processes only turbidity data, as turbidity sensors are
#' particularly susceptible to noise from suspended particles and optical interference.
#' For other parameters, the function returns the input dataframe unchanged.
#'
#' @param df A data frame containing water quality measurements. Must include columns for site, datetime,  parameter, value
#'
#' @param data_format The format of the input data. Options are either "dataframe" or "list"
#' If dataframe is selected, all data (multiple site-parameter combinations) are stored in a single long dataframe.
#' If list is selected, data are stored in a list of dataframes, with each dataframe containing data for a single site-parameter combination
#' Default is "dataframe".
#' @param site_col The name of the column containing site identifiers. Default is "site".
#' @param parameter_col The name of the column containing parameter identifiers. Default is "parameter".
#' @param time_col The name of the column containing datetime information. Must be a `POSIXct` or numeric type, or otherwise convertible to numeric. Default is "DT_round".
#' @param value_col The name of the column containing the values to be smoothed. Default is "mean".
#' @param new_value_col The name of the new column to store smoothed values. Default is "smoothed_mean".
#'
#' @return A data frame with the same structure as the input, but with an additional
#' `smoothed_mean` column containing the filtered turbidity values. For non-turbidity
#' parameters, returns the original dataframe unchanged.
#'
#' @examples
#' \dontrun{
#'
#' result <- apply_low_pass_binomial_filter(
#'  data = your_sensor_data,
#'  data_format = "dataframe",
#'  site_col = "site",
#'  parameter_col = "parameter",
#'  time_col = "DT_round",
#'  value_col = "mean",
#'  new_value_col = "smoothed_mean"
#'  )
#'
#'  # For list format
#'  result_list <- apply_low_pass_binomial_filter(
#'  data = your_sensor_data_list,
#'  data_format = "list")

apply_low_pass_binomial_filter <- function(df,
                                          data_format = "dataframe",
                            site_col = "site",
                            parameter_col = "parameter",
                            time_col = "DT_round",
                            value_col = "mean",
                            new_value_col = "smoothed_mean") {

  # Function to add column if it doesn't exist
  add_column_if_not_exists <- function(df, column_name, default_value = NA) {

    if (!column_name %in% colnames(df)) {
      df <- df %>% dplyr::mutate(!!sym(column_name) := default_value)
    }
    return(df)
  }

  # 5-point binomial kernel [1,4,6,4,1]
  binomial_kernel <- function(int_vec) {
    kernel <- c(1, 4, 6, 4, 1)
    sum(int_vec * kernel) / 16
  }

  if(data_format == "dataframe"){
    data_smoothed <- df %>%
      arrange(!!sym(site_col), !!sym(parameter_col), !!sym(time_col)) %>%
      group_split(!!sym(site_col), !!sym(parameter_col)) %>%   # split into list of dfs
      purrr::map(~ {
        .x <- add_column_if_not_exists(.x, new_value_col)

        this_param <- dplyr::first(.x[[parameter_col]])

        if (!is.na(this_param) && this_param %in% c("Turbidity", "Chl-a Fluorescence")) {
          .x <- .x %>%
            mutate(
              # First pass
              !!sym(new_value_col) := data.table::frollapply(
                !!sym(value_col), n = 5, FUN = binomial_kernel,
                fill = NA, align = "center"
              ),
              # Second pass
              !!sym(new_value_col) := data.table::frollapply(
                !!sym(new_value_col), n = 5, FUN = binomial_kernel,
                fill = NA, align = "center"
              ),
              # Third pass
              !!sym(new_value_col) := data.table::frollapply(
                !!sym(new_value_col), n = 5, FUN = binomial_kernel,
                fill = NA, align = "center"
              )
            )
        } else {
          .x <- .x %>%
            mutate(!!sym(new_value_col) := NA_real_)
        }
        # Fill remaining NA in smoothed column with original values
        .x <- .x %>%
          mutate(
            !!sym(new_value_col) := if_else(
              is.na(!!sym(new_value_col)) & !is.na(!!sym(value_col)),
              !!sym(value_col),
              !!sym(new_value_col)
            )
          )
        return(.x)
      }) %>%
      dplyr::bind_rows()   # put the list back together

    return(data_smoothed)
  }else if(data_format == "list"){

    data_smoothed <- df %>%
      purrr::map(~ {
        .x <- add_column_if_not_exists(.x, new_value_col)
        #Assumes that list only has single parameter per dataframe
        this_param <- dplyr::first(.x[[parameter_col]])

        if (!is.na(this_param) && this_param %in% c("Turbidity", "Chl-a Fluorescence")) {
          .x <- .x %>%
            arrange(!!sym(time_col)) %>%
            mutate(
              # First pass
              !!sym(new_value_col) := data.table::frollapply(
                !!sym(value_col), n = 5, FUN = binomial_kernel,
                fill = NA, align = "center"
              ),
              # Second pass
              !!sym(new_value_col) := data.table::frollapply(
                !!sym(new_value_col), n = 5, FUN = binomial_kernel,
                fill = NA, align = "center"
              ),
              # Third pass
              !!sym(new_value_col) := data.table::frollapply(
                !!sym(new_value_col), n = 5, FUN = binomial_kernel,
                fill = NA, align = "center"
              )
            )
        } else {
          .x <- .x %>%
            mutate(!!sym(new_value_col) := NA_real_)
        }
        # Fill remaining NA in smoothed column with original values
        .x <- .x %>%
          mutate(
            !!sym(new_value_col) := if_else(
              is.na(!!sym(new_value_col)) & !is.na(!!sym(value_col)),
              !!sym(value_col),
              !!sym(new_value_col)
            )
          )
        return(.x)
      })

    return(data_smoothed)
  } else {
    stop("Invalid data_format. Choose either 'dataframe' or 'list'.")
  }

}

