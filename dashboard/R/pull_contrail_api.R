#' @title Download water quality data from Contrail website
#'
#' @description
#' Downloads raw water quality monitoring data from the Contrail platform for
#' specified two water quality sites maintained by the City of Fort Collins,
#' Manner's Bridge (PMAN) and Indian Meadows (Poudre Below Rustic, PBR) for the specified time period.
#'
#' This script does not filter the data obtained from Contrail and returns all data available from API call.
#' Note the timezones for the returned dataset and the input timestamps.
#'
#' @param start_DT POSIXct timestamp indicating the starting point for data
#' retrieval. Timestamp should be in America/Denver timezone, if this is not the case, it will be updated to America/Denver.
#'
#' @param end_DT POSIXct timestamp indicating the endpoint for data retrieval.
#' Timestamp should be in America/Denver timezone, if this is not the case, it will be updated to America/Denver.
#' Default is the current system time (Sys.time()).
#'
#' @param username Character string for the Contrail account username used to access the site.
#'                  Obtained from City of Fort Collins staff
#'
#' @param password Character string for the Contrail account password used to access the site
#'                 Obtained from City of Fort Collins staff
#'
#' @param api_urls dataframe containing the URLs for the Contrail API endpoints.
#' This includes three sections of the url, site_id, site and device_id, as well as the site code (site_code)
#' and the parameter for this associated url.
#'
#' @return A list of dataframes, each containing the water quality data for a specific site.
#' The dataframes contain the following columns:
#' - `DT_round`: Datetime of the measurement in UTC, rounded to the nearest 15 minute interval.
#' - `DT_round_MT`: Datetime of the measurement in America/Denver timezone, rounded to the nearest 15 minute interval.
#' - `DT_join`: Character form of DT_round
#' - `parameter`: The water quality parameter being measured (e.g., "DO", "Turbidity", etc.), supplied by the api_urls dataframe
#' - `site`: The site where the measurement was taken (e.g., "PMAN", "PBR") with "_fc" added to the end of the site name
#' - `value`: The value of the measurement for the specified parameter
#' - `units`: The units of the measurement (e.g., "mg/L", "NTU", etc.), supplied by Contrail
#'
#'
#' @examples
#'
#'# Get start and end dates for the data pull
#'start_DT <- Sys.time() - days(7) # in America/Denver timezone
#'end_DT <- Sys.time() # in America/Denver timezone
#'
#' #Get credentials for Contrail API
#'creds <- yaml::read_yaml("creds/contrail_creds.yml") %>%
#' unlist()
#'
#' username <- as.character(creds["username"])
#' password <- as.character(creds["password"])
#'
#' # Read the API URLs from a CSV file
#' contrail_api_urls <- read_csv('creds/contrail_device_urls.csv')
#'
# Call the downloader function
#' data <- pull_contrail_api(start_DT = start_DT,
#'                           end_DT = end_DT,
#'                           username = username,
#'                           password = password,
#'                           api_urls = contrail_api_urls)

pull_contrail_api <- function(start_DT, end_DT = Sys.time(), username, password, api_urls) {

  # Check if both objects are datetime objects (POSIXct or POSIXlt)
  if (!inherits(start_DT, c("POSIXct", "POSIXlt"))) {
    stop("start_DT is not a datetime object")
  }
  if (!inherits(end_DT, c("POSIXct", "POSIXlt"))) {
    stop("end_DT is not a datetime object")
  }
  # Check timezone
  start_tz <- attr(start_DT, "tzone")
  end_tz <- attr(end_DT, "tzone")

  # Handle NULL timezone (system default) and set to Sys.timezone to check later
  if (is.null(start_tz)|start_tz == "") start_tz <- Sys.timezone()
  if (is.null(end_tz)||end_tz == "") end_tz <- Sys.timezone()
  #check to see if start_tz is in America/Denver from user submission or if Sys.timezone is in America/Denver
  #correct start_DT to America/Denver if not
  if (start_tz != "America/Denver") {
    message("start_DT or Sys.timezone() is not in America/Denver timezone.\n Changing start_DT to America/Denver timezone.")
    start_DT <- with_tz(start_DT, tzone = "America/Denver")
  }
  # Same as above for end_DT
  if (end_tz != "America/Denver") {
    message("end_DT or Sys.timezone() is not in America/Denver timezone.\nChanging end_DT to America/Denver timezone.")
    end_DT <- with_tz(end_DT, tzone = "America/Denver")
  }


  # Create session file for persistent cookies
  session_file <- tempfile()

  # Get the login page and extract CSRF token
  login_page_req <- request("https://contrail.fcgov.com/login/?status=300&message=Redirection:%20Multiple%20Choices&continue=ZA") |>
    req_cookie_preserve(session_file)

  login_page_resp <- req_perform(login_page_req)

  # Extract CSRF token
  csrf_token <- login_page_resp %>%
    resp_body_html() |>
    html_node("input[name='csrf_token']") |>
    html_attr("value")

  #  Login with credentials
  login_req <- request("https://contrail.fcgov.com/login/?status=300&message=Redirection:%20Multiple%20Choices&continue=ZA") |>
    req_cookie_preserve(session_file) |>
    req_method("POST") |>
    req_body_form(
      username = username,
      password = password,
      login = "login",
      continue = "ZA",  # from your earlier form inspection
      csrf_token = csrf_token
    )

  login_resp <- req_perform(login_req)

  # Check if login was successful
  if(resp_status(login_resp) == 200 && !grepl("login", login_resp$url)) {
    message("Login successful! Testing Download")
  } else {
    stop("Login failed - check credentials")
  }

  start_DT_encoded <- format(start_DT, "%Y-%m-%d 20%H:%M:%S")%>%
    gsub(" ", "%", .)


  end_DT_encoded <- format(end_DT, "%Y-%m-%d 20%H:%M:%S")%>%
    gsub(" ", "%", .)

  # Use the api url metadata and build URLs
  metas <- api_urls %>%
    mutate(encoded_url = paste0("https://contrail.fcgov.com/export/file/?site_id=",site_id, "&site=",site , "&device_id=", device_id, "&mode=&hours=&data_start=",
                                start_DT_encoded, "&data_end=", end_DT_encoded, "&tz=US%2FMountain&format_datetime=%25Y-%25m-%25d+%25H%3A%25i%3A%25S&mime=txt&delimiter=comma"
                                ))

  # Download function for individual URLs
  download_data <- function(encoded_url, parameter, site_code) {
    tryCatch({
      #setup request
      download_req <- request(encoded_url)%>%
        req_cookie_preserve(session_file)
      #perform request
      download_resp <- req_perform(download_req)

      #if URL is parsed correctly (resp_status = 200), grab the data
      if(resp_status(download_resp) == 200) {

#TODO: implement try catch here in case there are other errors with the request

        # Save the CSV data
        csv_content <- resp_body_string(download_resp)

        # Parse the CSV content
        parsed_data <- read_csv(csv_content, show_col_types = FALSE)%>%
          #convert datetimes, add in sites and paramters
          mutate(
               DT_MT = force_tz(Reading, tzone = "America/Denver"), # Force Reading to America/Denver timezone
               DT = with_tz(DT_MT, tzone = "UTC"), # Force Reading to UTC timezone
               DT_round = round_date(DT, "15 minutes"), # Round the DT to 15 minute intervals
               DT_round_MT = with_tz(DT_round, tz = "America/Denver"), # Create a MT version of the Reading date
               DT_join = as.character(DT_round), # Create a character version of the UTC time
               parameter = parameter, # Add the parameter
               site = paste0(tolower(site_code), "_fc")) %>% # note that the site is managed by Fort Collins
          dplyr::select(DT_round, DT_round_MT, DT_join, parameter, site,
                  value = Value, units = Unit) #fix names to match other API sources

        return(parsed_data)

      } else {
        message("Download failed for: ", site_code, " ", parameter)
        return(NULL)
      }

    }, error = function(e) {
      message("Error downloading from: ", site_code, " ", parameter)
      return(NULL)
    })
  }

  # Process all URLs
  results <- pmap(list(metas$encoded_url, metas$parameter, metas$site_code), download_data)%>%
    #remove any missing datasets
    compact()
  message("Download Successfull for ", length(results), " of ", length(metas$encoded_url), " datasets")
  return(results)
}



