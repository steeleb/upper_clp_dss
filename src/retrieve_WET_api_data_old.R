#Function to pull in data livestreaming from WET Radio Telemetry
#' @title Retrieve WET API Data
#' @description This function retrieves data from the WET API and processes it into a data frame.
#' @param site_code The site code to retrieve data for.
#' @param start_datetime The start date for the data retrieval in "YYYY-MM-DD HH:MM" format in MT
#' @param end_date The end date for the data retrieval in "YYYY-MM-DD HH:MM"" format. Default to System Time in MT
#' @param data_type The type of data to retrieve. Default is "all" otherwise a vector of strings containing WQ parameters
#' @param time_window The time window for the data retrieval. Default is "2 hours". If "all" then will pull all data since start_datetime
#' @return A data frame containing the retrieved data.
#' @example
#' #' # Retrieve data for site number 12345 from 2023-01-01 to 2023-01-31
#' #' data <- retrieve_WET_api_data(site_code = "sfm", start_datetime = "2025-04-25 10:00",end_datetime = "2025-05-01 10:00", data_type = "all")
retrieve_WET_api_data <- function(site_code, start_datetime, end_datetime = Sys.time(), data_type = "all", time_window = "2 hours") {
  # Construct the URL for the API request
  `%nin%` = Negate(`%in%`)
  #convert start and end datetimes

  #parse the start and end datetimes

  if(is.character(start_datetime)){
    start_datetime = ymd_hm(start_datetime, tz = "America/Denver")
    #if this does not parse, try other methods
    if(is.na(start_datetime)){
      start_datetime <- parse_date_time2(start_datetime, orders = c("Ymd HMS", "Ymd HM", "Ymd HMS z", "Y md HM z"), tz = "America/Denver")
      #if this still does not parse, throw an error
      if(is.na(start_datetime)){
        stop("start_datetime could not be parsed. Please provide a valid datetime in 'YYYY-MM-DD HH:MM' format.")
      }
    }
  }else{
    start_datetime <-with_tz(start_datetime, tzone = "MST") # not sure why this adjustment is not working correctly...
  }

  if(is.character(end_datetime)){
    end_datetime = ymd_hm(end_datetime, tz = "America/Denver")
    #if this does not parse, try other methods
    if(is.na(end_datetime)){
      end_datetime <- parse_date_time2(check, orders = c("Ymd HMS", "Ymd HM", "Ymd HMS z", "Y md HM z"), tz = "America/Denver")
      #if this still does not parse, throw an error
      if(is.na(end_datetime)){
        stop("end_datetime could not be parsed. Please provide a valid datetime in 'YYYY-MM-DD HH:MM' format.")
      }
    }
  }else{
    end_datetime <-with_tz(end_datetime, tzone = "MST") + hours(2)# not sure why this adjustment is not working correctly...
  }

  if(time_window == "2 hours"){
    increments_since_now = 10 # really should be 8 but we will use 10 to be safe
  }else{
    #WET system uses number of values to display to create urls so we are going to back calculate (assuming 4 datapoints an hour)
    #add an hour to be safe
    increments_since_now <- round(as.numeric(difftime(with_tz(Sys.time()+hours(1), tzone = "MST"), start_datetime, units = "hours"))*4, digits = 0)
  }


  #tibble of the sensor numbers and their corresponding data types
  sensor_numbers <- tibble(data_type = c("Depth", "pH", "Turbidity", "Specific Conductivity",
                                         "FDOM Fluorescence", "DO", "Chl-a Fluorescence", "Temperature"),
                           sensor_number = c(11, 12, 13, 15, 16, 17,19,20))
  #tibble of the site numbers and their corresponding site codes
  site_numbers <- tibble(site_code = c("sfm", "chd", "pfal"),
                         site_num = c(115170, 115290, 115310))%>%
    filter(site_code == !!site_code) #filter for sites selected

  #grab either all parameters or only the selected parameters
  if(data_type == "all"){
    selected_parameters <- sensor_numbers
  } else {
    selected_parameters <- sensor_numbers %>%
      filter(data_type %in% data_type)
  }
  #create a tibble of the urls to pull data from
  selected_urls <- selected_parameters%>%
    bind_cols(site_numbers)%>% #add in site numbers needed to look up
    mutate(indv_url = paste0("https://wetmapgc.wetec.us/cgi-bin/datadisp_q?ID=", site_num, sensor_number, "&NM=", increments_since_now))

  process_urls <- function(indv_url, data_type, site_code) {
    response <- httr2::request(indv_url)%>%
      httr2::req_perform()
    response_content <- response%>%
      httr2::resp_body_string()

    lines <- strsplit(response_content, "\n")[[1]]
    data_rows <- lines[16:(length(lines) - 4)]

    df <- data.frame(data = data_rows) %>%
      separate(data, into = c("Date", "Time", "value", "Raw", "Alarm"), sep = "\\s+", remove = FALSE)%>%
      mutate(value = ifelse(!is.na(value), as.numeric(value), NA)) %>%
      mutate(DT = as.POSIXct(paste0(Date, " ", Time), format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver"),
             DT_round = round_date(DT, unit = "15 minutes"),
             DT_round = with_tz(DT_round, tzone = "MST"))%>%
      select(-c(Date, Time, data, Raw, Alarm, DT)) %>%
      group_by(DT_round)%>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")%>%
      mutate(site = site_code,
             parameter = data_type)

    return(df)
  }

  #map over the urls and process them to grab station data for all parameters selected
  WQ_data <- pmap(list(selected_urls$indv_url,
                       selected_urls$data_type,
                       selected_urls$site_code),
                  process_urls)%>%
    bind_rows()%>%
    #filter for selected start/end dates
    filter(between(DT_round, start_datetime, end_datetime)) %>%
    filter(value %nin% c(-9999, 638.30, -99.99)) %>% # these are values used by WET engineers to test system of if system is down
    # Filter for deployed date based on site_code
    # WET Engineers were performing in house tests with the data to test transmission so we want to remove all of these always
    filter(
      (site_code == "sfm" & DT_round > ymd_hms("2024-04-25 10:00:00", tz = "MST")) |
        (site_code == "chd" & DT_round > ymd_hms("2025-03-28 10:00:00", tz = "MST")) |
        (site_code == "pfal" & DT_round > ymd_hms("2024-09-25 10:00:00", tz = "MST")))%>%
    #remove NaN rows
    na.omit()

  return(WQ_data)

}




