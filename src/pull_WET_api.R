pull_wet_api <- function(site_code, start_datetime, end_datetime = Sys.time(), data_type = "all", time_window = "2 hours") {

  # Helper function
  `%nin%` <- Negate(`%in%`)

  # Pre-define constants to avoid repeated object creation
  sensor_numbers <- data.frame(
    data_type = c("Stage", "Depth", "pH", "Turbidity", "Specific Conductivity",
                  "FDOM Fluorescence", "DO", "Chl-a Fluorescence", "Temperature"),
    sensor_number = c("07", "11", "12", "13", "15", "16", "17", "19", "20"),
    stringsAsFactors = FALSE
  )


  # Select parameters
  if (data_type == "all") {
    selected_parameters <- sensor_numbers
  } else {
    selected_parameters <- sensor_numbers[sensor_numbers$data_type %in% data_type, ]
  }

  #site numbers in WET system
  site_numbers <- data.frame(
    site_code = c("sfm", "chd", "pfal"),
    site_num = c(115170, 115290, 115310),
    stringsAsFactors = FALSE
  )

  # Get site info (early validation)
  site_info <- site_numbers[site_numbers$site_code == site_code, ]
  if (nrow(site_info) == 0) {
    stop("Invalid site_code: ", site_code)
  }

  # sensors were used for testing in office before deployed and we just want to make sure we are not including that data when we pull the data in
  deployment_dates <- data.frame(
    site_code = c("sfm", "chd", "pfal"),
    deploy_date = as.POSIXct(c("2024-04-25 10:00:00", "2025-03-28 10:00:00", "2024-09-25 10:00:00"),
                             tz = "America/Denver"),
    stringsAsFactors = FALSE
  )

  # Get deployment date for early filtering
  deploy_date <- deployment_dates[deployment_dates$site_code == site_code, "deploy_date"]


  # Optimized datetime parsing function
  parse_datetime_optimized <- function(dt, target_tz = "America/Denver") {
    if (is.character(dt)) {
      # Try most common format first
      parsed <- as.POSIXct(dt, format = "%Y-%m-%d %H:%M", tz = target_tz)
      if (!is.na(parsed)) return(parsed)

      # Fallback to lubridate parsing
      parsed <- ymd_hm(dt, tz = target_tz)
      if (!is.na(parsed)) return(parsed)

      # Final fallback
      parsed <- parse_date_time2(dt, orders = c("Ymd HMS", "Ymd HM", "Ymd HMS z", "Y md HM z"), tz = target_tz)
      if (is.na(parsed)) {
        stop("datetime could not be parsed. Please provide a valid datetime in 'YYYY-MM-DD HH:MM' format.")
      }
      return(parsed)
    } else {
      return(with_tz(dt, tzone = "America/Denver"))
    }
  }

  # Parse datetimes
  start_datetime <- parse_datetime_optimized(start_datetime)
  end_datetime <- parse_datetime_optimized(end_datetime)
  if (!is.character(end_datetime)) {
    end_datetime <- end_datetime + hours(2)
  }

  # Calculate increments
  if (time_window == "2 hours") {
    increments_since_now <- 10
  } else {
    increments_since_now <- round(as.numeric(difftime(with_tz(Sys.time() + hours(1), tzone = "America/Denver"),
                                                      start_datetime, units = "hours")) * 4, digits = 0)
  }


  # Create URLs to look up for site
  urls_df <- data.frame(
    data_type = selected_parameters$data_type,
    sensor_number = selected_parameters$sensor_number,
    site_code = site_info$site_code,
    site_num = site_info$site_num,
    stringsAsFactors = FALSE
  )%>%
    #CHD does not have stage data
    filter(!(site_code == "chd" & data_type == "Stage"))%>%
    #create URL for each sensor and increments
    mutate(indv_url =  paste0("https://wetmapgc.wetec.us/cgi-bin/datadisp_q?ID=", site_num, sensor_number,"&NM=", increments_since_now))

  # data html processing function
  process_urls_optimized <- function(indv_url, data_type, site_code) {
    tryCatch({

      # Read and parse HTML
      html_content <- read_html(indv_url) %>%
        html_node("pre") %>%
        html_text()

      # Split into lines and clean
      lines <- str_split(html_content, "\n")[[1]]

      # Remove empty lines and header lines
      data_lines <- lines[str_detect(lines, "\\d{2}/\\d{2}/\\d{4}")]

      if(data_type == "Stage"){
        parsed_data <- data.frame(data = data_lines) %>%
          separate(data, into = c("Date", "Time", "value","CFS",  "Raw", "Alarm"),
                   sep = "\\s+",
                   fill = "right") %>%
          mutate(
            Date = mdy(Date), # get date
            Time = hms::as_hms(Time), # get time
            value = as.numeric(value), #convert value to numeric
            # Create datetime column using America/Denver time zone
            datetime = as_datetime(paste0(Date," ", Time), tz = "America/Denver")) %>%
          # Select relevant columns
          select(datetime, value)%>%
          #create UTC DT
          mutate(DT_round_utc = with_tz(round_date(datetime, unit = "15 minutes"), tz = "UTC"))%>%
          # Group and summarize in case there are multiple rows per DT
          group_by(DT_round_utc) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop")%>%
          #add in site and parameter
          mutate(site = site_code,
                 parameter = data_type)
      }else{
        # Parse into tibble
        parsed_data <- tibble(raw_line = data_lines) %>%
          separate(raw_line,
                   into = c("Date", "Time", "value", "NULL_col", "Raw", "Alarm"),
                   sep = "\\s+",
                   fill = "right") %>%
          mutate(
            Date = mdy(Date), # get date
            Time = hms::as_hms(Time), # get time
            value = as.numeric(value), #convert value to numeric
            # Create datetime column using America/Denver time zone
            datetime = as_datetime(paste0(Date," ", Time), tz = "America/Denver")) %>%
          # Select relevant columns
          select(datetime, value)%>%
          #create UTC DT
          mutate(DT_round_utc = with_tz(round_date(datetime, unit = "15 minutes"), tz = "UTC"))%>%
          # Group and summarize in case there are multiple rows per DT
          group_by(DT_round_utc) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop")%>%
          #add in site and parameter
          mutate(site = site_code,
                 parameter = data_type)
      }

      return(parsed_data)

    }, error = function(e) {
      warning("Error processing URL for ", data_type, ": ", e$message)
      return(NULL)
    })
  }

  #not sure if this works yet...
  is_shiny <- tryCatch({
    shiny::isRunning()
  }, error = function(e) FALSE)

  if (is_shiny) {
    # Running in Shiny - use progress bar and sequential processing
    progress <- shiny::Progress$new(session, min = 0, max = nrow(urls_df))
    progress$set(message = "Retrieving data...", value = 0)
    on.exit(progress$close())

    WQ_data_list <- vector("list", nrow(urls_df))
    for (i in seq_len(nrow(urls_df))) {
      progress$set(detail = paste("Processing", urls_df$data_type[i]), value = i)
      WQ_data_list[[i]] <- process_urls_optimized(
        urls_df$indv_url[i],
        urls_df$data_type[i],
        urls_df$site_code[i]
      )
    }
  } else if (requireNamespace("future", quietly = TRUE) && requireNamespace("furrr", quietly = TRUE)) {
    # Not in Shiny - use parallel processing if available
    future::plan(future::multisession, workers = min(4, nrow(urls_df)))
    WQ_data_list <- furrr::future_pmap(
      list(urls_df$indv_url, urls_df$data_type, urls_df$site_code),
      process_urls_optimized,
      .options = furrr::furrr_options(seed = TRUE)
    )
    future::plan(future::sequential)
  } else {
    # Sequential processing
    WQ_data_list <- pmap(
      list(urls_df$indv_url, urls_df$data_type, urls_df$site_code),
      process_urls_optimized
    )
  }

  # Combine results
  WQ_data_list <- WQ_data_list%>%
    #remove blank lists
    compact()

  if (length(WQ_data_list) == 0) {
    return(data.frame())
  }

  # Apply final filters
  WQ_data <- bind_rows(WQ_data_list) %>%
    mutate(DT_round_MT = with_tz(DT_round_utc, tz = "America/Denver")) %>%
    #filter to time window selected
    filter(between(DT_round_MT, start_datetime, end_datetime)) %>%
    #check to make sure the data is after the testing dates
    filter(DT_round_MT > deploy_date)%>%
    mutate(units = case_when(
      parameter == "Temperature" ~ "C",
      parameter == "Specific Conductivity" ~ "uS/cm",
      parameter == "Turbidity" ~ "NTU",
      parameter == "DO" ~ "mg/L",
      parameter == "pH" ~ "pH",
      parameter == "ORP" ~ "mV",
      parameter == "Chl-a Fluorescence" ~ "RFU",
      parameter == "FDOM Fluorescence" ~ "RFU",
      parameter == "Depth" ~ "ft",
      parameter == "Stage" ~ "ft",
      TRUE ~ NA_character_),
      #replace any NaNs in value with NA
      value = ifelse(is.nan(value), NA, value))%>%
    filter(!is.na(value))

  return(WQ_data)
}

