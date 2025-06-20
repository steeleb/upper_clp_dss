

pull_contrail_api <- function(start_DT, end_DT, username, password) {

  # Create session file for persistent cookies
  session_file <- tempfile()

  # Step 1: Get the login page and extract CSRF token
  login_page_req <- request("https://contrail.fcgov.com/login/?status=300&message=Redirection:%20Multiple%20Choices&continue=ZA") |>
    req_cookie_preserve(session_file)

  login_page_resp <- req_perform(login_page_req)

  # Extract CSRF token
  csrf_token <- login_page_resp |>
    resp_body_html() |>
    html_node("input[name='csrf_token']") |>
    html_attr("value")

  # Step 2: Login with credentials
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
    message("Login successful!")
  } else {
    stop("Login failed - check credentials")
  }

  start_DT_encoded <- format(start_DT, "%Y-%m-%d 20%H:%M:%S")%>%
    gsub(" ", "%", .)


  end_DT_encoded <- format(end_DT, "%Y-%m-%d 20%H:%M:%S")%>%
    gsub(" ", "%", .)

  # Read metadata and build URLs
  metas <- read_csv( paste0(file.path(getwd(),'creds', 'contrail_device_urls.csv')),
                    show_col_types = FALSE) %>%
    mutate(encoded_url = paste0("https://contrail.fcgov.com/export/file/?site_id=",site_id, "&site=",site , "&device_id=", device_id, "&mode=&hours=&data_start=",
                                start_DT_encoded, "&data_end=", end_DT_encoded, "&tz=US%2FMountain&format_datetime=%25Y-%25m-%25d+%25H%3A%25i%3A%25S&mime=txt&delimiter=comma"
                                ))

  # Download function for individual URLs
  download_data <- function(encoded_url, parameter, site_code) {
    tryCatch({

      download_req <- request(encoded_url)%>%
        req_cookie_preserve(session_file)

      download_resp <- req_perform(download_req)

      #if URL is parsed correctly, try to grab the data
      if(resp_status(download_resp) == 200) {
        # Save the CSV data
        csv_content <- resp_body_string(download_resp)
        parsed_data <- read_csv(csv_content, show_col_types = FALSE)%>%
          mutate(parameter = parameter,
                 site_code = site_code)

        #message("Downloaded: ", filename)
        return(parsed_data)

      } else {
        message("Download failed for: ", encoded_url)
        return(NULL)
      }

    }, error = function(e) {
      message("Error downloading from: ", encoded_url, " - ", e$message)
      return(NULL)
    })
  }

  # Process all URLs
  results <- pmap(list(metas$encoded_url, metas$parameter, metas$site_code), download_data)

  return(results)
}



