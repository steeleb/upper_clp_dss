# Data Collation Workflow ====

# Description: Here we collect the data for all of the APIs, clean them up,
# autoqaqc them, collate them, and save them into a single parquet file that
# will be sourced by `the shiny app` for plotting the data and pulling the data.

# Set up environment ----

source("src/setup_libraries.R")

# suppress scientific notation to ensure consistent formatting
options(scipen = 999)

# Load in saved data ----

cached_data <- read_rds(here("shiny_data", "hv_data_20250401_20250904.rds"))

# TODO: Make this a parquet file
# TODO: Name data something without DTs to make loading easier
# cached_data <- arrow::read_parquet(here(""))

# Get the min max DT from all sites ----
mm_DT_hv <- cached_data %>%
  bind_rows() %>%
  filter(parameter != "ORP") %>%
  group_by(site, parameter) %>%
  summarize(max_dt = max(DT_round, na.rm = T),
            .groups = "drop") %>%
  filter(max_dt == min(max_dt, na.rm = T)) %>%
  slice(1) %>%
  pull(max_dt)

mm_DT <- mm_DT_hv

# TODO: Set the mm_DT objects up properly
# mm_DT_wet
# mm_DT_contrail
# mm_DT <- min(mm_DT_hv, mm_DT_wet, mm_DT_contrail)

# Set up dates ----
start_DT <- mm_DT
end_DT <- Sys.time() # Set the end date to now

# Pull in data ----

## Radio Telemetry Data
# TODO: This

## HydroVu Livestream Data ----
# Establishing staging directory - Replacing with temp_dir()
#staging_directory <- here("data", "api_pull", "raw")
staging_directory = tempdir()

# Read in credentials
# Get credentials from environment variables (GitHub Secrets)
# TODO: Set Up GH secrets
client_id <- Sys.getenv("HYDROVU_CLIENT_ID")
client_secret <- Sys.getenv("HYDROVU_CLIENT_SECRET")

# Check if credentials are available
if(client_id == "" || client_secret == "") {
  stop("HydroVu credentials not found. Please check GitHub Secrets.")
}

# Use credentials
hv_token <- hv_auth(client_id = client_id, client_secret = client_secret)

# Pulling in the data from hydrovu
# Making the list of sites that we need
hv_sites <- hv_locations_all(hv_token) %>%
  filter(!grepl("vulink", name, ignore.case = TRUE))%>%
  #sondes with 2024 in the name can be avoided to speed up the live data pull
  #these should be included in the historical data pull
  filter(!grepl("2024", name, ignore.case = TRUE))

# these sites are backed up on HydroVu but most do not livestream
# TODO: set up a daily CRON job that will look to see if any new data is available on HydroVu and grab it when possible?
# sites <- c( "cbri", "chd","joei",  "pbd", "pbr","pfal", "pman", "sfm")

#Canyon mouth does send data to livestream
sites <- c("pbd")

# When we are getting all the 2025 data across the network for modeling, use these sites
#sites <- c("pbd", "salyer", "udall", "riverbend", "cottonwood", "springcreek" , "elc", "boxcreek",  "archery", "riverbluffs")
source(file = "src/api_puller.R")

walk(sites,
     function(site) {
       message("Requesting HV data for: ", site)
       api_puller(
         site = site,
         network = "all",
         start_dt = with_tz(start_DT, tzone = "UTC"), # api puller needs UTC dates
         end_dt = with_tz(end_DT, tzone = "UTC"),
         api_token = hv_token,
         hv_sites_arg = hv_sites,
         dump_dir = staging_directory
       )
     }
)

# munge data from staging directory

hv_data <- list.files(staging_directory, full.names = TRUE, pattern = ".parquet") %>%
  map_dfr(function(file_path){
    site_df <- read_parquet(file_path, as_data_frame = TRUE)
    return(site_df)
  }) %>%
  #doing some clean up
  select(-id) %>%
  mutate(units = as.character(units)) %>%
  #double check that Vulink data has been removed
  filter(!grepl("vulink", name, ignore.case = TRUE)) %>%
  mutate(
    DT = timestamp, #timestamp comes in UTC
    DT_round = round_date(DT, "15 minutes"), #rounding
    #DT_round_MT = with_tz(DT_round, tzone = "America/Denver"),
    DT_join = as.character(DT_round), #keeping in UTC but character form
    site = tolower(site)) %>%
  select(-name) %>%
  distinct(.keep_all = TRUE)%>%
  split(f = list(.$site, .$parameter), sep = "-") %>%
  keep(~nrow(.) > 0)

# Clean up HV data

# Read in threshold and sensor notes

# Add field notes and summary stats

## Contrail Data ----
# TODO: This

# Collating Datasets ----
# TODO: This

# Applying AutoQAQC ----

# Single Sensor Flags

# Intrasensor Flags

# Network Check and Final Flags

# Write to new file ----
# Save as parquet file
