library(tidyverse)
library(httr)
library(rvest)
library(tidyr)
library(arrow)


#Set ENVIRO timezone
#?Sys.setlocale()

`%nin%` = Negate(`%in%`)

source("dashboard/src/retrieve_WET_api_data.R")

#get all 2025 data to start
sites <- c("sfm", "chd", "pfal")

new_data <- map_dfr(sites,
                     ~retrieve_WET_api_data(
                       site_code = .x,
                       start_date = Sys.time()-hours(12),
                       end_date = Sys.time(),
                       data_type = "all"))

#grab archived dataset
old_data <- read_parquet("data_backup/upper_CLP_WQ_data_2025.parquet")%>%
  mutate(DT_round = with_tz(DT_round, tzone = "America/Denver"))


#join archive dataset with new dataset
all_data <- rbind(old_data, new_data)%>%
  dplyr::distinct() # remove any duplicates


#write to parquet for smaller file size
write_parquet(all_data, "data_backup/upper_CLP_WQ_data_2025.parquet")

