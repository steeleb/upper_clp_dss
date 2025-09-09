#this file contains all the packages,metadata, groupings and color palettes that are used in downstream scripts

knitr::opts_chunk$set(echo = TRUE)
package_loader <- function(x) {
  if (x %in% installed.packages()) {
    suppressMessages({
      library(x, character.only = TRUE)
    })
  } else {
    suppressMessages({
      install.packages(x)
      library(x, character.only = TRUE)
    })
  }
}



invisible(
  lapply(c(
    # Date/time handling
    "zoo",
    "padr",
    # Data cleaning and utilities
    "janitor",
    "here",
    # Stats/modeling
    "RcppRoll",
    # Web scraping/data retrieval
    "httr",
    "httr2",
    "yaml",
    # Development tools
    "here",
    # Core data manipulation
    "tidyverse",
    "data.table",
    "arrow",
    "fcw.qaqc"
  ),
  package_loader)
)

# Helper function
`%nin%` <- Negate(`%in%`)
#Simple function to negate %in%
`%nin%` = Negate(`%in%`)

### ----- Meta Data ----- ###

# df for sensor metadata
sensor_meta <- tibble(
  # param sonde is the name that the sensor records
  param_sonde = c("% Saturation O₂","Actual Conductivity","Baro","Battery Level","Chl-a Fluorescence",
                  "DO","Density","Depth","External Voltage","FDOM Fluorescence","ORP","Partial Pressure O₂",
                  "Pressure","Resistivity","Salinity","Specific Conductivity", "Temperature", "Total Dissolved Solids",
                  "Turbidity","pH","pH MV"),
  # param common is an abbreviated version that is easier to type
  param_common = c("DO_sat", "actual_cond", "baro", "battery", "chla",
                   "DO", "density","depth", "voltage", "FDOM", "ORP", "Partial Pressure o2",
                   "pressure", "Resistivity", "salinity", "spec_cond", "temp", "tds",
                   "turb", "pH", "pH_mv"),
  # Param w units is used to label axes
  param_w_units = c("% Saturation O₂","Actual Conductivity (µS/cm)","Baro","Battery Level","Chl-a Fluorescence (RFU)",
                    "DO (mg/L)","Density","Depth (m)","External Voltage","FDOM Fluorescence (RFU)","ORP (mv)","Partial Pressure O₂",
                    "Pressure","Resistivity","Salinity","Specific Conductivity (µS/cm)", "Temperature (C)", "Total Dissolved Solids (mg/L)",
                    "Turbidity (NTU)","pH","pH MV (v)"),
  #key param is logical, derived parameters or parameters that are less important for WQ are F
  key_param = c(T, T,F,F,T,
                T,F,T,F,T,T,F,
                F,T,T,T,T,F,
                T,T,F))
