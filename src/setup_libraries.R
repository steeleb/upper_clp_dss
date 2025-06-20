#this file contains all the packages,metadata, groupings and color palettes that are used in downstream scripts

### ----- Load packages ----- ###
package_load <- function(package_names){
  for(i in 1:length(package_names)){
    if(!package_names[i] %in% installed.packages()){
      install.packages(package_names[i])
    }
    library(package_names[i],character.only = TRUE)
  }
}

#vector of packages
pack_req <- c(
  # data wrangling packages
  "tidyverse","lubridate","padr","janitor","padr", "broom","arrow","zoo",
  #spatial packages
  "sf","terra","nhdplusTools", "tigris","raster", "leaflet","tmap",
  # plotting
  "ggpubr","ggthemes","scales","corrplot","gghighlight","patchwork", "geomtextpath", "ggbeeswarm","plotly", "ggpmisc","flextable",
  # web scrapping
  "rjson", "rvest", "dataRetrieval", "httr", "jsonlite", "RSelenium",
  #extra
  "devtools", "trend")
package_load(pack_req)



rm(pack_req, package_load)
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

### ----- site and date groupings ----- ###


### ----- Color Sets ----- ###

#cbbPalette <- c( "#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Seasonal Color values
#season_color_vals =c('#047E82','#397534','#59B851','#DEB907','#FA850E')
require(tidyverse)
require(ggthemes)

# basic theme for all ggplots, if Roboto is not installed, just use default, but message
if ({
  require(systemfonts)
  ("Roboto" %in% system_fonts()$family)
}) {
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold', family = "Roboto"),
          plot.subtitle = element_text(hjust = 0.5, family = "Roboto"))
} else {
  message("You do not have the Roboto font family installed on your computer, currenly using ggplot default text family.
          See ROSS_themes.R for directions to install the font family on your computer.")
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5))
}

ROSS_lt_pal <- c("#002EA3", "#E70870", "#256BF5",
                 "#745CFB", "#1E4D2B", "#56104E")

ROSS_dk_pal <- c("#1E4D2B", "#E70870", "#256BF5",
                 "#FFCA3A", "#745CFB", "#C3F73A")

ROSS_acc_pal <- c("#56104E", "#745CFB", "#FFCA3A", "#1E4D2B")

