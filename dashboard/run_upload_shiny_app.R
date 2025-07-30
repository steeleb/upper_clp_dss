library(shiny)
# Run the application
shinyAppDir("dashboard/")




#Upload app to GeoSpatial Centroid Account

rsconnect::deployApp(appDir = "dashboard/", account = "geocentroid", appName = "UCLP_WQ_Dashboard")
