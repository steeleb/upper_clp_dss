
library(here)
#get current WD and add dashboard to it
path = paste0(here(), "/dashboard")
#Set wd to dashboard to test as though in web deployment
setwd(path)
#run the app
shiny::runApp(appDir = path )
