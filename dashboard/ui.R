#UI

#### Start UI ####
ui <- dashboardPage(
  dashboardHeader(title = "Water Quality Monitoring Dashboard"),
  #### Define Sidebar ####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Live WQ Data", tabName = "sensor_data", icon = icon("chart-line")),
      #Placeholder for future functions
      #menuItem("WQ Forecast", tabName = "forecast", icon = icon("bolt"))
      menuItem("CLP Basin Conditions", tabName = "flow_data", icon = icon("droplet")),
      menuItem("Site Map", tabName = "map", icon = icon("map"))
      #Placeholder for future functions
      #menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  #### Define Body Styling and start tabs ####
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    # Adjusting size of progress bar
  tags$style(HTML("
  .shiny-notification-content-progress {
    font-size: 72px !important;
    padding: 60px !important;
  }
  .progress {
    height: 120px !important;
  }
  .progress-bar {
    font-size: 64px !important;
    line-height: 120px !important;
  }
")),

    tabItems(
      #### Sensor Data Tab ####
      tabItem(tabName = "sensor_data",
              fluidRow(
                box(
                  title = "Data Controls", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           dateRangeInput("date_range",
                                          label = "Select Date Range:",
                                          start = Sys.Date() - 7,
                                          end = Sys.Date(),
                                          max = Sys.Date()
                           ),
                           pickerInput("sites_select",
                                       label = "Select Sites:",
                                       choices = c("South Fork CLP", "Chambers Lake Outflow", "CLP @ Poudre Falls", "Canyon Mouth", "CLP @ Indian Meadows", "CLP @ Manners Bridge"),
                                       selected = c("South Fork CLP", "Chambers Lake Outflow", "CLP @ Poudre Falls", "Canyon Mouth", "CLP @ Indian Meadows", "CLP @ Manners Bridge"),
                                       multiple = TRUE,
                                       options = pickerOptions(
                                         actionsBox = TRUE,
                                         selectAllText = "Select All",
                                         deselectAllText = "Deselect All"
                                       )
                           )
                    ),
                    column(4,
                           pickerInput("parameters_select",
                                       label = "Select Parameters:",
                                       choices = c("Temperature", "Turbidity", "pH", "DO",
                                                   "Specific Conductivity", "Chl-a Fluorescence", "FDOM Fluorescence", "Depth"),
                                       selected = c("Temperature", "Turbidity", "pH", "Specific Conductivity"),
                                       multiple = TRUE,
                                       options = pickerOptions(
                                         actionsBox = TRUE,
                                         selectAllText = "Select All",
                                         deselectAllText = "Deselect All"
                                       )
                           ),
                           uiOutput("dynamic_load_button")
                    ),
                    column(4,
                           h4("Log Scale Controls:"),
                           br(),
                           uiOutput("log_controls"),
                           br(),
                           downloadButton("download_data", "Download Data",
                                          class = "btn-success")
                    )
                  )
                )
              ),

              fluidRow(
                box(
                  title = "Time Series Plots", status = "primary", solidHeader = TRUE, width = 12,
                  uiOutput("dynamic_plots") %>% withSpinner()
                )
              ),
              fluidRow(
                box(
                  title = "Modelled Time Series plots", status = "primary", solidHeader = TRUE, width = 12,
                  #uiOutput("dynamic_plots") %>% withSpinner()
                )
              )
      ),
      #### WQ Site Map Tab ####
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Site Locations", status = "primary", solidHeader = TRUE, width = 12,
                  leafletOutput("site_map", height = "600px") %>% withSpinner()
                )
              )
      ),
      #### Flow Data Tab ####
      tabItem(tabName = "flow_data",
              layout_columns(
                col_widths = c(7, 5),
                # 3-day Q plot card
                card(
                  card_header("Current flows (7 days) for key CLP sites"),
                  card_body(
                    plotlyOutput("three_day_q_plot")
                  )
                ),
                # Conditional SNOTEL card that will only show between October and July
                uiOutput("snotel_card")
              ),
              # Main Map Card
              card(
                card_header("Tracking Flow rates through the CLP network"),
                card_body(
                  leafletOutput("map", height = 600)
                ),
                card_footer(
                  "Data retrieved from USGS/CDWR using the cdssr package. Colors indicate flow trend in last 24 hours: red (decreasing), green (increasing), grey (no data)."
                )
              )
      )
      #### End of Tabs ####
    )
  )
)
#### End of UI ####
