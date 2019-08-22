header <- dashboardHeader(
  title = tychobratools::tychobra_title("T3")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menu",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Time Entry", tabName = "time_tracker", icon = icon("clock-o"))
  )
)

body <- dashboardBody(
  tychobratools::use_tychobra_js(),
  shinytoastr::useToastr(),
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "https://unpkg.com/sweetalert/dist/sweetalert.min.js"),
    tags$script(src = "custom.js")
  ),
  tabItems(
    tabItem(
      tabName = "dashboard",
      source("ui/01_ui_dashboard.R", local = TRUE)$value
    ),
    tabItem(
      tabName = "time_tracker",
      source("ui/02_ui_time_tracker.R", local = TRUE)$value
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body,
  skin = "black"
)
