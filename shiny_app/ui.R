header <- dashboardHeader(
  title = tychobratools::tychobra_title("T3"),
  # user profile dropdown in top right
  htmltools::tags$li(
    class = "dropdown",
    htmltools::tags$a(href = "#",
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      htmltools::tags$i(class = "fa fa-user")
    ),
    htmltools::tags$ul(
      class = "dropdown-menu",
      htmltools::tags$li(
        style = "padding: 3px 20px;",
        shiny::textOutput("authed_user")
      ),
      htmltools::tags$li(
        shiny::actionLink(
          "polished__sign_out",
          label = "Sign Out",
          icon = icon("sign-out")
        )
      )
    )
  )
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
    tags$script(src = "custom.js")
  ),
  tabItems(
    source("ui/01_ui_dashboard.R", local = TRUE)$value,
    source("ui/02_ui_time_tracker.R", local = TRUE)$value
  )
)

dashboardPage(
  header,
  sidebar,
  body,
  skin = "black"
)

