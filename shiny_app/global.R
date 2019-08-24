library(shiny)
library(shinydashboard)
library(tychobratools) # remotes::install_github("tychobra/tychobratools")
library(lubridate)
library(dplyr)
library(RSQLite)
library(DBI)
library(pool)
library(shinyWidgets)
library(rhandsontable)
library(highcharter)
library(xts)
library(tidyr)
library(shinyjs)
library(DT)
library(shinycssloaders)
library(polished)
library(apexcharter)
library(shinytoastr)

tychobratools::hc_global_options()

# turn off scientific notation
options(scipen = 999)
options(spinner.type = 8)


Sys.setenv(R_CONFIG_ACTIVE = "default")

source("helpers/time_hours.R", local = TRUE)

# modules
source("modules/calendar_chart.R", local = TRUE)
source("modules/column_chart.R", local = TRUE)
source("modules/dash_table.R", local = TRUE)



# set up pool connection
pool <- pool::dbPool(
  RSQLite::SQLite(),
  dbname = "data/hours_db.sqlite3"
)


# disconnect pool connection when app exits
onStop(function() {
  pool::poolClose(pool)
})


# choices for grouping in dashboard
group_choices <- c(
  "Client" = "client_short_name",
  "Project" = "project_name"
)

start_date <- as.Date("2018-01-02")




valueBox2 <- function (value, subtitle, icon = NULL, backgroundColor = "#7cb5ec", color = "#FFF", width = 4, href = NULL) {

  boxContent <- div(
    class = "small-box",
    style = paste0("background-color: ", backgroundColor, "; color: ", color),
    div(
      class = "inner",
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) {
      div(class = "icon-large", icon)
    }
  )

  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}




more_colors <- c(
  # highcharts colors
  "#7cb5ec",
  "#434348",
  "#90ed7d",
  "#f7a35c",
  "#8085e9",
  "#f15c80",
  "#e4d354",
  "#2b908f",
  "#f45b5b",
  "#91e8e1",

  # from color brewer
  "#ffffd9",
  "#7fcdbb",
  "#41b6c4",
  "#1d91c0",
  "#edf8b1",
  "#225ea8",
  "#253494",
  "#c7e9b4",
  "#081d58"
)