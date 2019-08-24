
#' dash_table_ui
#'
#' Shiny module ui
#'
dash_table_ui <- function(id) {
  ns <- NS(id)

  DTOutput(ns("dash_hours_table")) %>% withSpinner()
}


#' dash_table
#'
#' Shiny module server
#'
#' @param hours_gouped reactive - a data frame of hours summarized by the user selected `groups`.  The
#' data frame has either 2, 3, or 4 columns (depending on the number of `groups` selected).
#'  - year_month  - character - the year and month in "yyyy-mm" format
#'  - client_short_name - character - the short name for the client.  This column only exists if "client_short_name"
#'  is in `groups`.
#'  - project_name - character - the project name.  This column only exists if "project_name" is in `groups`.
#'  - time - numeric - the total time in grouped by the columns to the left
#'  @param groups reactive - the user selected groups. Possible values are NULL, and 1 or both of "client_short_name"
#'  and "project_name"
#'  @param date_range reactive - Date vector of length 2
#'
dash_table <- function(input, output, session, hours_grouped, groups, date_range) {
  dash_table_prep <- reactive({
    out <- hours_grouped()
    groups <- groups()


    out <- out %>%
      spread(key = year_month, value = time)


    n_cols <- length(out)

    n_group_vars <- length(groups)

    start_time_cols <- 1 + n_group_vars

    out$Total <- rowSums(out[, start_time_cols:n_cols], na.rm = TRUE)

    out %>%
      arrange(desc(Total))
  })


  # create the column headers for the table
  hours_table_headers <- reactive({
    groups <- groups()
    tbl_dat <- dash_table_prep()
    tbl_len <- length(dash_table_prep())

    if (is.null(groups)) {
      totals <- lapply(tbl_dat, function(x) {
        sum(x, na.rm = TRUE)
      })

      out <- htmltools::withTags(table(
        thead(
          tr(
            th(colspan = tbl_len, "Hours Per Month")
          ),
          tr(
            lapply(names(tbl_dat), th)
          )
        ),
        tableFooter(format(totals, big.mark = ","))
      ))
    } else if (length(groups) == 1) {
      totals <- lapply(tbl_dat[, -1], function(x) {
        sum(x, na.rm = TRUE)
      })

      out <- htmltools::withTags(table(
        thead(
          tr(
            th(rowspan = 2, names(group_choices)[group_choices == names(tbl_dat)[1]]),
            th(colspan = tbl_len - 1, "Hours Per Month")
          ),
          tr(
            lapply(names(tbl_dat)[-1], th)
          )
        ),
        tableFooter(c("Total: ", format(totals, big.mark = ",")))
      ))
    } else if (length(groups) == 2) {
      totals <- lapply(tbl_dat[, -c(1:2)], function(x) {
        sum(x, na.rm = TRUE)
      })

      out <- htmltools::withTags(
        table(
          thead(
            tr(
              th(rowspan = 2, names(group_choices)[group_choices == names(tbl_dat)[1]]),
              th(rowspan = 2, names(group_choices)[group_choices == names(tbl_dat)[2]]),
              th(colspan = tbl_len - 2, "Hours Per Month")
            ),
            tr(
              lapply(names(tbl_dat)[-c(1:2)], th)
            )
          ),
          tableFooter(c("Total: ", " ", format(totals, big.mark = ",")))
        )
      )
    }



  })

  output$dash_hours_table <- renderDT({
    out <- dash_table_prep()

    datatable(
      out,
      class = "cell-border stripe compact",
      container = hours_table_headers(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bt",
        pageLength = nrow(out),
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("hours-monthly-", Sys.Date())
          )
        ),
        scrollX = TRUE
      )
    ) %>%
      formatStyle(
        1,
        target = 'row',
        backgroundColor = styleEqual("Total", "#ccc")
      ) %>%
      formatStyle(
        "Total",
        backgroundColor = "#ccc"
      )
  })
}