


hours <- reactiveVal(NULL)
hours_rows <- reactiveVal(NULL)

# data frame of all the hours
observe({
  hours_commit_trigger()

  hold_user <- user()

  out <- pool %>%
    tbl("hours2") %>%
    filter(created_by == hold_user) %>%
    collect()
  
  if (nrow(out) > 0) {
    out <- out %>%
      mutate(
        # to order by start time text string we need to make all times 4 digits (5 characters)
        start_time_temp = ifelse(nchar(start_time) == 4, paste0("0", start_time), start_time)
      ) %>%
      arrange(
        date,
        start_time_temp
      ) %>%
      select(-start_time_temp)
  }

  out <- out %>%
    select(id, date, start_time, end_time, time, client_short_name, project_name, description) %>% 
    mutate(date = as.Date(date))

  hours(out)
}, priority = -1)




# filter for just the most recent few hours.  These will be the hours
# displayed in the time tracker table.
observeEvent({
  hours()
  num_rows()
}, {
  req(hours())
  hold <- hours()
  n_rows_to_display <- num_rows()

  n_rows <- nrow(hold)

  start_row <- max(n_rows - n_rows_to_display + 1, 1)


  hold <- hold[start_row:n_rows, ]

  out <- hold

  n_rows_blank <- max(10, 20 - nrow(out))
  blank_rows <- tibble(
    id = rep(NA_integer_, n_rows_blank),
    date = rep(NA_character_, n_rows_blank) %>% as.Date(),
    start_time = rep(NA_character_, n_rows_blank),
    end_time = rep(NA_character_, n_rows_blank),
    time = rep(NA_real_, n_rows_blank),
    client_short_name = rep("", n_rows_blank),
    project_name = rep("", n_rows_blank),
    description = rep("", n_rows_blank)
  )




  out <- bind_rows(
    out,
    blank_rows
  )

  hours_rows(out)
})






# number of rows to display in handsontable
num_rows <- reactiveVal(10)
observeEvent(input$num_rows, {
  # set changed cells to null.  these will need to be recalculated

  if (input$num_rows == "All") {
    num_rows(nrow(hours()))
  } else {
    num_rows(as.numeric(input$num_rows))
  }

}, ignoreInit = TRUE)



handson_time <- reactive({
  req(hours_rows())

  hold <- hours_rows()

  hold
})



have_cells_changed <- reactiveVal(FALSE)
changed_cells <- reactiveVal(matrix(nrow = 0, ncol = 2))
observeEvent(changed_cells(), {

  if (nrow(changed_cells()) == 0 && have_cells_changed() == TRUE) {
    have_cells_changed(FALSE)
  } else if (nrow(changed_cells()) > 0 && have_cells_changed() == FALSE) {
    have_cells_changed(TRUE)
  }

}, ignoreNULL = FALSE)

observeEvent(have_cells_changed(), {
  session$sendCustomMessage(
    "have_cells_changed",
    message = have_cells_changed()
  )
})



hours_commit_trigger <- reactiveVal(0)


# TODO: validate cell entries

last_pay_period_ended <- as.Date("2019-07-16")
observeEvent(input$time_tracker_commit, {
  changed_row_nums <- changed_cells()[, 1] %>% unname()
  hold_deleted_rows <- deleted_rows()

  # get all rows that have changed
  changed_rows <- hot_to_r(input$handson_out)[changed_row_nums, ] %>% unique()
  
  #date_now <- lubridate::today(tzone = "America/New_York")

  # don't allow user to change time after payment period has ended
  if (any(changed_rows$date <= last_pay_period_ended)) {
    toastr_error("Time entry is closed for this pay period")
    return()
  }


  changed_rows <- changed_rows %>%
    mutate(
      time = diff_time(start_time, end_time)
    )

  tryCatch({

    pool::poolWithTransaction(pool, function(hold_pool) {

      for (i in seq_len(nrow(changed_rows))) {
        out <- changed_rows[i, ]
        out <- as.list(out)

        out$date <- as.character(out$date)
        # set NA values to NULL so they are recorded properly as null values in DB
        for (j in length(out):1) {
          out[[j]] <- if (is.na(out[[j]])) NULL else out[[j]]
        }




        time_now <- with_tz(Sys.time(), tzone = "EST")
        hold_user <- user()

        if (is.null(out$id)) {
          # this is a new row.  add it to hours table if it has a valid time
          out$created_by <- hold_user
          out$time_created <- time_now
          out$modified_by <- hold_user
          out$time_modified <- time_now

          # create preliminary value for "billable_hourly" column
          if (is.null(out$client_short_name)) {
            out$billable_hourly <- FALSE
          } else {
            if (out$project_name %in% c("", "admin", "marketing")) {
              out$billable_hourly <- FALSE
            } else {
              out$billable_hourly <- TRUE
            }
          }


          if (is.null(out[["time"]])) {

            stop("hours_edit_no_time_error")
          } else {
            rows_affected <- tychobratools::add_row(hold_pool, "hours2", out)
          }


        } else {
          out$modified_by <- hold_user
          out$time_modified <- time_now

          rows_affected <- update_by(
            hold_pool,
            "hours2",
            by = c("id" = out$id),
            .dat = out[names(out) != "id"]
          )
        }

      }

      # delete any rows that have been removed
      if (!is.null(hold_deleted_rows)) {
        for (i in seq_along(hold_deleted_rows)) {

          delete_by(hold_pool, "hours2", by = c("id" = hold_deleted_rows[i]))

        }
      }
    })

    toastr_success("Hours Successfully Updated")
    hours_commit_trigger(hours_commit_trigger() + 1)

  }, error = function(e) {
    if (e[[1]] == "hours_edit_no_time_error") {
      toastr_error("Incorrect Time Format")
    } else {
      toastr_error("Database Connection Error")
    }


    print(list("error inserting row into hours table" = e))
  })

})

deleted_rows <- reactiveVal(NULL)
# compare the `handson_out` table returned with `input$handson_out` with the `handson_time()` (the reactive
# returning a data frame of the hours before changes made in the handsontable).  If a cell contains a change,
# then change the background of the cell to pink and allow the user to either commit the changes to the data
# base, or discard the changes
observeEvent(input$handson_out, {
  req(handson_time(), input$handson_out)
  original <- handson_time()
  edited <- hot_to_r(input$handson_out)

  # check for deleted rows
  original_rows <- original$id
  hold_deleted_rows <- original_rows[!(original_rows %in% edited$id)]

  # if any rows have been deleted, remove them from original rows before the check
  # and update `deleted_rows()` reactiveVal
  if (length(hold_deleted_rows) > 0) {
    original <- original %>%
      dplyr::filter(!(.data$id %in% hold_deleted_rows))
    deleted_rows(hold_deleted_rows)
  } else {
    deleted_rows(NULL)
  }
  
  n_rows_blank <- nrow(original) - nrow(edited)
  
  blank_rows <- tibble(
    id = rep(NA_integer_, n_rows_blank),
    date = rep(NA_character_, n_rows_blank) %>% as.Date(),
    start_time = rep(NA_character_, n_rows_blank),
    end_time = rep(NA_character_, n_rows_blank),
    time = rep(NA_real_, n_rows_blank),
    client_short_name = rep("", n_rows_blank),
    project_name = rep("", n_rows_blank),
    description = rep("", n_rows_blank)
  )
  
  edited <- rbind(edited, blank_rows)
  
  n_rows <- nrow(edited)
  n_cols <- length(edited)
  changed_cells <- matrix(NA, ncol = n_cols, nrow = n_rows)

  # compare the original data to the edited data
  for (i in seq_len(n_cols)) {
    # can't compare NA calls so set any NA cells to "<NA>"
    original[, i] <- ifelse(is.na(original[[i]]), "<NA>", original[[i]])
    edited[, i] <- ifelse(is.na(edited[[i]]), "<NA>", edited[[i]])

    changed_cells[, i] <- original[[i]] != edited[[i]]
  }
  changed_cell_indices <- which(changed_cells, arr.ind = TRUE)
  changed_cells(changed_cell_indices)

  # convert from column wise to row wise indexing for use in JS
  if (length(changed_cell_indices) > 0 || !is.null(deleted_rows())) {
    enable("time_tracker_commit")
    enable("time_tracker_discard")

    #changed_cell_indices_out <- changed_cell_indices[, 1] * n_cols + changed_cell_indices[, 2] - n_cols
    #changed_cell_indices_out <- unname(changed_cell_indices_out)

    #for (i in seq_along(changed_cell_indices_out)) {
    #  session$sendCustomMessage(
    #    "cell_background",
    #    message = list(
    #      cell = changed_cell_indices_out[i]
    #    )
    #  )
    #}
  } else {
    disable("time_tracker_commit")
    disable("time_tracker_discard")
  }


}, ignoreInit = TRUE)


output$handson_out <- renderRHandsontable({
  hold <- handson_time()

  input$time_tracker_discard

  out <- rhandsontable(
    data = hold,
    colHeaders = c(
      "ID",
      "Date",
      "Start",
      "End",
      "Hours",
      "Client",
      "Project",
      "Description"
    ),
    scrollH = 'auto',
    scrollV = 'auto',
    stretchH = 'last',
    height = 500,
    hiddenColumns = list(
      columns = c(3, 5),
      indicators = TRUE
    ),
    colWidths = c(0.1, 100, 100, 100, 100, 100, 150, 500)
    #manualColumnResize = TRUE
  ) %>%
    hot_cols(multiColumnSorting = TRUE) %>%
    hot_col("Hours", readOnly = TRUE) %>%
    hot_col("ID", readOnly = TRUE) %>%
    hot_col("Description", wordWrap = FALSE) %>%
    # Adding dropdown for Project & Client columns
    hot_col("Client", type = "dropdown", source = clients()) %>%
    hot_col("Project", type = "dropdown", source = project_names())
    # TO DO:
    #   1) Check on updating Project selection after picking Client selection from dropdown

    #hot_cols(renderer = changed_cell_state)
    # these are slow
    #hot_col(
    #  "Start",
    #  validator = time_validator,
    #  allowInvalid = TRUE
    #) %>%
    #hot_col(
    #  "End",
    #  validator = time_validator,
    #  allowInvalid = FALSE
    #)

  out$x$contextMenu$items <- list(
    "remove_row"
  )

  out
})

# DT table to summarize hours
hours_month <- reactive({
  req(hours())
  # only looking at current month
  date_now <- lubridate::today(tz = "America/New_York")
  hours() %>%
    filter(
      date >= as.Date(paste0(year(date_now), "-", month(date_now), "-01")),
      date <= lubridate::today(tz = "America/New_York")
    )
})

hours_month_total <- reactive({
  hours_month()$time %>% sum()
})

hours_week_total <- reactive({
  hold <- hours() %>%
    mutate(dow = weekdays(date))

  date_now <- lubridate::today(tz = "America/New_York")
  past_7_days <- seq(from = date_now - 6, to = date_now, by = "day")

  last_monday_date <- past_7_days[weekdays(past_7_days) == "Monday"]

  mondays <- hold %>%
    filter(dow == "Monday")

  hold %>%
    filter(date >= last_monday_date) %>%
    pull(time) %>%
    sum()
})

hours_today_total <- reactive({
  hours_month() %>%
    # convert system time to EST to get the current date EST
    filter(date == lubridate::today(tz = "America/New_York")) %>%
    pull(time) %>%
    sum()
})




hours_summary_prep <- reactive({
  tibble(
    name = c(
      "today",
      "week",
      "month"
    ),
    value = c(
      hours_today_total(),
      hours_week_total(),
      hours_month_total()
    )
  )
})

output$hours_summary <- renderDT({
  datatable(
    hours_summary_prep(),
    class = "cell-border stripe compact",
    colnames = c("Hours", ""),
    rownames = FALSE,
    options = list(
      dom = "t",
      ordering = FALSE,
      columnDefs = list(
        list(targets = 0:1, width = "100")
      )
    )
  ) %>%
    formatRound(
      2,
      digits = 2
    )
})

output$project_codes <- renderDT({
  req(hours_summary_prep())
  categories <- tibble(
    "Non Billable Codes" = c("marketing", "admin", "research")
  )

  datatable(
    categories,
    class = "cell-border stripe compact",
    rownames = FALSE,
    options = list(
      dom = "t",
      ordering = FALSE,
      columnDefs = list(
        list(targets = 0, width = "150")
      )
    )
  )
})


