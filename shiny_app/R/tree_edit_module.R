#' Tree Add & Edit Module
#'
#' Module to add & edit tree site in the tree database
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param tree_to_edit reactive returning a 1 row data frame of the tree site to edit
#' from the "mt_cars" table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
tree_edit_module <- function(input, output, session, modal_title, tree_to_edit, modal_trigger) {
  ns <- session$ns
  states_provinces <- c("Alabama", "Alaska", "Alberta", "Arizona", "Arkansas", "British Columbia", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Manitoba", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Brunswick", "New Hampshire", "New Jersey", "New Mexico", "New York", "Newfoundland and Labrador", "North Carolina", "North Dakota", "Northwest Territories", "Nova Scotia", "Nunavut", "Ohio", "Oklahoma", "Ontario", "Oregon", "Pennsylvania", "Prince Edward Island", "Quebec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Yukon")

  observeEvent(modal_trigger(), {
    hold <- tree_to_edit()

    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("site_name"),
              "Site Name",
              value = ifelse(is.null(hold), "", hold$site_name)
            ),
            textInput(
              ns("code"),
              "Site Code",
              placeholder = "No spaces",
              value = ifelse(is.null(hold), "", hold$code)
            ),
            textInput(
              ns("drainage_basin"),
              "Drainage Basin",
              value = ifelse(is.null(hold), "", hold$drainage_basin)
            ),
            selectizeInput(
              ns("prov_terr_state"),
              "Prov/Terr/State",
              choices = states_provinces,
              options = list(placeholder = "Please select...", onInitialize = I('function() { this.setValue(""); }')),
              selected = ifelse(is.null(hold), "", hold$prov_terr_state)
            ),
            textInput(
              ns("species_name"),
              "Species Name",
              value = ifelse(is.null(hold), "", hold$species_name)
            ),
            numericInput(
              ns("lat"),
              "Lattitude",
              value = ifelse(is.null(hold), "", hold$lat),
              min = 0,
              step = 0.01
            ),
            numericInput(
              ns("long"),
              "Longitude",
              value = ifelse(is.null(hold), "", hold$long),
              min = 0,
              step = 0.01
            )
          ),
          column(
            width = 6,
            numericInput(
              ns("elevation_m"),
              "Elevation(meters)",
              value = ifelse(is.null(hold), "", hold$elevation_m),
              min = 0,
              step = 10
            ),
            selectInput(
              ns("status"),
              "Status",
              selected = ifelse(is.null(hold), "", hold$status),
              choices = c("Finished", "Not Finished")
            ),
            selectInput(
              ns("measuring_system"),
              "Measuring System",
              selected = ifelse(is.null(hold), "", hold$measuring_system),
              choices = c("V", "W")
            ),
            textInput(
              ns("lab_location"),
              "Lab Location",
              value = ifelse(is.null(hold), "", hold$lab_location)
            ),
            textInput(
              ns("notes"),
              "Notes",
              value = ifelse(is.null(hold), "", hold$notes)
            ),
            dateInput(
              ns("date_col"),
              "Date Collected"
            )
          )
        ),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("submit"),
            "Submit",
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )

    # Observe event for "code" text input in Add/Edit Tree Modal
    # `shinyFeedback`

    # Function to check if the code exists in the database
    code_exist <- function(code) {
      # SQL query to check if the code exists
      query <- glue_sql("SELECT COUNT(*) as count FROM {`config$tbl_name`} WHERE code = {code}", .con = conn)


      # Execute the query
      result <- dbGetQuery(conn, query)

      # Extract the count from the result
      count <- as.integer(result$count)

      # Check if the count is greater than 0, meaning the code exists
      return(count > 0)
    }


    # register that a code has been entered
    observeEvent(input$code, {
      # Check for empty input first
      if (input$code == "") {
        hideFeedback("code")
        shinyFeedback::showFeedbackDanger("code", text = "Must enter code of site!")
        shinyjs::disable("submit")
        return()
      }

      # make sure code does not have spaces
      if (grepl("\\s", input$code)) {
        hideFeedback("code")
        shinyFeedback::showFeedbackDanger("code", text = "Code must not contain whitespace!")
        shinyjs::disable("submit")
        return()
      }

      # Handle the case for editing a site
      if (modal_title == "Edit Site") {
        shinyjs::disable("code")
        return()
      }

      # Check for code existance in the database
      exist_result <- code_exist(input$code)


      # show feedback if code already exists and disable feedback
      if (exist_result) {
        hideFeedback("code")
        shinyFeedback::showFeedbackDanger("code", text = "Code already exists!")
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("code")
        shinyjs::enable("submit")
      }
    })
  })

  # user modal to log change in database

  user_modal <- function() {
    modalDialog(
      title = "User Information",
      tags$h4("Who is making the change?"),
      textInput(ns("user_name"), "Name:", placeholder = "First Name Last Initial (e.g., John S)"),
      textAreaInput(ns("user_note"), "Note:", placeholder = "Enter your note here (optional)", rows = 3),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("submit_user"), "Submit",
          disabled = TRUE,
          class = "btn btn-primary",
          style = "color: white"
        )
      )
    )
  }


  # creating data row to be added to database in place of current existing row or new row
  edit_tree_dat <- reactive({
    hold <- tree_to_edit()

    out <- list(
      code = if (is.null(hold)) NA else hold$code,
      data = list(
        "site_name" = input$site_name,
        "code" = input$code,
        "drainage_basin" = input$drainage_basin,
        "prov_terr_state" = input$prov_terr_state,
        "species_name" = input$species_name,
        "lat" = input$lat,
        "long" = input$long,
        "elevation_m" = input$elevation_m,
        "status" = input$status,
        "measuring_system" = input$measuring_system,
        "lab_location" = input$lab_location,
        "notes" = input$notes,
        "date_col" = input$date_col
      )
    )


    out
  })

  # observe the submit button being clicked to add or edit row and show the log modal
  observeEvent(input$submit, {
    req(validate_edit()) # make sure the user has filled out the form correctly (no special characters)


    showModal(user_modal())
  })


  observeEvent(input$user_name, {
    if (nchar(input$user_name) > 0) {
      shinyjs::enable("submit_user")
    } else {
      shinyjs::disable("submit_user")
    }
  })


  # registering that the user has clicked submit on the user log modal so we can make this change to the database
  validate_edit <- eventReactive(input$submit, {
    dat <- edit_tree_dat()

    only_text_items <- Filter(is.character, dat$data)



    special_str <- lapply(only_text_items, function(x) { # find if there are special strings
      grepl("[^A-Za-z0-9 ]", x)
    })

    if (sum(unlist(special_str)) > 0) {
      showToast("error", "Special characters are not allowed")
      return(FALSE)
    } else {
      return(TRUE)
    }
  })

  # once the user has clicked submit on the user log modal display who has made the change and remove log modal
  observeEvent(input$submit_user, {
    user_name <- input$user_name
    removeModal()
    # You can now use the user_name variable as needed
    print(paste("User making the change:", user_name))
  })


  # log the change with a timestamp and the correct action and based on the action
  observeEvent(input$submit_user, {
    req(validate_edit())
    removeModal()
    dat <- edit_tree_dat()
    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

    if (modal_title == "Add Site") {
      action <- "add"
    } else {
      action <- "edit"
    }

    tryCatch(
      {
        if (is.na(dat$code)) { # does the code/data exist in the database already?

          # creating a new tree site
          code <- input$code

          code <- gsub("\\s+", "", code)


          query <- glue("INSERT INTO {config$tbl_name} (site_name, code, drainage_basin, prov_terr_state, species_name, lat, `long`, elevation_m, status, measuring_system, lab_location, notes, date_col) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")

          dbExecute(
            conn,
            query,
            params =
              unname(dat$data)
          )

          log_change(
            site_name = dat$data$site_name,
            code = code,
            drainage_basin = dat$data$drainage_basin,
            prov_terr_state = dat$data$prov_terr_state,
            species_name = dat$data$species_name,
            lat = dat$data$lat,
            long = dat$data$long,
            elevation_m = dat$data$elevation_m,
            status = dat$data$status,
            measuring_system = dat$data$measuring_system,
            lab_location = dat$data$lab_location,
            notes = dat$data$notes,
            date_col = dat$data$date_col,
            user = input$user_name,
            date_changed = time_now,
            action = action,
            edit_note = input$user_note
          )
        } else {
          # editing an existing tree
          query <- glue("UPDATE {config$tbl_name} SET site_name=?, code=?, drainage_basin=?, prov_terr_state=?, species_name=?, lat=?, `long` =?, elevation_m=?, status=?, measuring_system=?, lab_location=?, notes=?, date_col=? WHERE code=?")
          dbExecute(
            conn,
            query,
            params = c(
              unname(dat$data),
              list(dat$code)
            )
          )

          # log the changes made to the database
          log_change(
            site_name = dat$data$site_name,
            code = dat$code,
            drainage_basin = dat$data$drainage_basin,
            prov_terr_state = dat$data$prov_terr_state,
            species_name = dat$data$species_name,
            lat = dat$data$lat,
            long = dat$data$long,
            elevation_m = dat$data$elevation_m,
            status = dat$data$status,
            measuring_system = dat$data$measuring_system,
            lab_location = dat$data$lab_location,
            notes = dat$data$notes,
            date_col = dat$data$date_col,
            user = input$user_name,
            date_changed = time_now,
            action = action,
            edit_note = input$user_note
          )
        }

        # increment the tree trigger to update the tree table and reload the proxy table
        session$userData$tree_trigger(session$userData$tree_trigger() + 1)
        showToast("success", paste0(modal_title, " Successs"))
      },
      error = function(error) {
        msg <- paste0(modal_title, " Error")


        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      }
    )
  })
}
