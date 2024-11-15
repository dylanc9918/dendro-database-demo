#' Tree Delete Module
#'
#' This module is for deleting a row's information from the tree database file
#'
#' @importFrom shiny observeEvent req showModal h3 modalDialog removeModal actionButton modalButton
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param tree_to_delete string - the model of the tree to be deleted
#' @param modal_trigger reactive trigger to open the modal (Delete button)
#'
#' @return None
#'
tree_delete_module <- function(input, output, session, modal_title, tree_to_delete, modal_trigger) {
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {
    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2(
            style = "line-height: 1.75;",
            paste0(
              'Are you sure you want to delete site "',
              tree_to_delete()$code,
              '"?'
            )
          )
        ),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("submit_delete"),
            "Delete Site",
            class = "btn-danger",
            style = "color: #fff;"
          )
        )
      )
    )
  })



  # function to log changes to the database with user and notes
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


  # observe the submit button being clicked to delete row and show the log modal
  observeEvent(input$submit_delete, {
    showModal(user_modal())
  })

  # make sure at least one letter has been entered for user name before enabling submit button
  observeEvent(input$user_name, {
    if (nchar(input$user_name) > 0) {
      shinyjs::enable("submit_user")
    } else {
      shinyjs::disable("submit_user")
    }
  })


  # one the submit button is clicked, delete the row from the database and log the change report any errors that occur
  observeEvent(input$submit_user, {
    req(tree_to_delete())

    removeModal()

    tryCatch(
      {
        code <- tree_to_delete()$code
        query <- glue("DELETE FROM {config$tbl_name} WHERE code=?")

        DBI::dbExecute(
          conn,
          query,
          params = c(code)
        )

        log_change(
          site_name = tree_to_delete()$site_name,
          code = tree_to_delete()$code,
          drainage_basin = tree_to_delete()$drainage_basin,
          prov_terr_state = tree_to_delete()$prov_terr_state,
          species_name = tree_to_delete()$species_name,
          lat = tree_to_delete()$lat,
          long = tree_to_delete()$long,
          elevation_m = tree_to_delete()$elevation_m,
          status = tree_to_delete()$status,
          measuring_system = tree_to_delete()$measuring_system,
          lab_location = tree_to_delete()$lab_location,
          notes = tree_to_delete()$notes,
          date_col = tree_to_delete()$date_col,
          user = input$user_name,
          date_changed = Sys.time(),
          action = "delete",
          edit_note = input$user_note
        )

        session$userData$tree_trigger(session$userData$tree_trigger() + 1)
        showToast("success", "Site Successfully Deleted")
      },
      error = function(error) {
        msg <- "Error Deleting Site"
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
