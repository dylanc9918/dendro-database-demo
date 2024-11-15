#' Tree Table Module UI
#'
#' The UI portion of the module for displaying the site datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
tree_table_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 2,
        actionButton(
          ns("add_tree"),
          "Add",
          class = "btn-success",
          icon = icon("plus"),
          width = "100%"
        )
      ),
      verbatimTextOutput("link_click"),
      column(
        width = 2,
        actionButton(
          ns("history"),
          "History",
          class = "btn-primary",
          style = "color: #fff;",
          icon = icon("history"),
          width = "100%"
        )
      )
    ),
    tags$br(),
    tags$br(),
    fluidRow(
      column(
        width = 12,
        title = "Dendro-sites Database",
        dataTableOutput(ns("tree_table")) %>%
          withSpinner(),
        tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "tree_table_module.js"),
    tags$script(paste0("tree_table_module_js('", ns(""), "')"))
  )
}

#' Tree Table Module Server
#'
#' The Server portion of the module for displaying the site datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

tree_table_module <- function(input, output, session) {
  # trigger to reload data from the  table
  session$userData$tree_trigger <- reactiveVal(0)

  # Read in "site" table from the database
  tree <- reactive({
    session$userData$tree_trigger()

    out <- NULL
    tryCatch(
      {
        out <- conn %>%
          tbl(config$tbl_name) %>%
          collect() %>%
          mutate(
            date_col = as.Date(date_col)
          )
      },
      error = function(err) {
        msg <- "Database Connection Error"
        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        shinyFeedback::showToast("error", msg)
      }
    )

    out
  })



  ### Function that finds all the READ ME files in the folders
  find_read_files <- function(folders) {
    list_folders <- list.dirs(folders, full.names = TRUE, recursive = FALSE)


    # Initialize an empty list to store matching file paths
    matching_files <- list()

    # Loop through each folder in the list
    for (folder in list_folders) {
      # List all text files in the folder
      files <- list.files(folder, pattern = "\\.txt$", full.names = TRUE)

      # Loop through each file and check if "READ" is in the file name
      for (file in files) {
        if (grepl("READ", basename(file), ignore.case = TRUE)) {
          # Add the matching file path to the list
          matching_files <- c(matching_files, file)
        }
      }
    }

    # Return the list of matching file paths
    return(matching_files)
  }





  tree_table_prep <- reactiveVal(NULL)

  # observeevent that triggers when tree data has been loaded and then creates edit,delete, and readme buttons for each unique id.
  observeEvent(tree(), {
    out <- tree()

    ids <- out$code

    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 120px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button><button class="btn btn-info btn-sm readme_btn" data-toggle="tooltip" data-placement="top" title="ReadMe" id=', id_, ' style="margin: 0"><i class="fa fa-book"></i></button>

        </div>'
      )
    })



    # Set the Action Buttons row to the first column of the `site` table
    out <- cbind(
      tibble(" " = actions),
      out
    )


    if (is.null(tree_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      tree_table_prep(out)
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(tree_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
  })


  ## takes any GET response and finds the files names that are returned in the folder selected
  file_extractor <- function(response) {
    content <- content(response)


    files <- content$data$files

    unlisted <- unlist(files)

    site_code <- unlisted[names(unlisted) == "name"]
  }



  # GET request to the correct site based on selected ID from datatable
  site_link <- function(id) {
    login_resp <- GET(config$login)


    login_code <- fromJSON(content(login_resp))

    synotoken <- login_code$data$synotoken
    sid <- login_code$data$sid


    wrkdir <- paste0(config$wrkdir, "api=SYNO.FileStation.List&method=list&version=2&SynoToken=", synotoken, "&folder_path=", config$file_path, "/")

    open_file <- paste0(config$wrkdir, "api=SYNO.FileStation.Download&method=download&version=2&SynoToken=", synotoken, "&mode=open&path=", config$file_path, "/")


    response <- GET(paste0(config$wrkdir, "api=SYNO.FileStation.List&method=list&version=2&SynoToken=", synotoken, "&folder_path=", config$file_path))



    site_files <- file_extractor(response)

    site_no_zip <- subset(site_files, !grepl("\\.zip$", site_files, ignore.case = TRUE))

    site_code <- site_no_zip[grepl(id, site_no_zip)]

    api_code_list <- sapply(unname(site_code), function(x) paste0(wrkdir, x))

    dir_list <- GET(paste0(wrkdir, id))

    dir_files <- file_extractor(dir_list)

    readme_file <- dir_files[grepl("READ", dir_files, ignore.case = TRUE)]

    readme_url <- paste0(open_file, site_code, "/", readme_file)

    response <- GET(URLencode(URLdecode(readme_url)))

    content <- content(response)

    return(content)
  }

  # observeEvent that triggers when the readme button is clicked and then displays the readme file in a modal
  observeEvent(input$hyperlink_click, {
    site_code <- input$hyperlink_click
    readme <- site_link(site_code)

    readme <- gsub("\r\n", "<br>", readme)
    readme <- gsub("\n", "<br>", readme)

    if (grepl("page you are looking for is not found", readme, ignore.case = TRUE)) {
      readme <- "The <strong>README</strong> file for this site does not exist."
    }

    showModal(modalDialog(
      title = "ReadMe File",
      HTML(readme),
      easyClose = TRUE,
      footer = NULL
    ))
  })



  # render the tree table for display
  output$tree_table <- renderDataTable({
    req(tree_table_prep())
    out <- tree_table_prep()

    new_order <- c(
      " ", "site_name", "code", "drainage_basin", "prov_terr_state",
      "species_name", "lat", "long", "elevation_m",
      "status", "measuring_system", "lab_location", "date_col", "notes"
    )


    out <- out[, new_order]


    datatable(
      out,
      rownames = FALSE,
      colnames = c(
        "Site Name", "Code", "Drainage Basin", "Prov/Terr/State",
        "Species Name",
        "Latitude", "Longitude", "Elevation (meters)", "Status", "Measuring System",
        "Lab Location", "Date Collected",
        "notes"
      ),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st and last column (which has the buttons)
      escape = c(-1),
      extensions = c("Buttons", "FixedColumns"),
      options = list(
        scrollX = TRUE,
        dom = "Blftip",
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("dendrosites-", Sys.Date()),
            exportOptions = list(
              columns = 1:(length(out) - 1)
            )
          )
        ),
        columnDefs = list(
          list(targets = 0, orderable = FALSE),
          list(className = "dt-center", targets = "_all")
        ),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }"),
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100, nrow(out)),
        fixedColumns = list(leftColumns = 2)
      )
    ) %>%
      formatRound(columns = c("lat", "long"), digits = 2)
  })

  # Create a proxy for the tree table to avoid reloading of entire table when updating
  tree_table_proxy <- DT::dataTableProxy("tree_table")

  callModule(
    tree_edit_module,
    "add_tree",
    modal_title = "Add Site",
    tree_to_edit = function() NULL,
    modal_trigger = reactive({
      input$add_tree
    })
  )

  tree_to_edit <- eventReactive(input$tree_id_to_edit, {
    tree() %>%
      filter(code == input$tree_id_to_edit)
  })

  callModule(
    tree_edit_module,
    "edit_tree",
    modal_title = "Edit Site",
    tree_to_edit = tree_to_edit,
    modal_trigger = reactive({
      input$tree_id_to_edit
    })
  )

  tree_to_delete <- eventReactive(input$tree_id_to_delete, {
    out <- tree() %>%
      filter(code == input$tree_id_to_delete) %>%
      as.list()
  })

  callModule(
    tree_delete_module,
    "delete_tree",
    modal_title = "Delete Site",
    tree_to_delete = tree_to_delete,
    modal_trigger = reactive({
      input$tree_id_to_delete
    })
  )

  tree_history_server("history_table",
    data = tree(),
    modal_trigger = reactive({
      input$history
    })
  )
}
