#' Tree History Module Server
#'
#' The server portion of the module for displaying the version history datatable.
#'
#' @importFrom shiny moduleServer observeEvent showModal modalDialog div modalButton
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#'
#' @param id The id for this module.
#' @param data A reactive expression representing the data to be displayed in the datatable.
#' @param modal_trigger A reactive expression that triggers the modal dialog.
#'
#' @return None


tree_history_server <- function(id, data, modal_trigger) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(modal_trigger(), {
            showModal(
                modalDialog(
                    div(
                        style = "overflow-x: auto;",
                        class = "text-center",
                        DT::dataTableOutput(ns("history_table"))
                    ),
                    title = "Version History",
                    size = "l",
                    footer = list(
                        modalButton("Close")
                    )
                )
            )
        })



        # rendering the datatable for the version history from the audit table
        output$history_table <- DT::renderDataTable({
            query <- glue("SELECT * FROM {config$audit} ORDER BY date_changed DESC")
            dt <- dbGetQuery(conn, query)

            dt <- dt[, c(
                "user", "date_changed", "action", "edit_note",
                "site_name", "code", "drainage_basin", "prov_terr_state",
                "species_name", "lat", "long", "elevation_m", "status",
                "measuring_system", "lab_location", "notes", "date_col"
            )]

            dt$date_changed <- format(as.POSIXct(dt$date_changed, format = "%Y-%m-%d %H:%M:%S"), "%b %d, %Y %I:%M %p")


            dt$date_only <- format(as.Date(dt$date_changed, format = "%b %d, %Y %I:%M %p"), "%b %d, %Y")


            dt <- cbind(" " = "&#9654;", dt)


            datatable(dt,
                rownames = FALSE,
                escape = -1,
                colnames = c(
                    "User", "Date Changed", "Action", "Editor Note",
                    "Site Name", "Code", "Drainage Basin", "Province/Territory/State",
                    "Species Name", "Latitude", "Longitude", "Elevation (m)", "Status",
                    "Measuring System", "Lab Location", "Notes", "Date Collected", "date_only"
                ),
                extensions = c(
                    "RowGroup"
                ),
                options = list(
                    rowGroup = list(dataSrc = 18),
                    dom = "frtip",
                    columnDefs = list(
                        list(
                            visible = FALSE,
                            targets = c(6:18)
                        ),
                        list(className = "dt-center", targets = "_all"),
                        list(
                            targets = 0,
                            className = "details-control",
                            orderable = FALSE,
                            defaultContent = "&#9654;"
                        )
                    )
                ),
                callback = JS(
                    "table.column(0).nodes().to$().css({cursor: 'pointer'});
        var format = function(d) {
          return '<div style=\"background-color:#eee; padding: .5em;\">' +
               '<b>Site Code:</b> ' + d[6] + '<br>' +
                              '<b>Drainage Basin:</b> ' + d[7] + '<br>' +
                              '<b>Province/Territory/State:</b> ' + d[8] + '<br>' +
                              '<b>Species Name:</b> ' + d[9] + '<br>' +
                              '<b>Latitude:</b> ' + d[10] + '<br>' +
                              '<b>Longitude:</b> ' + d[11] + '<br>' +
                              '<b>Elevation (m):</b> ' + d[12] + '<br>' +
                              '<b>Status:</b> ' + d[13] + '<br>' +
                              '<b>Measuring System:</b> ' + d[14] + '<br>' +
                              '<b>Lab Location:</b> ' + d[15] + '<br>' +
                              '<b>Notes:</b> ' + d[16] + '<br>' +
                              '<b>Date Collected:</b> ' + d[17] + '<br>' +
                              '</div>';
        };
        table.on('click', 'td.details-control', function() {
          var td = $(this), row = table.row(td.closest('tr'));
          if (row.child.isShown()) {
            row.child.hide();
            td.html('&#9654;');
          } else {
            row.child(format(row.data())).show();
            td.html('&#9660;');
          }
        });
      "
                )
            ) %>%
                formatStyle(
                    "action",
                    target = "row",
                    backgroundColor = styleEqual(
                        c("edit", "add", "delete"),
                        c("lightblue", "lightgreen", "lightcoral") # Add more colors if needed
                    )
                )
        })
    })
}
