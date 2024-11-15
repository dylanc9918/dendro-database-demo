my_server <- function(input, output, session) {
  # Call the server function portion of the `cars_table_module.R` module file
  callModule(
    tree_table_module,
    "tree_table"
  )
}
