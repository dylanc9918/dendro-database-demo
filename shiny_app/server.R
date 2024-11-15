my_server <- function(input, output, session) {
  callModule(
    tree_table_module,
    "tree_table"
  )
}
