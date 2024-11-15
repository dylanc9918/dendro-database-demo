my_ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  # Application Title
  titlePanel(
    h1("Dendro-sites Database", align = "center"),
    windowTitle = "Dendro-sites Database"
  ),
  tree_table_module_ui("tree_table")
)
