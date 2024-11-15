
function tree_table_module_js(ns_prefix) {
  console.log("trigger")
  $("#" + ns_prefix + "tree_table").on("click", ".delete_btn", function () {
    Shiny.setInputValue(ns_prefix + "tree_id_to_delete", this.id, { priority: "event" });
    $(this).tooltip('hide');
  });

  $("#" + ns_prefix + "tree_table").on("click", ".edit_btn", function () {
    Shiny.setInputValue(ns_prefix + "tree_id_to_edit", this.id, { priority: "event" });
    $(this).tooltip('hide');
  });
  // Add click event handler for hyperlinks
  console.log($("#" + ns_prefix + "tree_table"))

  $("#" + ns_prefix + "tree_table").on("click", ".readme_btn", function () {
    Shiny.setInputValue(ns_prefix + "hyperlink_click", this.id, { priority: "event" });
  });

}

