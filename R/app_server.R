#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Run at application startup
  mod_model_source_xgboost_server("model_source_xgboost_ui_1")
  mod_sync_s3_output_server("sync_s3_output_ui_1")
  mod_admin_list_files_server("admin_list_files_ui_1")
  
  
}
