#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Start with hidden tabs
  set_ui <- reactiveValues(simple = TRUE) 
  hideTab(inputId = "tabs", target = "Superviser", session = session)
  hideTab(inputId = "tabs", target = "Générer des prévisions", session = session)
  hideTab(inputId = "tabs", target = "Charger des données", session = session)
  
  # Open advanced tabs on click
  observeEvent(input$set_advanced, {
    set_ui$simple <- FALSE
    showTab(inputId = "tabs", target = "Superviser", session = session)
    showTab(inputId = "tabs", target = "Générer des prévisions", session = session)
    showTab(inputId = "tabs", target = "Charger des données", session = session)
  }, ignoreInit = TRUE)
  
  # Close advanced tabs on click
  observeEvent(input$set_simple, {
    set_ui$simple <- TRUE
    hideTab(inputId = "tabs", target = "Superviser", session = session)
    hideTab(inputId = "tabs", target = "Générer des prévisions", session = session)
    hideTab(inputId = "tabs", target = "Charger des données", session = session)
    updateNavlistPanel(inputId = "tabs", session = session, selected = "Consulter des prévisions")
  }, ignoreInit = TRUE)
  
  # Run at application startup
  mod_model_fetch_xgboost_server("model_fetch_xgboost_ui_1")
  # mod_model_source_xgboost_server("model_source_xgboost_ui_1")
  # prepare_arborescence()
  mod_sync_s3_output_server("sync_s3_output_ui_1")
  create_folder("temp")
  create_folder("data")
  set_config_variables()
  mod_admin_list_files_server("admin_list_files_ui_1")
  
  
}
