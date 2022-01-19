#' admin_list_files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_list_files_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Print the existing files
    verbatimTextOutput(ns("file_content"))
  )
}
    
#' admin_list_files Server Functions
#'
#' @noRd 
mod_admin_list_files_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # List files
    output$file_content <- renderPrint(fs::dir_tree())
  })
}
    
## To be copied in the UI
# mod_admin_list_files_ui("admin_list_files_ui_1")
    
## To be copied in the server
# mod_admin_list_files_server("admin_list_files_ui_1")
