#' ui_04_monitor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ui_04_monitor_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ui_04_monitor Server Functions
#'
#' @noRd 
mod_ui_04_monitor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ui_04_monitor_ui("ui_04_monitor_ui_1")
    
## To be copied in the server
# mod_ui_04_monitor_server("ui_04_monitor_ui_1")
