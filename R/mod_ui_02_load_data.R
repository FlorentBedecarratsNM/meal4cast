#' ui_02_load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ui_02_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ui_02_load_data Server Functions
#'
#' @noRd 
mod_ui_02_load_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ui_02_load_data_ui("ui_02_load_data_ui_1")
    
## To be copied in the server
# mod_ui_02_load_data_server("ui_02_load_data_ui_1")
