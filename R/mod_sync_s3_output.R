#' sync_s3_output UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sync_s3_output_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sync_s3_output Server Functions
#'
#' @noRd 
mod_sync_s3_output_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    if (dir.exists("output")) {
      dir.create("output")
    }
    aws.s3::s3sync(path = "output", 
                   bucket = "fbedecarrats", 
                   prefix = "diffusion/cantines/output", 
                   region = "")
  })
}
    
## To be copied in the UI
# mod_sync_s3_output_ui("sync_s3_output_ui_1")
    
## To be copied in the server
# mod_sync_s3_output_server("sync_s3_output_ui_1")
