#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  navbarPage("Prévoir commandes et fréquentation", id = "tabs",
             theme = bslib::bs_theme(bootswatch = "simplex", version = 5),
             tabPanel("Consulter des prévisions",
                      mod_ui_01_browse_data_ui("ui_01_browse_data_ui_1")),
             tabPanel("Charger des données",
                      mod_ui_02_load_data_ui("ui_02_load_data_ui_1")),
             tabPanel("Générer des prévisions",
                      mod_ui_03_run_model_ui("ui_03_run_model_ui_1")),
             tabPanel("Superviser", 
                      mod_ui_04_monitor_ui("ui_04_monitor_ui_1")),
             bslib::nav_item(actionButton("set_simple", "Simple"),
                             actionButton("set_advanced", "Avancé")))
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'meal4cast'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

