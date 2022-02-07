#' model_source_xgboost UI Function
#'
#' @description This shiny modules launches the python/reticulate parameters.
#' Server side only.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model_source_xgboost_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' model_source_xgboost Server Functions
#'
#' @noRd 
mod_model_source_xgboost_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Load python environment depending on context
    if (stringr::str_starts(Sys.info()[["nodename"]], "meal4cast")) { # Yes if runs over Docker/SSPCloud
      
      virtualenv_dir <- "/opt/venv"
    
      } else if (Sys.info()[['user']] == "rstudio") { # This is for SSPCloud

        virtualenv_dir = Sys.getenv("VIRTUALENV_NAME")
        python_path = Sys.getenv("PYTHON_PATH")
        
        if (!reticulate::virtualenv_exists(envname = "~/venv_shiny_app")) {
          reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
          reticulate::virtualenv_install(virtualenv_dir, packages = c("pandas==1.1.0",
                                                                      "numpy==1.19.1",
                                                                      "xgboost==1.1.1",
                                                                      "scikit-learn==0.23.1",
                                                                      "dask[dataframe]==0.19.4",
                                                                      "lunardate==0.2.0",
                                                                      "convertdate==2.2.1",
                                                                      "matplotlib==3.2.1",
                                                                      "python-dateutil==2.8.1"))
        }
        reticulate::use_virtualenv(virtualenv = virtualenv_dir, required = TRUE)
      # reticulate::virtualenv_remove(virtualenv_dir)
    } else { # otherwise for linux environment for local development
      virtualenv_dir <- "venv_shiny_app"
      reticulate::use_virtualenv(virtualenv = virtualenv_dir, required = TRUE)
    }
    reticulate::source_python("main.py")
  })

}
    
## To be copied in the UI
# mod_model_source_xgboost_ui("model_source_xgboost_ui_1")
    
## To be copied in the server
# mod_model_source_xgboost_server("model_source_xgboost_ui_1")
