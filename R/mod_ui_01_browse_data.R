#' ui_01_browse_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ui_01_browse_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # Hide temporary error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    fluidRow(
      column(1, actionButton("avant", 
                             "<< Avant",
                             style = "margin-top:25px; background-color: #E8E8E8")),
      column(2, uiOutput("select_period")),
      column(2, uiOutput("select_year")),
      column(1, actionButton("apres", 
                             "Après >>",
                             style = "margin-top:25px; background-color: #E8E8E8")),
      column(3, uiOutput("select_cafet"))),
    fluidRow(plotly::plotlyOutput("plot")),
    fluidRow(
      column(3, downloadButton("dwn_filtered", 
                               "Télécharger les données affichées")))
  )
}
    
#' ui_01_browse_data Server Functions
#'
#' @noRd 
mod_ui_01_browse_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Button after ----------------------------------------------------------
    observeEvent(input$apres, {
      period_rank <- which(periods() == input$select_period)
      if (period_rank == 5) {
        year_rank <- which(years() == input$select_year)
        if (year_rank == 1) {
          shinyalert::shinyalert("Attention",
                                 paste("Les données ne sont pas préparées
                                       pour des dates après l'année scolaire", 
                                       input$select_year, "."), 
                                 type = "error", html = TRUE)
        } else {
          new_year <- years()[year_rank - 1]
          updateSelectInput(inputId = "select_period",
                            choices = periods(),
                            selected = "Ete-Toussaint")
          updateSelectInput(inputId = "select_year",
                            choices = years(),
                            selected = new_year)
        }
      } else {
        new_period <- periods()[period_rank + 1]
        updateSelectInput(inputId = "select_period",
                          choices = periods(),
                          selected = new_period)
      }
    })
    
    # Button before ----------------------------------------------------------
    observeEvent(input$avant, {
      period_rank <- which(periods() == input$select_period)
      if (period_rank == 1) {
        year_rank <- which(years() == input$select_year)
        if (year_rank == length(years())) {
          shinyalert::shinyalert("Attention",
                                 paste("Les données ne sont pas préparées
                                       pour des dates avant l'année scolaire", 
                                       input$select_year, "."), 
                                 type = "error", html = TRUE)
        } else {
          new_year <- years()[year_rank + 1]
          updateSelectInput(inputId = "select_period",
                            choices = periods(),
                            selected = "Avril-Ete")
          updateSelectInput(inputId = "select_year",
                            choices = years(),
                            selected = new_year)
        }
      } else {
        new_period <- periods()[period_rank - 1]
        updateSelectInput(inputId = "select_period",
                          choices = periods(),
                          selected = new_period)
      }
    })
 
  })
  
  # Button select_period ----------------------------------------------
  output$select_period <- renderUI({
    selectInput("select_period", "Période inter-vacances",
                choices = periods(),
                selected = piv_last_prev()$periode)
  })

# Button select_year ---------------------------------------------------
  output$select_year <- renderUI({
    selectInput("select_year", "Année scolaire",
                choices = years(),
                selected = piv_last_prev()$annee
    )
  })
  
  # Button select_cafet  -------------------------------------------------
  output$select_cafet <- renderUI({
    selectInput("select_cafet", "Filtrer un restaurant scolaire",
                choices = cafets())
  })
  
  # View plot ---------------------------------
  output$filters <- DT::renderDataTable({
    DT::datatable(filtered_prev())
  })
  
  output$dwn_filtered <- downloadHandler(
    filename = function() {
      paste("previsions_", 
            input$select_period, "_", 
            input$select_year, "_",
            input$select_cafet, ".ods", sep="")
    },
    content = function(file) {
      readODS::write_ods(out_filtered_dt(), file)
    }
  )
}
    
## To be copied in the UI
# mod_ui_01_browse_data_ui("ui_01_browse_data_ui_1")
    
## To be copied in the server
# mod_ui_01_browse_data_server("ui_01_browse_data_ui_1")
