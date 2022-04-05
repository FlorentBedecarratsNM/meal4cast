#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  width_load_widgets <- "317px"
  schoolyear_hq_start <- 2010
  schoolyear_hq_end <- 2025
  hc_years <- gen_schoolyears(schoolyear_hq_start, schoolyear_hq_end)

  navbarPage("Prévoir commandes et fréquentation", id = "tabs",
             theme = bslib::bs_theme(bootswatch = "simplex", version = 5),
             # cosmo, simplex
             ## Result visualization ----------------------------------------------------
             tabPanel("Consulter des prévisions",
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
                                                 "Télécharger les données affichées")))#☺,
                      # column(3, downloadButton("dwn_filtered", 
                      #                          "Télécharger toutes les données")))
             ),
             
             ## Import new data ------------------------------------------------------
             tabPanel("Charger des données",
                      waiter::autoWaiter(id = "available_data",
                                         html = tagList(
                                           waiter::spin_flower(),
                                           h4("Inventaire en cours, patientez 20 secondes environ...")
                                         )),
                      shinyalert::useShinyalert(),
                      sidebarLayout(
                        sidebarPanel(
                          shinyjs::inlineCSS(list(
                            ".shiny-input-container" = "margin-bottom: -1px",
                            ".btn" = "margin-bottom: 5px"
                          )),
                          # sources for icons: https://icons.getbootstrap.com/
                          h4("Importer de nouvelles données"),
                          p(strong("Commandes et fréquentation réelle"),
                            tags$button(id = "help_freqs",
                                        type = "button",
                                        class="action-button",
                                        HTML("?"))),
                          #icon("question-circle")),
                          actionButton("add_effs_opendata", "Open data",
                                       icon = icon("cloud-download-alt")),
                          actionButton("add_effs_fusion", "Application Fusion",
                                       icon = icon("hdd")),
                          fileInput("add_effs_parquet", 
                                    label = NULL,
                                    buttonLabel = "Parcourir",
                                    placeholder = "Fichier extrait de Fusion",
                                    width = width_load_widgets),
                          p(strong("Menus pour la restauration scolaire"),
                            tags$button(id = "help_menus",
                                        type = "button",
                                        class="action-button",
                                        HTML("?"))),
                          actionButton("add_menus_opendata", "Open data",
                                       icon = icon("cloud-download-alt")),
                          actionButton("add_menus_fusion", "Application Fusion",
                                       icon = icon("hdd")),
                          fileInput("add_menus_parquet", label = NULL,
                                    buttonLabel = "Parcourir",
                                    placeholder = "Fichier extrait de Fusion",
                                    width = width_load_widgets),
                          p(strong("Grèves (éducation ou restauration)"),
                            tags$button(id = "help_strikes",
                                        type = "button",
                                        class="action-button",
                                        HTML("?"))),
                          fileInput("add_strikes", label = NULL,
                                    buttonLabel = "Parcourir",
                                    placeholder = "Fichier de suivi",
                                    width = width_load_widgets),
                          p(strong("Effectifs des écoles"),
                            tags$button(id = "help_effs",
                                        type = "button",
                                        class="action-button",
                                        HTML("?"))),
                          actionButton("add_hc_od", "Open data",
                                       icon = icon("cloud-download-alt")),
                          fileInput("add_headcounts", label = NULL,
                                    buttonLabel = "Parcourir",
                                    placeholder = "Fichier sur le PC",
                                    accept = c(".xls", ".xlsx"),
                                    width = width_load_widgets),
                          selectInput("schoolyear_hc", NULL,
                                      choices = c("Préciser l'année",
                                                  hc_years),
                                      width = width_load_widgets),
                          p(strong("Vacances scolaires pour la zone B"),
                            tags$button(id = "help_holi",
                                        type = "button",
                                        class="action-button",
                                        HTML("?"))),
                          actionButton("add_vacs_od", "Open data",
                                       icon = icon("cloud-download-alt")),
                          width = 3),
                        mainPanel(actionButton("process_inventory", 
                                               "Inventorier les données disponibles"),
                                  actionButton("check_mappings", 
                                               "Vérifier les tables de correspondance"),
                                  plotOutput("available_data"))
                      )
             ),
             
             ## Model parameters --------------------------------------------------------
             tabPanel("Générer des prévisions",
                      fluidRow(
                        column(2,
                               selectInput("column_to_predict", "Variable à prévoir :",
                                           c("Fréquentation réelle" = "reel", 
                                             "Commandes par les écoles" = "prevision")),
                               dateRangeInput("daterange_forecast", "Période à prévoir :",
                                              start  = "2019-09-01",
                                              end    = "2019-12-31",
                                              min    = "2015-01-01",
                                              max    = "2025-12-31",
                                              format = "dd/mm/yyyy",
                                              separator = " - ",
                                              language = "fr",
                                              weekstart = 1),
                               br(),
                               dateInput("start_training_date", "Date de début d'apprentissage :",
                                         value =  "2015-09-01",
                                         min    = "2012-01-01",
                                         max    = "2021-12-31",
                                         format = "dd/mm/yyyy",
                                         language = "fr",
                                         weekstart = 1),
                               br(),
                               sliderInput("week_latency", "Dernières semaines à exclure pour l'apprentissage :",
                                           min = 0, max = 100, value = 10, step = 1, round = TRUE)),
                        column(3,
                               selectInput("training_type", "Algorithme de prédiction :",
                                           c("XGBoost avec intervalle de confiance" = "xgb_interval",
                                             "XGBoost simple" = "xgb"), width = "100%"),
                               sliderInput("confidence", "Niveau de confiance :",
                                           min = 0, max = 1, value = 0.9, step = 0.01),
                               br(),
                               checkboxGroupInput("model_options", "Autres options",
                                                  c("Réexécuter la préparation des données" = "preprocessing", 
                                                    "Ne pas prédire les jours sans école" = "remove_no_school", 
                                                    "Omettre les valeurs extrèmes (3 sigma)" = "remove_outliers"),
                                                  selected = c("preprocessing", "remove_no_school", "remove_outliers")),
                               br(),
                               actionButton("launch_model", "Lancer la prédiction")),
                        column(7,
                               pre(id = "console")))),
             
             
             ##  UI display of server parameters --------------------------------------------------
             tabPanel("Superviser", 
                      plotOutput("error_by_school"),
                      plotOutput("error_global"),
                      h3('Information système'),
                      "(Ces valeurs changent selon le poste ou serveur qui fait tourner l'application)",
                      hr(),
                      DT::dataTableOutput('sysinfo'),
                      br(),
                      verbatimTextOutput('which_locale'),
                      verbatimTextOutput('which_python'),
                      verbatimTextOutput('python_version'),
                      verbatimTextOutput('ret_env_var'),
                      verbatimTextOutput('venv_root'),
                      mod_admin_list_files_ui("admin_list_files_ui_1")),
             bslib::nav_item(actionButton("set_simple", "Simple"),
                             actionButton("set_advanced", "Avancé"))
  )
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
  
  # A module to list files in shiny environment
  mod_admin_list_files_server("admin_list_files_ui_1")
}

