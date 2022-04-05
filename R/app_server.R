#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # To enable upload of large files (Parquet files from Fusion)
  options(shiny.maxRequestSize=30*1024^2)
  library(lubridate) # explicitly declared here to use '%within%' below
  
  create_folder("temp")
  create_folder("data")
  set_config_variables()
  
  mod_model_fetch_xgboost_server("model_fetch_xgboost_ui_1")
  mod_model_source_xgboost_server("model_source_xgboost_ui_1")
  # prepare_arborescence()
  sync_sspcloud(c("output", "data"))

  mod_admin_list_files_server("admin_list_files_ui_1")

  # Handle simple vs. advanced interface ------------------------------------
  
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
  
  # Reactive values for result display -----------------------------------
  
  # prev <- reactive({ load_results() }) 
  prev <- reactivePoll(5000, session, # Previsions
                       function() check_results_fresh(), 
                       function() load_results()) 
  dt <- reactivePoll(5000, session, # training data
                     function() check_traindata_fresh(), 
                     function() load_traindata()) 
  vacs <- reactive({ return(dt()$vacs) }) # vacations
  pivs <- reactive({ gen_piv(vacs()) }) # Period between vacations
  cafets <- reactive({ 
    if (any(any(is.na(prev())) | nrow(filtered_prev) == 0)) {
      list_cafets <- levels(factor(dt()$freqs$site_nom))
    } else {
      list_cafets <- levels(factor(prev()$cantine_nom))
    }
    c("Tous", list_cafets) 
  })
  periods <- reactive({ levels(pivs()$periode) }) # Name of the periods
  years <- reactive({ # School years
    levels(forcats::fct_rev(pivs()$annee)) 
  })
  
  selected_cafet <- reactive({ input$select_cafet })
  selected_dates <- reactive({  
    pivs() %>%
      dplyr::filter(periode == input$select_period & 
                      annee == input$select_year) %>%
      dplyr::select(`Début`, `Fin`)
  })
  
  filtered_prev <- reactive({ # Filtering the prevision based on parameters
    # Retreive parameters
    date_start <- lubridate::ymd(selected_dates()[[1]])
    date_end <- lubridate::ymd(selected_dates()[[2]])
    cafet <- input$select_cafet
    # Filter dates
    filtered <- prev() %>%
      dplyr::mutate(Date = lubridate::as_date(date_str),
                    Source = dplyr::case_when(variable == "reel" ~ "prevision_frequentation",
                                              variable == "prevision" ~ "prevision_commandes")) %>%
      dplyr::select(Date, site_nom = cantine_nom, Source, Repas = output) %>%
      dplyr::filter(Date >= date_start & Date <= date_end)
    # Filter cafet
    if (cafet != "Tous") {
      filtered <- filtered %>%
        dplyr::filter(site_nom == cafet)
    }
    # Summarise
    filtered <- filtered %>%
      dplyr::group_by(Date, Source) %>%
      dplyr::summarise(Repas = sum(Repas, na.rm = TRUE))
    
    return(filtered)
  })
  
  filtered_freqs <- reactive({ 
    # Tertreive_parameters
    date_start <- lubridate::ymd(selected_dates()[[1]])
    date_end <- lubridate::ymd(selected_dates()[[2]])
    cafet <- input$select_cafet
    # Gilter by fate and recode
    filtered <- dt()$freqs %>%
      dplyr::mutate(Date = lubridate::as_date(date)) %>%
      dplyr::filter(Date >= date_start & Date <= date_end) %>%
      dplyr::select(Date, site_nom, reel, prevision) %>%
      tidyr::pivot_longer(reel:prevision, names_to = "Source", values_to = "Repas") %>%
      dplyr::mutate(Source = dplyr::case_when(Source == "reel" ~ "reel_frequentation",
                                              Source == "prevision" ~ "reel_commandes"))
    # Filtering on cafeteria
    if (cafet != "Tous") {
      filtered <- filtered %>%
        dplyr::filter(site_nom == cafet)
    }
    
    # Summarise for global or per cafeteria
    filtered <- filtered  %>%
      dplyr::group_by(Date, Source) %>%
      dplyr::summarise(Repas = sum(Repas, na.rm = TRUE)) %>%
      dplyr::filter(if_any(where(is.numeric), ~ .x > 0)) 
    
    return(filtered)
    
  })
  
  filtered_dt <- reactive({
    
    # Filter parameters
    date_start <- lubridate::ymd(selected_dates()[[1]])
    date_end <- lubridate::ymd(selected_dates()[[2]])
    cafet <- input$select_cafet
    
    no_prev <- any(any(is.na(prev())) | nrow(filtered_prev()) == 0)
    no_freqs <- any(nrow(filtered_freqs()) == 0 | nrow(filtered_freqs()) == 0)
    
    # Create an empty tibble
    join_filtered <- dplyr::tibble(
      Date = lubridate::ymd(),
      site_nom = character(),
      Source = character(),
      Repas = integer())
    
    # Conditional to enable displaying only traininf data if no previsions
    if (!no_prev & no_freqs) {
      prevs <- filtered_prev() 
      join_filtered <- prevs %>%
        dplyr::bind_rows(dplyr::mutate(prevs,
                                       Source = stringr::str_replace(Source, "prevision_", "reel_"),
                                       Repas = NA))
    } else if (no_prev & !no_freqs) {
      freqs <- filtered_freqs()
      join_filtered <- freqs %>%
        dplyr::bind_rows(dplyr::mutate(freqs,
                                       Source = stringr::str_replace(Source, "reel_", "prevision_"),
                                       Repas = NA))
    } else if (!no_prev & !no_freqs) {
      join_filtered <- filtered_freqs() %>%
        dplyr::bind_rows(filtered_prev())
    }
    return(join_filtered)
  })
  
  out_filtered_dt <- reactive({
    filtered_dt() %>%
      # filtered <- filtered %>%
      dplyr::mutate(Jour = lubridate::wday(Date, label = TRUE, abbr = FALSE),
                    Date = format(Date, "%d/%m/%Y")) %>%
      dplyr::select(Date, Jour, everything()) %>%
      dplyr::filter(Jour %in% c("lundi", "mardi", "jeudi", "vendredi"))
  })
  
  last_prev <- reactive ({
    if (any(is.na(prev()))) {
      max(dt()$freqs$date)
    } else {
      max(lubridate::ymd(prev()$date_str))
    }
  })
  
  piv_last_prev <- reactive({
    pivs() %>%
      dplyr::filter(last_prev() %within% lubridate::interval(`Début`, dplyr::lead(`Début`)))
  })
  
  # Navigation - bouton "Après" ---------------------------------------------
  
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
  
  # Navigation - bouton "Avant" ---------------------------------------------
  
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
                          selected = "Printemps-Ete")
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
  
  output$select_period <- renderUI({
    selectInput("select_period", "Période inter-vacances",
                choices = periods(),
                selected = piv_last_prev()$periode)
  })
  output$select_year <- renderUI({
    selectInput("select_year", "Année scolaire",
                choices = years(),
                selected = piv_last_prev()$annee
    )
  })
  output$select_cafet <- renderUI({
    selectInput("select_cafet", "Filtrer un restaurant scolaire",
                choices = cafets())
  })
  
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
  
  ## Consult results -----------------------------------------------------
  
  
  output$plot <- plotly::renderPlotly({
    dt2 <- filtered_dt()
    
    static <- dt2 %>%
      ggplot2::ggplot(ggplot2::aes(x = Date, y = Repas, fill = Source, 
                                   color = Source, ymin = 0)) +
      ggplot2::geom_bar(data = subset(dt2, stringr::str_starts(Source, "prevision_")),
                        ggplot2::aes(x = Date, y = Repas, alpha = 0.5),
                        stat = "identity",
                        position = "dodge2") +
      ggplot2::geom_line(data = subset(dt2, stringr::str_starts(Source, "reel_commandes"))) +
      ggplot2::geom_point(data = subset(dt2, stringr::str_starts(Source, "reel_commandes"))) +
      ggplot2::geom_line(data = subset(dt2, stringr::str_starts(Source, "reel_freq"))) +
      ggplot2::geom_point(data = subset(dt2, stringr::str_starts(Source, "reel_freq"))) +
      ggplot2::theme(axis.title.x=ggplot2::element_blank(), 
                     axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5)) +
      ggplot2::scale_fill_manual(values = c("red", "green", "red", "green")) + 
      ggplot2::scale_color_manual(values = c("red", "green3", "red3", "green4")) +
      ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%a %d %b")
    
    plotly::ggplotly(static, tooltip = c("x", "y", "fill")) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(legend = list(orientation = "h", x = 0, y = 1.1))
    
  })
  
  
  ## Visualize existing data for each day-----------------------------------------
  
  
  ### Compute and format days where strike events ----------------------------
  avail_strikes <- reactive({ 
    dt()$strikes %>%
      dplyr::mutate("avail_data" = "Grèves") %>%
      dplyr::select(date, avail_data, n= greve)
  })
  
  ### Compute the number of values of staff previsions and kid attendance --- 
  avail_freqs <- reactive ({
    dt()$freqs %>%
      dplyr::select(date, prevision, reel) %>%
      tidyr::pivot_longer(cols = -date, names_to = "avail_data") %>%
      dplyr::mutate(avail_data = dplyr::recode(avail_data,
                                               prevision = "Commandes",
                                               reel = "Fréquentation")) %>%
      dplyr::group_by(date, avail_data) %>%
      dplyr::summarise(n = dplyr::n()) 
  })
  
  ### Compute the number of menu items registered per day -------------------
  avail_menus <- reactive ({
    dt()$menus %>%
      dplyr::mutate("avail_data" = "Menus",
                    date = lubridate::dmy(date)) %>%
      dplyr::group_by(date, avail_data) %>%
      dplyr::summarise(n = dplyr::n())
  })
  
  
  avail_vacs <- reactive ({
    vacs <- dt()$vacs
    purrr:::map2(vacs$date_debut, vacs$date_fin, 
                 ~ seq(.x, .y, by = "1 day")) %>%
      purrr::reduce(c) -> vacs_dates
    tidyr::tibble(
      date = vacs_dates,
      avail_data = "Vacances",
      n = 1
    )
  })
  
  avail_holidays <-reactive ({
    dt()$holidays %>%
      dplyr::mutate(avail_data = "Fériés") %>%
      dplyr::select(date, avail_data, n = jour_ferie)
  })
  
  ### Plot available data ---------------------------------------------------
  observeEvent(input$process_inventory, {
    output$available_data <- renderPlot({
      dt_act <- dt()
      compute_availability(x = dt_act) %>%
        ggplot2::ggplot(ggplot2::aes(x = `Jour`, y = avail_data)) +
        ggplot2::geom_tile(ggplot2::aes(fill = avail_data,
                                        alpha = nday_vs_nyearmax)) +
        ggplot2::scale_alpha(guide = "none") +
        ggplot2::scale_fill_discrete("") +
        ggplot2::facet_grid(forcats::fct_rev(an_scol) ~ ., # fct_rev to have recent first
                            switch = "both") + 
        ggplot2::scale_x_date(labels = function(x) format(x, "%b"),
                              date_breaks = "1 month", date_minor_breaks = "1 month",
                              position = "top") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(hjust = 0),
                       # axis.text.y = ggplot2::element_blank(),
                       legend.position = "none")+
        ggplot2::ggtitle("Données déjà chargées dans l'outil")
      
    }, height = 600)
  })
  
  ## Import new data ----------------------------------------------------------
  
  ### Help --------------------------------------------------------------
  observeEvent(input$help_freqs, {
    shinyalert::shinyalert("Import de données de fréquentation", 
                           "Ces données peuvent être importées de plusieurs manières :
                               - en allant récupérer les inormations les plus récentes sur l'open data
                               - en changeant les données brutes extraites d'un sauvegarde de la base de données de Fusion
                               - en se connectation directement à l'outil Fusion (uniquement lorsque l'application fonctionne en local sur un PC configuré pour avoir accès à Fusion)", 
                           type = "info")
  }) 
  observeEvent(input$help_menus, {
    shinyalert::shinyalert("Import de données de menus", 
                           "Ces données peuvent être importées de plusieurs manières :
                               - en allant récupérer les inormations les plus récentes sur l'open data
                               - en changeant les données brutes extraites d'un sauvegarde de la base de données de Fusion
                               - en se connectation directement à l'outil Fusion (uniquement lorsque l'application fonctionne en local sur un PC configuré pour avoir accès à Fusion)", 
                           type = "info")
  })
  observeEvent(input$help_strikes, {
    shinyalert::shinyalert("Import de données de grèves", 
                           paste("Ces données doivent reconstruites à partir des fichiers de suivi des grêves",
                                 "de la direction de l'éducation. Il suffit de construire un tableau avec, dans",
                                 "une première colonne nommée 'date' la date des grêves de l'éducation ou de la", 
                                 "restauration ayant fait l'objet d'un préavis à Nantes Métropole, et une colonne", 
                                 "nommée 'greve' indiquant des 1 pour chaque date ayant connu une grève. Pour des",
                                 "exemples, Voir le fichier readme ou le fichier tests/data/calculators/greves.csv"), 
                           type = "info")
  })
  observeEvent(input$help_effs, {
    shinyalert::shinyalert("Import de données d'effectifs des écoles", 
                           paste("Ces données sont fournies par la direction de l'éducation et correspondent aux",
                                 "effectifs en octobre. Le format à suivre correspond à trois colonnes 'ecole',", 
                                 "'annee_scolaire' et 'effectif'. Il faut s'assurer que la table de correspondance", 
                                 "entre les noms d'écoles et les noms de restaurants scolaires associés soit à jour",
                                 "dans tests/data/mappings/mapping_ecoles_cantines.csv"), 
                           type = "info")
  }) 
  observeEvent(input$help_holi, {
    shinyalert::shinyalert("Import des données de vacances scolaires", 
                           paste("Ces données sont importées automatiquement à partir du portail open data de",
                                 "l'éducation nationale. Les dates correspondent à la zone B."), 
                           type = "info")
  }) 
  ### Import attendance OD -------------------------------------------------
  observeEvent(input$add_effs_opendata, {
    httr::GET(freq_od, # httr_progress(waitress_od),
              httr::write_disk(freq_od_temp_loc, overwrite = TRUE))
    freqs <- dt()$freqs # %>%
    # dplyr::mutate(site_id = as.character(site_id))
    to_add <- arrow::read_delim_arrow(freq_od_temp_loc, delim = ";",
                                      col_select = c(
                                        site_id, site_type, date, 
                                        prevision_s = prevision, 
                                        reel_s = reel, site_nom
                                      )) %>%
      dplyr::mutate(site_id = as.character(site_id)) %>%
      dplyr::anti_join(freqs)
    
    nrows_to_add <- nrow(to_add)
    ndays_to_add <- length(unique(to_add$date))
    to_add %>%
      dplyr::bind_rows(freqs) %>%
      readr::write_csv(index$path[index$name == "freqs"])
    
    # update_mapping_cafet_freq(to_add)
    sync_ssp_cloud("input")
    shinyalert::shinyalert(title = "Import réussi !",
                           text = paste("Ajout de ",
                                        nrows_to_add,
                                        " effectifs de repas par établissements pour ",
                                        ndays_to_add,
                                        " jours de service."),
                           type = "success")
  })
  ### Import attendance parquet ---------------------------------------------
  # Manually load datafile
  observeEvent(input$add_effs_parquet, {
    file_in <- input$add_effs_parquet
    dt_in <- arrow::read_parquet(file_in$datapath,
                                 col_select = c("DATPLGPRESAT", "NOMSAT", "LIBPRE",
                                                "LIBCON","TOTEFFREE", "TOTEFFPREV")) %>%
        transform_fusion(check_against = dt()$map_freqs$cantine_nom) %>%
        load_fusion(freqs = dt()$freqs)
    shinyalert::shinyalert(title = "Cette fonction est temporairement désactivée",
                           text = paste("Un correctif doit être apporté pour que cette",
                                        "fonctionnalité soit rétablie."),
                           type = "error")
  })
  
  
  ### Import attendance Firebase ----------------------------------------------
  observeEvent(input$add_effs_fusion, {
    drivers <- sort(unique(odbc::odbcListDrivers()[[1]]))
    if (sum(stringr::str_detect(drivers, "Firebird"), na.rm = TRUE) < 1) {
      shinyalert::shinyalert(title = "Besoin d'un accès spécial pour cette option",
                             text = paste("Cette méthode d'import requiert de",
                                          "disposer d'un poste disposant des droits",
                                          "en lecture et des drivers permettant de",
                                          "lire la base de donnée de l'application",
                                          "métier"),
                             type = "error")
    } else {
      # On charge le mot de passe de la base
      load("secret.Rdata")
      # On paramètre la connexion
      con <- DBI::dbConnect(odbc::odbc(), 
                            .connection_string = paste0(
                              "DRIVER=Firebird/InterBase(r) driver;
                 UID=SYSDBA; PWD=",
                              secret, ";
                 DBNAME=C:\\Users\\FBEDECARRA\\Documents\\Fusion\\2021-11-21\\FUSION.FDB;"),
                            timeout = 10)
      dt_in <- DBI::dbReadTable(con, "VIFC_EFFECTIFS_REEL_PREV_CNS") %>%
        dplyr::select(DATPLGPRESAT, NOMSAT, LIBPRE, LIBCON, 
                      TOTEFFREE, TOTEFFPREV) %>%
        transform_fusion(check_against = dt()$map_freqs$cantine_nom) %>%
        load_fusion(freqs = dt()$freqs)
      # update_mapping_cafet_freq(dt_in)
      sync_ssp_cloud("input")
    }
    
  })
  
  ### Import menus Firebase ----------------------------------------------
  observeEvent(input$add_menus_fusion, {
    drivers <- sort(unique(odbc::odbcListDrivers()[[1]]))
    if (sum(stringr::str_detect(drivers, "Firebird"), na.rm = TRUE) < 1) {
      shinyalert::shinyalert(title = "Besoin d'un accès spécial pour cette option",
                             text = paste("Cette méthode d'import requiert de",
                                          "disposer d'un poste disposant des droits",
                                          "en lecture et des drivers permettant de",
                                          "lire la base de donnée de l'application",
                                          "métier"),
                             type = "error")
    } else {
      # On charge le mot de passe de la base
      load("secret.Rdata")
      # On paramètre la connexion
      con <- DBI::dbConnect(odbc::odbc(), 
                            .connection_string = paste0(
                              "DRIVER=Firebird/InterBase(r) driver;
                 UID=SYSDBA; PWD=",
                              secret, ";
                 DBNAME=C:\\Users\\FBEDECARRA\\Documents\\Fusion\\2021-11-21\\FUSION.FDB;"),
                            timeout = 10)
      new_menus <- DBI::dbReadTable(con, "VIFC_MENU") %>%
        dplyr::filter(LIBPRE == "DEJEUNER" & LIBCATFIT != "PAIN") %>%
        dplyr::select(date = "DATPLGPRE", rang = "ORDRE_LIBCATFIT", 
                      plat = "LIBCLIFIT") %>%
        unique() %>%
        dplyr::arrange(dplyr::desc(date), rang) %>% # nicer to inspect the table this way
        dplyr::mutate(date = format(date, "%d/%m/%Y")) %>%
        dplyr::filter(!(date %in% dt()$menus$date)) 
      dplyr::bind_rows(dt()$menus, new_menus) %>%
        readr::write_csv(index$path[index$name == "menus"])
      sync_sspcloud("input")
      shinyalert::shinyalert(title = "Import des menus depuis l'open data réussi !",
                             text = paste("Ajout des menus de convive pour",
                                          nrow(new_menus), 
                                          "plats pour",
                                          length(unique(new_menus$date)), 
                                          "jours de service."),
                             type = "success")
    }
    
  })
  
  ### Import menus parquet ---------------------------------------
  observeEvent(input$add_menus_parquet, {
    file_in <- input$add_menus_parquet
    new_menus  <- arrow::read_parquet(file_in$datapath) %>%
      # new_menus <- arrow::read_parquet("menus.parquet") %>%
      dplyr::filter(LIBPRE == "DEJEUNER" & LIBCATFIT != "PAIN") %>%
      dplyr::select(date = "DATPLGPRE", rang = "ORDRE_LIBCATFIT", 
                    plat = "LIBCLIFIT") %>%
      unique() %>%
      dplyr::arrange(dplyr::desc(date), rang) %>% # nicer to inspect the table this way
      dplyr::mutate(date = format(date, "%d/%m/%Y")) %>%
      dplyr::filter(!(date %in% dt()$menus$date))
    dplyr::bind_rows(dt()$menus, new_menus) %>%
      readr::write_csv(index$path[index$name == "menus"])
    sync_ssp_cloud("input")
    shinyalert::shinyalert(title = "Import réussi des menus depuis le fichier !",
               text = paste("Ajout des menus de convive pour",
                            nrow(new_menus), 
                            "plats pour",
                            length(unique(new_menus$date)), 
                            "jours de service."),
               type = "success")
    
  })
  
  ### Import menus OD -------------------------------------------------
  observeEvent(input$add_menus_opendata, {
    httr::GET(menus_od, # httr_progress(waitress_od),
              httr::write_disk(menus_od_temp_loc, overwrite = TRUE))
    new_menus <- arrow::read_delim_arrow(menus_od_temp_loc, delim = ";") %>%
      dplyr::mutate(date = format(date, "%d/%m/%Y")) %>%
      dplyr::filter(!(date %in% dt()$menus$date))
    menu_path <- as.character(index[index$name == "menus", "path"])
    menus <- readr::read_csv(menu_path)
    dplyr::bind_rows(menus, new_menus) %>%
      readr::write_csv(index$path[index$name == "menus"])
    sync_ssp_cloud("input")
    shinyalert::shinyalert(title = "Import des menus depuis l'open data réussi !",
                           text = paste("Ajout des menus de convive pour",
                                        nrow(new_menus), 
                                        "plats pour",
                                        length(unique(new_menus$date)), 
                                        "jours de service."),
                           type = "success")
    
  })
  
  
  ### Manually load strikes -------------------------------------------------
  observeEvent(input$add_strikes, {
    file_in <- input$add_strikes
    dt_in <- readr::read_csv(file_in$datapath)
    dt_old <- readr::read_csv(index$path[index$name == "strikes"])
    dt_new <- dplyr::anti_join(dt_in, dt_old, by = "date")
    dt_old %>%
      dplyr::bind_rows(dt_new) %>%
      readr::write_csv(index$path[index$name == "strikes"])
    sync_ssp_cloud("input")
    shinyalert::shinyalert(title = "Import manuel des gèves réussi !",
                           text = paste("Ajout des grèves pour ", nrow(dt_new), " jours."),
                           type = "success")
  })
  
  ### Import vacations from open data ----------------------------------------
  observeEvent(input$add_vacs_od, {
    httr::GET(vacs_od, # httr_progress(waitress_od),
              httr::write_disk(vacs_od_temp_loc, overwrite = TRUE))
    old_vacs <- dt()$vacs
    new_vacs <- readr::read_delim(vacs_od_temp_loc, delim = ";") %>%
      dplyr::filter(location == "Nantes" & population != "Enseignants")  %>%
      dplyr::select(annee_scolaire, vacances_nom = description,
                    date_debut = start_date, date_fin = end_date) %>%
      dplyr::mutate(zone = "B", vacances = 1, 
                    date_debut = as.Date(date_debut),
                    date_fin = as.Date(date_fin)) %>%
      # dplyr::anti_join(old_vacs)
      dplyr::filter(!(annee_scolaire %in% old_vacs$annee_scolaire))
    new_vacs %>%
      dplyr::bind_rows(old_vacs) %>%
      readr::write_csv(index$path[index$name == "vacs"])
    
    # Updating annees_scolaires.csv
    gen_piv(dt()$vacs) %>%
      dplyr::filter(stringr::str_detect(periode, "Ete")) %>%
      tidyr::pivot_longer(cols = c(-annee, -periode), names_to = "borne", values_to = "date") %>%
      dplyr::filter((periode == "Ete-Toussaint" & borne == "Début") | 
                      (periode == "Printemps-Ete" & borne == "Fin")) %>%
      dplyr::select(-periode) %>%
      tidyr::pivot_wider(id_cols = "annee", names_from = "borne", values_from = "date") %>%
      dplyr::select(annee_scolaire = annee, date_debut = `Début`, date_fin = Fin) %>%
      dplyr::mutate(date_debut = date_debut + 1, date_fin = date_fin - 1) %>%
      dplyr::anti_join(dt()$schoolyears, by = "annee_scolaire") %>%
      dplyr::arrange(dplyr::desc(date_debut)) %>%
      dplyr::bind_rows(dt()$schoolyears) %>%
      readr::write_csv(index$path[index$name == "schoolyears"])
    
    sync_ssp_cloud("input")
    shinyalert::shinyalert(title = "Import des vacances depuis l'open data de l'éducation nationale réussi !",
               text = paste("Ajout des vacances scolaires pour la Zone B, pour",
                            nrow(new_vacs), 
                            "périodes de vacances."),
               type = "success")
    
  })
  
  ### Import headcounts  ---------------------------------------------
  
  # Manually load datafile
  observeEvent(input$add_headcounts, {
    file_in <- input$add_headcounts
    if (stringr::str_starts(input$schoolyear_hc, "[0-9]", negate = TRUE)) {
      shinyalert::shinyalert("Sélectionner une année", 
                 "Veuillez sélectionner l'année scolaire correspondante au fichier importé et relancer l'import.",
                 type = "error")
    } else {
      an_scol_import <- input$schoolyear_hc
      hc_new <- readxl::read_excel(file_in$datapath, 
                                   skip = 1) %>%
        dplyr::filter(!is.na(.[[colnames(.)[1]]])) %>%
        dplyr::select(ecole = Ecoles, effectif = starts_with("Total g")) %>%
        dplyr::mutate(annee_scolaire = an_scol_import)
      hc_all <- dt()$effs %>%
        dplyr::filter(!(paste(ecole, annee_scolaire) %in% paste(hc_new$ecole, hc_new$annee_scolaire))) %>%
        dplyr::bind_rows(hc_new) %>%
        readr::write_csv(index$path[index$name == "effs"])
      shinyalert::shinyalert(title = "Import manuel des effectifs réussi !",
                             text = paste("Ajout de ",
                                          nrow(hc_new), 
                                          "effectifs d'écoles."),
                             type = "success")
    }
  })
  
  ### Import headcounts OD -------------------------------------------------
  observeEvent(input$add_hc_od, {
    old_hc <- dt()$effs
    httr::GET(hc_od, # httr_progress(waitress_od),
              httr::write_disk(hc_od_temp_loc, overwrite = TRUE))
    new_hc <- arrow::read_delim_arrow(hc_od_temp_loc, delim = ";") %>%
      dplyr::select(ecole, annee_scolaire, effectif)
    old_hc <- dt()$effs %>%
      dplyr::filter(!(paste(ecole, annee_scolaire) %in% paste(new_hc$ecole, new_hc$annee_scolaire)))
    hc_path <- as.character(index[index$name == "effs", "path"])
    dplyr::bind_rows(old_hc, new_hc) %>%
      readr::write_csv(index$path[index$name == "effs"])
    shinyalert::shinyalert(title = "Import des effectifs depuis l'open data réussi !",
                           text = paste("Ajout de",
                                        nrow(new_hc), 
                                        "effectifs d'écoles."),
                           type = "success")
  })
  
  ### Check mappings -------------------------------------------------
  observeEvent(input$check_mappings, {
    # check mapping  mapping_frequentation_cantines.csv
    freqs_notin_mfreqs <- not_in(dt()$freqs$site_nom, dt()$map_freqs$site_nom)
    cafets_notin_mfreqs <- not_in(dt()$cafets$cantine_nom, dt()$map_freqs$cantine_nom)
    # check mapping mapping_ecoles_cantines.csv
    effs_notin_mschools <- not_in(dt()$effs$ecole, dt()$map_schools$ecole)
    cafets_notin_mschools <- not_in(dt()$cafets$cantine_nom, dt()$map_schools$cantine_nom)
    # check that mapped cafets appear in the list cantines.csv
    mschools_notin_cafets <- not_in(dt()$map_schools$cantine_nom, dt()$cafets$cantine_nom)
    mfreqs_notin_cafets <- not_in(dt()$map_freqs$cantine_nom, dt()$cafets$cantine_nom)
    # Display result
    map_check_msg <- paste(c(freqs_notin_mfreqs, cafets_notin_mfreqs, effs_notin_mschools, 
                             cafets_notin_mschools, mschools_notin_cafets), collapse = "\n")
    if (map_check_msg != "") {
      shinyalert::shinyalert(title = "Tables de correspondances incomplètes",
                             text = map_check_msg,
                             type = "warning")
    } else {
      shinyalert::shinyalert(title = "Les tables de correspondances sont en ordre",
                             text = "Pas d'incohérences relevées.",
                             type = "success")
    }
    
    
    
  })
  
  ## Launch model ------------------------------------------------------------
  observeEvent(input$launch_model, {
    run_verteego(
      data_path = data_path,
      begin_date = as.character(input$daterange_forecast[1]),
      column_to_predict =  input$column_to_predict,
      confidence = input$confidence,
      end_date = as.character(input$daterange_forecast[2]),
      preprocessing = "preprocessing" %in% input$model_options,
      remove_no_school = "remove_no_school" %in% input$model_options,
      remove_outliers = "remove_outliers" %in% input$model_options,
      start_training_date = as.character(input$start_training_date),
      training_type = input$training_type,
      weeks_latency = input$week_latency
    )
    # then send outputs to S3 storage if runing on SSPCloud
    sync_ssp_cloud("output")
    
    
  })
  
  
  ## Compute and render prevision errors -------------------------------------
  
  consolidated <- reactive({
    consolid <- dt()$freqs %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::right_join(prev(),
                        by = c("date" = "date_str", 
                               "site_nom" = "cantine_nom")) %>%
      dplyr::select(date, site_nom, site_type, prevision, reel, output) %>%
      dplyr::left_join(readr::read_csv(index$path[index$name == "strikes"]),
                       by = "date")
    
    consolid <- consolid %>%
      dplyr::mutate(`Erreur de prédiction` =  output - reel,
                    type = "Modèle") %>%
      dplyr::bind_rows(dplyr::mutate(consolid, 
                                     `Erreur de prédiction` =  prevision - reel,
                                     type = "Agents")) %>%
      dplyr::filter(is.na(greve) & reel != 0)  %>%
      dplyr::filter(abs(`Erreur de prédiction`) < 100) %>%
      dplyr::mutate(Mois = paste(lubridate::year(date), 
                                 lubridate::month(date), sep = "-")) %>%
      dplyr::group_by(type) 
    
    return(consolid)
  })
  
  distance <- reactive({
    consolidated() %>%
      dplyr::summarise(mean_error = mean(mean(`Erreur de prédiction`, 
                                              na.rm = TRUE)))
  })
  
  
  output$error_by_school <- renderPlot({ 
    consolidated() %>%
      ggplot2::ggplot(ggplot2::aes(x = `Erreur de prédiction`)) + 
      ggplot2::geom_density(ggplot2::aes(y = ..count.., color = type)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean_error, color = type),
                          data = distance(), linetype = "dashed") +
      ggplot2::labs(title = "Erreurs de prédiction quotidiennes par cantine",
                    x = "Erreur de prédiction : densité (courbe) et moyenne (pointillés)",
                    y = "Occurence (densité lissée)")
  })
  
  
  global <- reactive({
    consolidated() %>%
      dplyr::mutate(`Année` = ifelse(lubridate::month(date) > 7,
                                     paste(lubridate::year(date), lubridate::year(date)+1, sep = "-"),
                                     paste(lubridate::year(date)-1, lubridate::year(date), sep = "-"))) %>%
      dplyr::group_by(date, `Année`, type) %>%
      dplyr::summarise(`Erreur de prédiction` = sum(`Erreur de prédiction`,
                                                    na.rm = TRUE))
  })
  
  
  distance_global <- reactive({
    global() %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(mean_error = mean(mean(`Erreur de prédiction`,
                                              na.rm = TRUE)))
  })
  
  
  output$error_global <- renderPlot({ global() %>%
      dplyr::group_by(type) %>%
      ggplot2::ggplot(ggplot2::aes(x = `Erreur de prédiction`)) +
      ggplot2::geom_density(ggplot2::aes(y = ..count.., color = type)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean_error, color = type),
                          data = distance_global(), linetype = "dashed") +
      ggplot2::labs(title = "Erreurs de prédiction quotidiennes au global",
                    x = "Erreur de prédiction : densité (courbe) et moyenne (pointillés)",
                    y = "Fréquence (densité lissée, 1 = 100%)")
  })
  
  ## System info -------------------------------------------------------------
  
  output$sysinfo <- DT::renderDataTable({
    s = Sys.info()
    df = data.frame(Info_Field = names(s),
                    Current_System_Setting = as.character(s))
    return(DT::datatable(df, rownames = F, selection = 'none',
                         style = 'bootstrap', filter = 'none', options = list(dom = 't')))
  })
  # Display system language
  output$which_locale <- renderText({
    paste0("Paramètres de langue de l'environnement R : ", 
           Sys.getlocale(category = "LC_ALL"))
  })
  # Display system path to python
  output$which_python <- renderText({
    paste0("Emplacement de Python : ", Sys.which('python'))
  })
  # Display Python version
  output$python_version <- renderText({
    rr = reticulate::py_discover_config(use_environment = 'python35_env')
    paste0("Version de Python : ", rr$version)
  })
  # Display RETICULATE_PYTHON
  output$ret_env_var <- renderText({
    paste0('RETICULATE_PYTHON: ', Sys.getenv('RETICULATE_PYTHON'))
  })
  # Display virtualenv root
  output$venv_root <- renderText({
    paste0("Emplacement de l\'environnement virtuel :", reticulate::virtualenv_root())
  })
  
  
}
