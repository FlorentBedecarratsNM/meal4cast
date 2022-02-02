#' set_config_variables 
#'
#' @description A utils function that sets all application variables
#'
#' @return Sets variables in the global environmnent with the '<<-' operator.
#'
#' @noRd
set_config_variables <- function() {
  data_path <<- "data"
  index <<- dplyr::tribble(
    ~name,          ~path,
    "schoolyears",  "calculators/annees_scolaires.csv",
    "strikes",      "calculators/greves.csv",
    "holidays",     "calculators/jours_feries.csv",
    "vacs",         "calculators/vacances.csv",
    "cafets",       "raw/cantines.csv",            
    "effs",         "raw/effectifs.csv",
    "freqs",        "raw/frequentation.csv",
    "menus",        "raw/menus_tous.csv",
    "map_schools",  "mappings/mapping_ecoles_cantines.csv",
    "map_freqs",    "mappings/mapping_frequentation_cantines.csv") %>%
    dplyr::mutate(path = paste(data_path, path, sep = "/"))
  
  # Begin and end year for selecting school years when loading headcounts
  schoolyear_hq_start <<- 2010
  schoolyear_hq_end <<- 2025
  
  # A parameter for the display of widgets on the "load data" page
  width_load_widgets <<- "317px"
  
  # A function to build open data urls from portal and dataset id
  portal <- "data.nantesmetropole.fr"
  
  freq_id <- "244400404_nombre-convives-jour-cantine-nantes-2011"
  freq_od <<- gen_opendata_url(portal = portal, dataset_id = freq_id)
  freq_od_temp_loc <<- "temp/freq_od.csv"
  
  menus_id <- "244400404_menus-cantines-nantes-depuis-2011"
  menus_od <<- gen_opendata_url(portal = portal, dataset_id = menus_id)
  menus_od_temp_loc <<- "temp/menus_od.csv"
  
  
  hc_id <- "244400404_effectifs-eleves-ecoles-publiques-maternelles-elementaires-nantes"
  hc_od <<- gen_opendata_url(portal = portal, dataset_id = hc_id)
  hc_od_temp_loc <- "temp/headcounts_od.csv"
  
  vacs_od <<- paste0("https://data.education.gouv.fr/explore/dataset/",
                    "fr-en-calendrier-scolaire/download/?format=csv")
  vacs_od_temp_loc <<- "temp/vacs_od.csv"
}