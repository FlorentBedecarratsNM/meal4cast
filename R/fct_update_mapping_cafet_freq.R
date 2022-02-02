#' update_mapping_cafet_freq 
#'
#' @description A function to enrich cafet list after frequentation import.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

update_mapping_cafet_freq <- function(x, 
                                      map_freq_loc = paste0(data_path,
                                                            "/mappings/mapping_frequentation_cantines.csv")) {
  map_freq <-  readr::read_csv(map_freq_loc)
  
  new_site_names <- x %>%
    dplyr::select(site_nom) %>%
    unique() %>%
    dplyr::filter(!(site_nom %in% map_freq$site_nom)) %>%
    dplyr::left_join(dplyr::select(x, site_nom, site_type), by = "site_nom") %>%
    unique() %>%
    dplyr::mutate(site_type = ifelse(is.na(site_type), "M/E", site_type),
                  cantine_nom = site_nom,
                  cantine_type = site_type)
  
  if (nrow(new_site_names) > 0) {
    map_freq <- map_freq %>%
      dplyr::bind_rows(new_site_names)
    readr::write_csv(map_freq, map_freq_loc)
  }
  
}