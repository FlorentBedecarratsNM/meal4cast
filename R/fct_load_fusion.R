#' load_fusion 
#'
#' @description A function to load data from Salamandre Fusion.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

load_fusion <- function(x, freqs) {
  new_days <- x %>%
    dplyr::anti_join(freqs, by = c("date", "site_nom"))
  
  alert_exist <- ""
  if (!("reel_adulte" %in% colnames(freqs))) {
    exist_days <- x %>%
      dplyr::select(-reel, -prevision) %>%
      dplyr::inner_join(dplyr::select(freqs, -reel, -prevision, -site_type), 
                        by = c("date", "site_nom"))
    alert_exist <- paste("Complément des fréquentation par type de convive pour",
                         nrow(exist_days), 
                         "effectifs de repas par établissement pour",
                         length(unique(exist_days$date)), 
                         "jours de service.\n")
    freqs <- freqs %>%
      dplyr::left_join(exist_days, by = c("date", "site_nom"))
  }
  freqs <- dplyr::bind_rows(freqs, new_days) %>%
    readr::write_csv(index$path[index$name == "freqs"])
  alert_new <- paste("Ajout des fréquentation par type de convive pour",
                     nrow(new_days), 
                     "effectifs de repas par établissement pour",
                     length(unique(new_days$date)), 
                     "jours de service.")
  
  shinyalert::shinyalert(title = "Import depuis le fichier issu de Fusion réussi !",
                         text = paste0(alert_exist, alert_new),
                         type = "success")
}