#' gen_piv 
#'
#' @description A function to generate inter-vacation periods from the 
#' vacation calendar.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

gen_piv <- function(vacations) {
  vacations %>%
    dplyr::filter(vacances_nom != "Pont de l'Ascension") %>%
    unique() %>%
    dplyr::arrange(date_debut) %>%
    dplyr::mutate(piv_nom2 = stringr::str_remove(vacances_nom, 
                                                 "Vacances (d'|de la |de )"),
                  piv_nom2 = stringr::str_replace(piv_nom2, "Avril", "Printemps"),
                  piv_nom2 = stringr::str_replace(piv_nom2, "Début des Été", "Ete"),
                  piv_nom1 = dplyr::lag(piv_nom2, 1),
                  periode = paste(piv_nom1, piv_nom2, sep = "-"),
                  `Début` = dplyr::lag(date_fin, 1),
                  Fin = date_debut)  %>%
    dplyr::filter(!is.na(piv_nom1)) %>%
    dplyr::select(annee = annee_scolaire,periode, `Début`, `Fin`) %>%
    dplyr::mutate(periode = stringi::stri_trans_general(str = periode, id = "Latin-ASCII"),
                  periode = factor(periode, c(
                    "Ete-Toussaint", "Toussaint-Noel", "Noel-Hiver", "Hiver-Printemps",
                    "Printemps-Ete")))
}