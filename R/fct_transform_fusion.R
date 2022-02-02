#' transform_fusion 
#'
#' @description A function to transform data from Fusion for training data.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

transform_fusion <- function(x, check_against) {
  x %>%
    dplyr::rename(date = DATPLGPRESAT, site_nom = NOMSAT, repas = LIBPRE, convive = LIBCON,
                  reel = TOTEFFREE, prev = TOTEFFPREV) %>%
    dplyr::filter(repas == "DEJEUNER") %>%
    dplyr::filter(stringr::str_starts(site_nom, "CL", negate = TRUE)) %>%
    dplyr::filter(stringr::str_detect(site_nom, "TOURNEE", negate = TRUE)) %>%
    dplyr::select(-repas) %>%
    dplyr::mutate(convive = dplyr::recode(convive, 
                                          "1MATER." = "maternelle",
                                          "2GS." = "grande_section",
                                          "3PRIMAIRE" = "primaire",
                                          "4ADULTE" = "adulte"),
                  site_id = stringr::str_remove(site_nom, "[0-9]{3}"),
                  site_nom = stringr::str_remove(site_nom, "[0-9]{3} "),
                  site_nom = stringr::str_replace(site_nom, "COUDRAY MAT", "COUDRAY M\\."),
                  site_nom = stringr::str_replace(site_nom, "MAT", "M"),
                  site_nom = stringr::str_replace(site_nom, "COUDRAY ELEM", "COUDRAY E\\."),
                  site_nom = stringr::str_replace(site_nom, "ELEM", "E"),
                  site_nom = stringr::str_remove(site_nom, " M/E"),
                  site_nom = stringr::str_remove(site_nom, " PRIM"),
                  site_nom = stringr::str_remove(site_nom, "\\(.*\\)$"),
                  site_nom = stringr::str_trim(site_nom),
                  site_nom = stringr::str_replace(site_nom, "BAUT", "LE BAUT"),
                  site_nom = stringr::str_replace(site_nom, "  ", " "),
                  site_nom = stringr::str_replace(site_nom, "FOURNIER", "FOURNIER E"),
                  site_nom = stringr::str_replace(site_nom, " E / ", "/"),
                  site_nom = stringr::str_replace(site_nom, "MACE$", "MACE M"),
                  site_nom = ifelse(!(site_nom %in% check_against) & stringr::str_ends(site_nom, " (E|M)"),
                                    stringr::str_remove(site_nom, " (E|M)$"), site_nom),
                  site_nom = stringr::str_replace(site_nom, "A.LEDRU-ROLLIN/S.BERNHARDT", 
                                                  "LEDRU ROLLIN/SARAH BERNHARDT"),
                  site_nom = stringr::str_replace(site_nom, "F.DALLET/DOCT TEILLAIS", 
                                                  "FRANCOIS DALLET/DOCTEUR TEILLAIS")) %>%
    dplyr::group_by(date, site_id, site_nom, convive) %>%
    dplyr::summarise(reel = sum(reel, na.rm = TRUE),
                     prev = sum(prev, na.rm = TRUE)) %>%
    tidyr::pivot_wider(names_from = convive, values_from = c(reel, prev),
                       values_fill = 0) %>%
    dplyr::mutate(reel = reel_maternelle + reel_grande_section + reel_primaire + reel_adulte,
                  prevision = prev_maternelle + prev_grande_section + prev_primaire + prev_adulte,
                  date = lubridate::date(date)) # %>%
  # dplyr::select(site_id, site_nom, site_type, date, prevision, reel)
}
