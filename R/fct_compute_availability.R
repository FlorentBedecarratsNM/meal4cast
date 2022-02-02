#' compute_availability 
#'
#' @description A function to inventory the available data.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

compute_availability <- function(x) {
  avail_strikes <- x$strikes %>%
    dplyr::mutate("avail_data" = "Grèves") %>%
    dplyr::select(date, avail_data, n= greve)
  
  # Compute the number of values of staff previsions and kid attendance
  avail_freqs <- x$freqs %>%
    dplyr::select(date, prevision, reel) %>%
    tidyr::pivot_longer(cols = -date, names_to = "avail_data") %>%
    dplyr::mutate(avail_data = dplyr::recode(avail_data,
                                             prevision = "Commandes",
                                             reel = "Fréquentation")) %>%
    dplyr::group_by(date, avail_data) %>%
    dplyr::summarise(n = dplyr::n())
  
  # Compute the number of menu items registered per day 
  avail_menus <- x$menus %>%
    dplyr::mutate("avail_data" = "Menus",
                  date = lubridate::dmy(date)) %>%
    dplyr::group_by(date, avail_data) %>%
    dplyr::summarise(n = dplyr::n())
  
  # Vacances  
  vacs <- x$vacs
  
  vacs_dates <- purrr:::map2(vacs$date_debut, vacs$date_fin, 
                             ~ seq(.x, .y, by = "1 day")) %>%
    purrr::reduce(c)
  
  avail_vacs <-tidyr::tibble(
    date = vacs_dates,
    avail_data = "Vacances",
    n = 1)
  
  avail_holidays <- x$holidays %>%
    dplyr::mutate(avail_data = "Fériés") %>%
    dplyr::select(date, avail_data, n = jour_ferie)
  
  avail_data <- dplyr::bind_rows(avail_freqs, avail_menus, avail_strikes,
                                 avail_vacs) %>%
    dplyr::bind_rows(dplyr::filter(avail_holidays,
                                   date <= max(.$date),
                                   date >= (min(.$date)))) %>%
    dplyr::mutate(annee = lubridate::year(date),
                  an_scol_start = ifelse(lubridate::month(date) > 8, 
                                         lubridate::year(date), 
                                         lubridate::year(date)-1),
                  an_scol = paste(an_scol_start, an_scol_start+1, sep = "-"),
                  an_scol = forcats::fct_rev(an_scol),
                  `Jour` = lubridate::ymd(
                    paste(ifelse(lubridate::month(date) > 8, "1999", "2000"),
                          lubridate::month(date), lubridate::day(date), sep = "-"))) %>%
    dplyr::group_by(an_scol, avail_data) %>%
    dplyr::mutate(max_year_var = max(n, na.rm = TRUE),
                  nday_vs_nyearmax = n / max_year_var) %>%
    dplyr::mutate(avail_data = factor(avail_data,
                                      levels = c("Vacances", "Fériés", "Grèves", "Menus", "Commandes", "Fréquentation")))
  return(avail_data)
}
