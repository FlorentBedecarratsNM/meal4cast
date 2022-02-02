#' gen_opendata_url 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
gen_opendata_url <- function(portal, dataset_id, 
                   params = "/exports/csv") {
  left <- paste0("https://", portal, "/api/v2/catalog/datasets/")
  paste(left, dataset_id, params, sep = "")
}