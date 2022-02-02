#' gen_schoolyears 
#'
#' @description A function to generate a vector of school years
#'
#' @return Character vector. 
#'
#' @noRd
# 
gen_schoolyears <- function(year_start, year_end) {
  if(!(year_start > 2000 & year_end < 2050 & year_start < year_end)) {
    print("Specified year must be integers between 2000 and 2050 and start must be before end.")
  } else {
    left_side <- year_start:year_end
    right_side <- left_side + 1
    schoolyears <- paste(left_side, right_side, sep = "-")
    return (schoolyears)
  }
}