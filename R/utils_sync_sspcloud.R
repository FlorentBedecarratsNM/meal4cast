#' sync_sspcloud 
#'
#' @description A function to sync data with SSPCloud S3 storage
#'
#' @return list of objects with TRUE if sync worked.
#'
#' @noRd
# a function to sync training data or generated previsions to SSPCloud
sync_sspcloud <- function(folders) {
  # Check if the app is running on SSPCloud
  if (Sys.info()[['user']] == "rstudio") {
    # Then send selected objects to SSP Cloud
    for (i in 1:length(folders)) {
      folder <- folders[i]
      create_folder(folder)
      # Check that folder name has a trailing slash and add it if needed
      folder <- ifelse(stringr::str_ends(folder, "/"), folder, paste0(folder, "/"))
      aws.s3::s3sync(path = folder,
                     bucket = "fbedecarrats",
                     prefix = paste0("diffusion/cantines/", folder), # diffusion to be able to share
                     create = FALSE,
                     region = "") # Important for the aws.s3 functions to work
    }
  }
}
