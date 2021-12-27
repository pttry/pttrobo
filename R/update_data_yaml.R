#' Get yaml files from ennustedata folder
#'
#' @param pattern A string to look in yaml file names.
#'
#' @export
#' @examples
#' yaml_files_ennustedata()
#' yaml_files_ennustedata("ME)
yaml_files_ennustedata <- function(pattern = NULL){
  files <- list.files(path = system.file("ennustedata", package = "pttrobo"),
             pattern = "\\.yaml$",
             full.names = TRUE)
  if (!is.null(pattern)){
    files <- stringr::str_subset(files, pattern)
  }
  files
}

#' Update PTT ennustedata from yaml definitions
#'
#' @param pattern A string to look in file names to update.
#' @param start_year A first year of data
ptt_update_ennustedata <- function(pattern = NULL, start_year){
  files <- yaml_files_ennustedata(pattern = pattern)
  for (file in files) {
    yaml_to_excel(file = file, start_year = start_year)
  }

}


