#' Get yaml files from ennustedata folder
#'
#' @param pattern A string to look in yaml file names.
#'
#' @export
#' @examples
#' yaml_files_ennustedata()
#' yaml_files_ennustedata("ME")
yaml_files_ennustedata <- function(pattern = NULL){
  files <- list.files(path = system.file("ennustedata", package = "pttrobo"),
             pattern = "\\.yaml$",
             full.names = TRUE)
  if (!is.null(pattern)){
    files <- files[grepl(pattern = pattern, basename(files))]
  }
  files
}

#' Get excel files from ennustedata folder
#'
#' @param pattern A string to look in excel file names.
#'
#' @export
#' @examples
#' excel_files_ennustedata()
#' excel_files_ennustedata("ME")
excel_files_ennustedata <- function(pattern = NULL){
  files <- list.files(path = system.file("ennustedata", package = "pttrobo"),
                      pattern = "\\.xlsx$",
                      full.names = TRUE)
  if (!is.null(pattern)){
    files <- files[grepl(pattern = pattern, basename(files))]
  }
  files
}


#' Update PTT ennustedata from yaml definitions
#'
#' @param pattern A string to look in file names to update.
#' @param start_year A first year of data
#'
#' @export
ptt_update_ennustedata <- function(pattern = NULL, start_year){
  files <- yaml_files_ennustedata(pattern = pattern)
  for (file in files) {
    yaml_to_excel(file = file, start_year = start_year)
  }

}

#' Copy ennustedata to teams
#'
#' @param ennuste A character of ennuste. One of "ME", "KT", "MA".
#'
#' @export
#'
#'
#'

ptt_copy_ennustedata <- function(ennuste = c("ME", "KT", "MA"), path = NULL){

  paths <-
    c(ME = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - Taulut ME/data",
      MA = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - MAdata",
      KT = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - KTdata")

  if (is.null(path)){
    path <- paths[ennuste]
  }

  files <- excel_files_ennustedata(pattern = ennuste)
  file.copy(files, to = path, overwrite = TRUE)

}

