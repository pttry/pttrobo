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
  files <- list.files(path = system.file("ennustedata_xlsx", package = "pttrobo"),
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
#' @param transpose A logical to transpose data for excel.
#'
#' @export
ptt_update_ennustedata <- function(pattern = NULL, start_year, transpose = FALSE){
  files <- yaml_files_ennustedata(pattern = pattern)
  for (file in files) {
    yaml_to_excel(file = file, start_year = start_year, transpose = transpose)
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

ptt_copy_ennustedata <- function(ennuste = c("ME", "KT", "MA", "POP"), path = NULL){

  paths <-
    c(ME = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - data",
      MA = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - MAdata",
      KT = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - KTdata",
      POP = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - KTdata")

  if (is.null(path)){
    path <- paths[ennuste]
  }

  files <- excel_files_ennustedata(pattern = ennuste)
  file.copy(files, to = path, overwrite = TRUE)

}

#' Update PTT data from yaml definitions
#'
#' @param pattern A string to look in file names to update.
#' @param start_year A first year of data
#' @param transpose A logical to transpose data for excel.
#'
#' @import Microsoft365R dplyr
#' @export
#' @examples
#'   ptt_update_data_sharepoint(
#'     site = "Ennuste", path = "KT/Taulut KT/Laskentataulukot/KTdata_test",
#'     start_year = 2013)
ptt_update_data_sharepoint <-
  function(pattern = NULL,
           site, drive = "Documents", path,
           start_year,
           transpose = FALSE){

  site <- get_sharepoint_site(
    site_url = file.path("https://pttry.sharepoint.com/sites", site))
  drv <- site$get_drive(drive)

  yaml_files <- drv$list_files(file.path(path, "yamls"))$name
  yaml_files <- grep(".yaml", yaml_files, value = TRUE)

  tmp_dir <- tempdir()

  for (yfile in yaml_files){

    yaml_file <- drv$get_item(file.path(path, "yamls", yfile))

    tmp_file <- tempfile()
    yaml_file$download(dest = tmp_file, overwrite = TRUE)

    xlsx_files <- yaml_to_excel(file = tmp_file, xlsx_path = tmp_dir, start_year = start_year, transpose = transpose)

    for (file in xlsx_files){
      dest <- file.path(path, basename(file))
      drv$upload_file(file, dest)# file, path)
      cli_alert_success("Wrote {dest}")
    }

    unlink(tmp_file)
  }

  unlink(tmp_dir)

  cli_alert_success("files in {path} updated")

}
