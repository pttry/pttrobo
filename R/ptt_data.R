#' Get data with robonomistClient in PTT form
#'
#' Uses \code{robonomistClient::data_get} to get data from robonomist,
#' but with PTT formating:
#'  - codes instead of labels
#'  - tidy_time = TRUE
#'  - variables as factors
#'
#' @param ... Arguments to \code{\link[robonomistClient]{data_get}}.
#'            see \code{\link[robonomistClient]{data}}
#' @param labels A logical whether to get labels or codes.
#'
#' @export
#' @examples
#'   ptt_data_robo("StatFin/asu/asvu/statfin_asvu_pxt_11x4.px") |> head()
ptt_data_robo <- function(..., labels = FALSE){
  robonomistClient::data_get(..., labels = labels, tidy_time = TRUE) |>
    statfitools::clean_names() |>
    dplyr::mutate(dplyr::across(where(is.character), forcats::as_factor)) |>
    droplevels()

}


utils::globalVariables("where")
