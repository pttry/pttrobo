#' Get Data with robonomistClient in PTT Format
#'
#' Uses \code{robonomistClient::data_get} to get data from robonomist,
#' but with PTT formating:
#'  - tidy_time = TRUE
#'  - variables as factors
#'
#' @param ... Arguments to \code{\link[robonomistClient]{data_get}}.
#'            see \code{\link[robonomistClient]{data}}
#' @param labels A logical whether to get labels or codes.
#'
#' @export
#' @examples
#' \dontrun{
#'   ptt_data_robo("StatFin/asu/asvu/statfin_asvu_pxt_11x4.px") |> head()
#' }
ptt_data_robo <- function(..., labels = TRUE){
  robonomistClient::data_get(..., labels = labels, tidy_time = TRUE) |>
    statfitools::clean_names() |>
    dplyr::mutate(dplyr::across(where(is.character), forcats::as_factor)) |>
    droplevels()
}


#' @describeIn ptt_data_robo With labels TRUE.
#' @export
#'
ptt_data_robo_l <- function(..., labels = TRUE){
  ptt_data_robo(..., labels = labels)
}

#' @describeIn ptt_data_robo With labels FALSE.
#' @export
#'
ptt_data_robo_c <- function(..., labels = FALSE){
  ptt_data_robo(..., labels = labels)
}

#' @describeIn ptt_data_robo Both labels and codes.
#' @export
#'
ptt_data_robo_b <- function(...){
  bind_cols(
    ptt_data_robo_l(...),
    rename_with(ptt_data_robo_c(...), ~paste0(.x, "_code"))
  ) |>
    select(-value_code, -time_code) |>
    relocate(value, .after = last_col())
}

utils::globalVariables("where")
