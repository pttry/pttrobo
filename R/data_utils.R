#' Print get code and full filtering for Robonomist id or url
#'
#' @param x robonomist id or url that robonomist undestands (statfi).
#' @param conc A locigal to copy to clipboard.
#'
#' @export
#'
#' @examples
#'   pttrobo_print_code("https://pxweb2.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__vaerak/statfin_vaerak_pxt_11ra.px/", conc = FALSE)
#'   pttrobo_print_code("StatFin/vaerak/statfin_vaerak_pxt_11ra.px", conc = FALSE)
#'
pttrobo_print_code <-
  function(x, conc = TRUE){

    check_url <- httr::parse_url(x)

    if (!is.null(check_url$scheme) && check_url$scheme %in% c("http", "https")){
      id <- robonomistClient::data_search(x)$id

      if (length(id) == 0) stop("Url: ", x, " not found in Robonomist data.")
      if (length(id) != 1) {
        message("Several ids found. First will be used\n", id)
        id <- id[1]
      }

    } else {
      id <- x
    }

    out <- paste0(
      "ptt_data_robo(", id, ") |>\n  ",
      pttrobo_print_filter_recode(id, conc = FALSE, print = FALSE)

    )
    cat(out)
    if (conc) cat(out, file = "clipboard-128")

  }


#' Print full filtering for Robonomist id or data
#'
#' In ptt-format
#'
#' @param x A Robonomist id or robonomist_data
#' @param conc A locigal whether to copy in clipboard
#' @param print A locigal whether to print output (to only return invisibly)
#' @export
#' @examples
#'   pttrobo_print_filter(x = "luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/08_Tuottajahinnat_Vilja_rypsi_rapsi_v.px", conc = FALSE)
#'   pttrobo_print_filter_recode(x = "luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/08_Tuottajahinnat_Vilja_rypsi_rapsi_v.px", conc = FALSE)

pttrobo_print_filter <- function(x, conc = TRUE, print = TRUE){
  if (!inherits(x, "robonomist_data")){
    x <- ptt_data_robo_l(x)
  }

  y <- lapply(x, function(x) {
    if (is.character(x) | is.factor(x)) {
      unique(x)
    }})

  y <- y[!unlist(lapply(y, is.null))]



    out <- paste0(
      "filter(\n  ",
      paste0(purrr::imap(y, ~paste0(.y," %in% c(\"", paste0(as.character(.x), collapse = "\", \""), "\")")), collapse = ",\n  "),
      "\n  )"


    )

  if (print) cat(out)
  if (conc) cat(out, file = "clipboard-128")
  invisible(out)

}

#' @describeIn pttrobo_print_filter version for pttdatahaku::filter_recode()
#' @export
pttrobo_print_filter_recode <- function(x, conc = TRUE, print = TRUE){
  if (!inherits(x, "robonomist_data")){
    x <- ptt_data_robo_l(x)
  }

  y <- lapply(x, function(x) {
    if (is.character(x) | is.factor(x)) {
      unique(x)
    }})

  y <- y[!unlist(lapply(y, is.null))]



  out <- paste0(
    "filter_recode(\n    ",
    paste0(purrr::imap(y, ~paste0(.y," = c(\"", paste0(as.character(.x), collapse = "\", \""), "\")")), collapse = ",\n    "),
    "\n  )"


  )
  if (print) cat(out)
  if (conc) cat(out, file = "clipboard-128")
  invisible(out)

}
