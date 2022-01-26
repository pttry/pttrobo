#' Print full filtering for Robonomist id or data
#'
#' In ptt-format
#'
#' @param x A Robonomist id or robonomist_data
#' @param conc A locigal whether to copy in clipboard
#' @export
#' @examples
#'   pttrobo_print_filter(x = "luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/08_Tuottajahinnat_Vilja_rypsi_rapsi_v.px", conc = FALSE)
#'   pttrobo_print_filter_recode(x = "luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/08_Tuottajahinnat_Vilja_rypsi_rapsi_v.px", conc = FALSE)

pttrobo_print_filter <- function(x, conc = TRUE){
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
 cat(out)
 if (conc) cat(out, file = "clipboard-128")

}

#' @describeIn pttrobo_print_filter version for pttdatahaku::filter_recode()
#' @export
pttrobo_print_filter_recode <- function(x, conc = TRUE){
  if (!inherits(x, "robonomist_data")){
    x <- ptt_data_robo_l(x)
  }

  y <- lapply(x, function(x) {
    if (is.character(x) | is.factor(x)) {
      unique(x)
    }})

  y <- y[!unlist(lapply(y, is.null))]



  out <- paste0(
    "filter_recode(\n  ",
    paste0(purrr::imap(y, ~paste0(.y," = c(\"", paste0(as.character(.x), collapse = "\", \""), "\")")), collapse = ",\n  "),
    "\n  )"


  )
  cat(out)
  if (conc) cat(out, file = "clipboard-128")

}
