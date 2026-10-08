#' Print retrieval code and full filtering for a Tosi table or URL
#'
#' @param x Tosi table identifier or supported source-table URL.
#' @param conc A locigal to copy to clipboard.
#'
#' @export
#'
#' @examples
#'   pttrobo_print_code("https://pxweb2.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__vaerak/statfin_vaerak_pxt_11ra.px/", conc = FALSE)
#'   pttrobo_print_code("statfin/vaerak/11ra.px", conc = FALSE)
#'
pttrobo_print_code <-
  function(x, conc = TRUE){

    if (grepl("^https?://", x)) {
      data <- tosi::tosi_url(x)
      if (!tosi::is_tosi_table(data)) stop("URL must identify a Tosi observation table.")
      data <- statfitools::clean_names(ptt_tosi_table(data))
    } else {
      data <- ptt_data_robo_l(x)
    }
    id <- attr(data, "tosi_id")

    out <- paste0(
      "ptt_data_robo(\"", id, "\") |>\n  ",
      pttrobo_print_filter_recode(data, conc = FALSE, print = FALSE)

    )
    cat(out)
    if (conc) cat(out, file = "clipboard-128")

  }


#' Print full filtering for a Tosi table identifier or dataframe
#'
#' In ptt-format
#'
#' @param x A Tosi table identifier or dataframe.
#' @param conc A locigal whether to copy in clipboard
#' @param print A locigal whether to print output (to only return invisibly)
#' @export
#' @examples
#'   pttrobo_print_filter(x = "luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/08_Tuottajahinnat_Vilja_rypsi_rapsi_v.px", conc = FALSE)
#'   pttrobo_print_filter_recode(x = "luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/08_Tuottajahinnat_Vilja_rypsi_rapsi_v.px", conc = FALSE)

pttrobo_print_filter <- function(x, conc = TRUE, print = TRUE){
  if (!is.data.frame(x)){
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
  if (!is.data.frame(x)){
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
