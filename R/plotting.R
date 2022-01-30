#' Basic PTT line plot
#'
#' @param dat A data.frame to plot.
#' @param colour A variable for colours
#' @param size A variable for size.
#' @param title A plot title
#' @param subtitle A plot subtitle, use also for y-axis label.
#' @param source A source information for caption. Text is added to
#'               "Lähde: ..., PTT".
#' @param caption A caption text. Will override source
#'
#' @export
#' @examples
#' ptt_data_robo_l("StatFin/kan/vtp/statfin_vtp_pxt_11sf.px") |>
#' dplyr::filter(
#'     taloustoimi %in% c("P7R Tavaroiden ja palvelujen tuonti, tulona",
#'     "P6K Tavaroiden ja palvelujen vienti, menona"),
#'     tiedot == c("Volyymin muutokset, %")) |>
#'     aplot_lines(colour = taloustoimi,
#'                 title = "Vienti ja tuonti",
#'                 subtititle = "%, volyymin muutos",
#'                 source = "Tilastokeskus")
#'
#'
aplot_lines <- function(dat, x = time, y = value,
                        colour = tiedot, size = NULL,
                      title = NULL, subtitle = NULL,
                      source = NULL,
                      caption = NULL, ...){
  piirtaja <- ptt_plot()

  if (is.null(caption) & !is.null(source)){
    caption <- paste0("Lähde: ", source, ", PTT")
  }

    dat |>
    piirtaja$lines(grouping_variable = {{colour}},
                   title = title, subtitle = subtitle, lahde = caption,
                   yksikko = NULL, ...)
}
