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

#' @describeIn aplot_lines
#' @export
#' @examples
#' ptt_data_robo_l("StatFin/kan/ntp/statfin_ntp_pxt_132h.px") |>
#' dplyr::filter(
#'     taloustoimi %in% c("P7R Tavaroiden ja palvelujen tuonti, tulona",
#'     "P6K Tavaroiden ja palvelujen vienti, menona"),
#'     tiedot == c("Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa")) |>
#'     aplot_trends(colour = taloustoimi,
#'                 title = "Vienti ja tuonti",
#'                 subtititle = "%, volyymin muutos",
#'                 source = "Tilastokeskus")

aplot_trends <- function(dat, x = time, y = value,
                         colour = tiedot, size = NULL,
                         title = NULL, subtitle = NULL,
                         source = NULL,
                         caption = NULL, ...){
  piirtaja <- ptt_plot()

  dat <-
    dat |>
    droplevels() |>
    mutate(alk = {{y}}) |>
    group_by({{colour}}) |>
    mutate(value = statfitools::trend_series({{y}}, time)) |>
    ungroup()

  tiedot_name <- rlang::enquo(colour)

  piirtaja$lines(dat, grouping_variable = {{colour}},
                 title = title, subtitle = subtitle, lahde = caption,
                 yksikko = NULL, legendgroup = tiedot_name) |>
    plotly::add_lines(y = dat$alk,
                      size = I(0.5),
                      legendgroup = tiedot_name,
                      showlegend = FALSE
    )
}
