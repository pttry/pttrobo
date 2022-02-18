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
#' @param rangeslider A locigal for rangeslider
#' @param ... Additional arguments for ptt_plot.
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
                        title = "",
                        subtitle = "",
                        source = NULL,
                        caption = "",
                        rangeslider = FALSE,
                        ...){


  if ((is.null(caption) || caption == "" ) & !is.null(source)){
    caption <- paste0("Lähde: ", source, ", PTT")
  }


  dat |>
    ptt_plot(grouping = {{colour}},
             title = title, subtitle = subtitle, caption = caption,
             rangeslider = rangeslider, ...)
}

#' @describeIn aplot_lines Estimate and plot trend with original
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
                         title = "", subtitle = "",
                         source = NULL,
                         caption = "", ...){

  if ((is.null(caption) || caption == "" ) & !is.null(source)){
    caption <- paste0("Lähde: ", source, ", PTT")
  }

  dat <-
    dat |>
    droplevels() |>
    mutate(alk = {{y}}) |>
    group_by({{colour}}) |>
    mutate(value = statfitools::trend_series({{y}}, time, ...)) |>
    ungroup()

  tiedot_name <- rlang::enquo(colour)

  p <- ptt_plot(dat, grouping = {{colour}},
                title = title, subtitle = subtitle, caption = caption)

  print(names(p$color_vector))

  for(var in unique(dat[[as_name(tiedot_name)]])) {
    # print(as_name(tiedot_name))
    # print(var)
    sec.dat <- dat %>% filter(!!tiedot_name == var) %>% mutate(value = alk) %>%
      mutate(alk.sarja = "Alkuperäinen sarja")
    rel <- unique(sec.dat[[as_name(tiedot_name)]]) %>% as.character()
    p <- p|> ptt_plot_add_secondary_traces(sec.dat, !!rel, alk.sarja)
  }
  p
}
