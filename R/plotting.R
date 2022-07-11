#' Basic PTT line plot
#'
#' @param dat A data.frame to plot.
#' @param x, y Variables for x- and y-axis.
#' @param colour A variable for colours
#' @param size A variable for size. Not in use
#' @param y2 An optional y-variable for narrow line (forexample original in a trend plot).
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
#' library(pttdatahaku)
#' library(tidyverse)
#' ptt_data_robo_l("StatFin/vtp/statfin_vtp_pxt_11sf.px") |>
#' dplyr::filter(
#'     taloustoimi %in% c("P7R Tavaroiden ja palvelujen tuonti, tulona",
#'     "P6K Tavaroiden ja palvelujen vienti, menona"),
#'     tiedot == c("Volyymin muutokset, %")) |>
#'     aplot_lines(colour = taloustoimi,
#'                 title = "Vienti ja tuonti",
#'                 subtititle = "%, volyymin muutos",
#'                 source = "Tilastokeskus")
#'
#' ptt_data_robo("StatFin/ttvi/statfin_ttvi_pxt_13bh.px") |>
#' filter_recode(toimiala_tol_2008 = c("BCD Koko teollisuus", "19-22 Kemianteollisuus"),
#'        tiedot = c(indeksi = "Työpäiväkorjattu indeksisarja", trendi = "Trendisarja")) |>
#'   group_by(toimiala_tol_2008, tiedot) |>
#'   mutate(value = 100 * (value / lag(value, 12) -1)) |>
#'   ungroup() |>
#'   spread(tiedot, value) |>
#'   filter(time >= "2018-01-01") |>
#'   aplot_lines(colour = toimiala_tol_2008, y = trendi,y2 = indeksi,
#'             title = "Teollisuustuotannon volyymi",
#'             subtitle = "%-muutos, trendi ja työpäiväkorjattu")
#'
#'
aplot_lines <- function(dat, x = time, y = value,
                        colour = tiedot, size = NULL,
                        y2,
                        title = "",
                        subtitle = "",
                        source = NULL,
                        caption = "",
                        rangeslider = FALSE,
                        # start_time = NULL,
                        ...){


  if ((is.null(caption) || caption == "" ) & !is.null(source)){
    caption <- paste0("L\u00e4hde: ", source, ", PTT")
  }

  dat <-
    dat |>
    droplevels() |>
    mutate(value = {{y}},
           time = {{x}})

  p <- dat |>
    ptt_plot(grouping = {{colour}},
             title = title, subtitle = subtitle, caption = caption,
             rangeslider = rangeslider,
             #start_time = start_time,
             ...)

  if (!missing(y2)) {

    tiedot_name <- rlang::enquo(colour)


    dat <-
      dat |>
      droplevels() |>
      mutate(value = {{y2}},
             grouping = {{colour}})

    for(var in unique(dat[[as_name(tiedot_name)]])) {


      sec.dat <- dat %>%
        filter(!!tiedot_name == var)

      rel <- unique(sec.dat[[as_name(tiedot_name)]]) %>%
        as.character()

      p <- p|>
        ptt_plot_add_secondary_traces(sec.dat, !!rel, grouping,
                                      showlegend = FALSE)

    }}

  p
}

#' @describeIn aplot_lines Estimate and plot trend with original
#' @param org_showlegend A logical to show legend for original data.
#' @param trends_only A logical to have only trends in plot.
#' @export
#' @examples
#' ptt_data_robo_l("StatFin/ntp/statfin_ntp_pxt_132h.px") |>
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
                         caption = "",
                         rangeslider = FALSE,
                         # start_time = NULL,
                         org_showlegend = FALSE,
                         trends_only = FALSE,
                         ...){

  if ((is.null(caption) || caption == "" ) & !is.null(source)){
    caption <- paste0("L\u00e4hde: ", source, ", PTT")
  }

  # Fix to filter ... for trend_series
  lst <- list(...)
  trend_lst <- lst[c("x11")]

  dat <-
    dat |>
    droplevels() |>
    mutate(alk = {{y}}) |>
    group_by({{colour}}) |>
    # see fix above
    mutate(value = do.call(statfitools::trend_series , args = purrr::discard(c(quote({{y}}), quote({{x}}), trend_lst), is.null))) |>
    ungroup() |>
    drop_na(value)
  tiedot_name <- rlang::enquo(colour)

  p <- ptt_plot(dat, grouping = {{colour}},
                title = title, subtitle = subtitle, caption = caption,
                rangeslider = rangeslider)
  # print(names(p$color_vector))

  if (!trends_only) {
    for(var in unique(dat[[as_name(tiedot_name)]])) {
    # print(as_name(tiedot_name))
    # print(var)
    sec.dat <- dat %>% filter(!!tiedot_name == var) %>% mutate(value = alk) %>%
      mutate(alk.sarja = "Alkuper\u00e4inen sarja")
    rel <- unique(sec.dat[[as_name(tiedot_name)]]) %>% as.character()
    p <- p|>
      ptt_plot_add_secondary_traces(sec.dat, !!rel, alk.sarja,
                                    showlegend = org_showlegend)
  }}
  p
}
