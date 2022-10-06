#' Add secondary traces of existing traces with reduced linewidth to an existing ptt_plot.
#'
#' Outputs a plotly object.
#'
#' @param p Plotly object created with ptt_plot to add prediction traces to.
#' @param secondary_data Tibble with secondary variables of one grouping in the parent ptt_plot object.
#' @param grouping Tibble column used for grouping in plot, should be labeled the same as data used for parent ptt_plot.
#' @param name The name of the data in the secondary data.
#' @param line_width A width of the line.
#' @param hovertext Uses parent ptt_plot specification if undefined. A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)", dateformat = "%Y-%m-&d")".
#' @param showlegend A locigal to show legend for secondary trace.
#' @param grouplegend A locigal to tie legend grouping to grouping in p.
#' @return plotly object
#' @export
#' @importFrom tidyr uncount pivot_longer
#' @importFrom dplyr slice_tail
#' @importFrom forcats fct_inorder
#' @importFrom plotly plot_ly add_lines
#' @importFrom stringr str_replace
#'
#' @examples
#' library(tidyverse)
#' library(pttdatahaku)
#' dat <- ptt_data_robo("StatFin/ntp/statfin_ntp_pxt_132h.px") |>
#'   filter_recode(
#'     taloustoimi = c(
#'       "BKT" = "B1GMH Bruttokansantuote markkinahintaan",
#'       "Vienti" = "P61K Tavaroiden vienti, menona"),
#'     tiedot = c(
#'      kausitasoitettu = "Kausitasoitetun ja työpäiväkorjatun sarjan volyymin muutos vuodentakaisesta, %",
#'      trendi = "Trendisarjan volyymin muutos vuodentakaisesta, %")) |>
#'  spread(tiedot, value)
#' dat |>
#'   rename(value = trendi) |>
#'   ptt_plot(grouping = taloustoimi,
#'            title = "BKT ja vienti",
#'            subtitle = "%, volyymin vuosimuutos",
#'            caption = "Lähde: Tilastokeskus, PTT") |>
#'   ptt_plot_add_secondary_traces2(dat, y = kausitasoitettu, grouping = taloustoimi)
#'
#'   dat |>
#'   rename(value = trendi) |>
#'   ptt_plot(grouping = taloustoimi,
#'            title = "BKT ja vienti",
#'            subtitle = "%, volyymin vuosimuutos, trendi ja kausitasoitettu",
#'            caption = "Lähde: Tilastokeskus, PTT") |>
#'   ptt_plot_add_secondary_traces2(dat, y = kausitasoitettu, grouping = taloustoimi,
#'   name = "kausitasoitettu", showlegend = TRUE, grouplegend = FALSE)

ptt_plot_add_secondary_traces2 <-
  function(p, secondary_data, y = value, grouping, name = NULL,
           line_width = 2,
           hovertext,
           showlegend = FALSE,
           grouplegend = TRUE) {

    if(!grouplegend & !showlegend) { message("If grouplegend = FALSE, showlegend should usually be TRUE") }




    if (missing(grouping) | missing(secondary_data)) {
      stop("Define the relation to parent ptt_plot by providing both the data with the secondary data and the grouping variable.", call. = F)
    }

    grouping <- enquo(grouping)
    y <- enquo(y)

    # relates_to <- as_name(enquo(relates_to))

    d <- droplevels(secondary_data)
    d[["value"]] <- d[[as_name(y)]]


    # print(str(grouping))
    unique_groups <- d[[as_name(grouping)]] |> unique() |> as.character()
    # print(str(unique_groups))
    if(!all(unique_groups %in% names(p$color_vector))) {
      stop("Provided relates_to not in parent ptt_plot variables!", call. = F)
    }

    if (missing(hovertext)) { hovertext <- p$hover_template }





    if(!is.factor(d[[as_name(grouping)]])) {
      d[[as_name(grouping)]] <- fct_inorder(d[[as_name(grouping)]])
    }



    split_d <- group_split(d, !!grouping) |> rev()
    for (g in split_d) {

      g.name <- unique(g[[as_name(grouping)]]) |> as.character()
      g.level <- which(g.name == levels(g.name))
      # lw <- seq.int(2,1,length.out = length(levels(g.name)))[g.level]
      g.name_legend <- str_c(g.name, ", ", name)
      legend.rank <- p$legend_ranks[g.name] + 10

      p <-
        p |>
        add_trace(data=g, y = ~value,
                  text = g.name_legend,
                  hovertemplate = ptt_plot_hovertemplate(hovertext),
                  line = list(width = line_width),
                  legendgroup = if (grouplegend) g.name else "",
                  legendrank = legend.rank,
                  showlegend = showlegend,
                  name = g.name_legend,
                  color = I(p$color_vector[g.name]), type = "scatter", mode ='lines'
        )
    }

    if (is.null(name)) name <- "_2"
    sec_d <- map_dfr(split_d, ~ .x) |>
      rename(csv.data.tiedot = !! rlang::sym(rlang::quo_name(grouping))) |>
      select(csv.data.tiedot, time, value) |>
      mutate(
        csv.data.tiedot = str_c(csv.data.tiedot,", ", name),
        across(everything(), ~ as.character(.x)),
        value = str_replace(value, "\\.",",")
      )
    p$data <- bind_rows(p$data, sec_d) |> arrange(time, csv.data.tiedot)
    p |> ptt_plot_set_modebar(p$title, p$subtitle, p$png_attrs, T)
  }
