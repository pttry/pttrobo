#' Parse PTT prediction data from excel to tibble format
#'
#' @param excel_path Path to the excel file containing prediction data
#' @return prediction data in tibble format
#' @examples
#' \dontrun{
#' excel_to_tibble("ennustedata.xlsx")
#' }
#' @export
excel_to_tibble <- function(excel_path) {
  # filling na rows
  d <- readxl::read_excel(excel_path)  %>%
    tidyr::fill(c(name, title, ylab, caption), .direction = "down")

  d %>%
    pivot_longer(!c(name, title, subtitle, ylab, caption, serie, serie_name, special), names_to = "time", values_to = "value")
}

#' Plot PTT prediction data in plotly format
#'
#' @param data_tibble data for plotting in tibble format
#' @return plotly object
#' @examples
#' \dontrun{
#' Filtering datatable that contains multiple different time series
#' d <- excel_to_tibble("ptt_ennusteet_testi.xlsx") %>% filter(name == "bkt_private_consumption")
#' Using plotting function with parameters
#' ptt_plot()$lines(d,
#'                 title = d$title,
#'                 lahde = d$caption,
#'                 yksikko = d$ylab)
#' }
#' @export
#' @importFrom plotly layout
#' @import plotly htmltools
ptt_plot <- function(){
  ptt_vihrea <- "#0B9D4A"
  ptt_sininen <- "#337ab7"
  ptt_white <- "white"

  ptt_dark_grey_custom <- "#8c8787"

  theme_ppt <- function() {
    theme_bw(base_size = 16,
             base_family = "sans-serif") +
      theme(text = element_text(color = ptt_vihrea),
            legend.position = "top",
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            ## axis.line.x = element_line(lineend = "butt", color = bf_gray),
            panel.grid.major.y = element_line(linetype = 1, color = ptt_dark_grey_custom, size = 0.5),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(linetype = 1, color = ptt_dark_grey_custom, size = 0.2),
            panel.grid.minor.x = element_blank(),
            legend.background = element_rect(fill = ptt_white),
            legend.margin = margin(),
            legend.text = element_text(size = rel(0.9)),
            axis.text = element_text(size = rel(0.9), color = ptt_vihrea),
            axis.ticks = element_blank(),
            axis.text.y = element_text(margin=unit(c(0.2, 0, 0, -0.5), "cm")),
            plot.title = element_text(hjust = 0, vjust = 1,
                                      margin = margin(b = 11/2), colour = ptt_vihrea, family = "Finlandica Bold"),
            plot.subtitle = element_text(margin = margin(b = 10), size = rel(0.9)),
            plot.caption = element_text(size = rel(0.9), margin=unit(c(0.0,0.0,0.0,0.0), "cm"), hjust = 0),
            plot.margin = margin(2, 5, 5, 2, "mm"),
            plot.caption.position = "plot",
            plot.title.position = "plot"
      )
  }

  # PLOTLY GRAPH MÄÄRITTELYT
  plotly_korkeus <- 400

  plot_lines <- function(d, title = d$title, subtitle = "", alaviite = "", lahde = d$caption,
                         source_y_adjustment = -0.06, source_x_adjustment = 0, legend_orientation = "h", x_legend = 0, y_legend = 0.98,
                         top_margin = 90, yksikko = d$ylab, bottom_margin = 65, # 75 lähtötilanne, nostettu to 85 from 75.  y_legend, nostettu 0.98 from 0.94
                         color_vector = c(ptt_vihrea, ptt_sininen),
                         rounding = 1,
                         grouping_variable = serie_name){
    grouping_variable <- enquo(grouping_variable)

    plotly::plot_ly(d, x = d$time, y = d$value, color = grouping_variable, text = grouping_variable) %>%
      add_lines(hovertemplate = paste0("%{text}<br>%{y:.", rounding, "f} ", yksikko, "<extra></extra>"), line = list(width = 3),
                colors = color_vector, mode ='lines') %>%
      layout(hovermode = "compare") %>%
      add_title(title, subtitle, top_margin = top_margin) %>%
      add_source(lahde, alaviite, source_y_adjustment = source_y_adjustment, source_x_adjustment = source_x_adjustment, padding = bottom_margin) %>%
      #add_fonts() %>%
      set_grid() %>%
      set_locale() %>%
      minimal_modebar() %>%
      zoom_off() %>%
      change_ticks() %>%
      sizing(width = "100%", height = plotly_korkeus)  %>%
      layout(legend = list(x= x_legend, y = y_legend, orientation =legend_orientation, xanchor = "left", yanchor = "bottom"),
             xaxis=list(tickfont=list(color=c(ptt_vihrea))),
             yaxis=list(tickfont=list(color=c(ptt_vihrea)),
                        tickformat = "digit" ),
             margin = list(l = 0),
             autosize = TRUE,
             dragmode = FALSE)
  }

  add_source <- function(p, text, alaviite, padding = 80, source_y_adjustment = -0.07, source_x_adjustment = 0) {
    if (alaviite != "") {
      text <- paste0(alaviite,"<br>", text)
    }
    layout(p,
           margin = list(b = padding, l = 0),
          annotations = list(x = source_x_adjustment, y = source_y_adjustment, text = text, align = "left",
                             showarrow = F, xref = 'paper', yref = "paper",
                             xanchor='left', yanchor = 'top', xshift=0, yshift=0,
                             font = list(size = 12, family = "finlandicaregular, Open sans",
                                         color = ptt_vihrea)))
  }

  add_custom_source <- function(p, text, padding = 0, size = 10) {
    layout(p,
           margin = list(b = padding, l = 0),
           annotations = list(x = 0, y = -0.07, text = text, align = "left",
                              showarrow = F, xref = 'paper', yref = "paper",
                              xanchor='left', yanchor = 'top', xshift=0, yshift=0,
                              font = list(size = size, family = "finlandicaregular, Open sans", color = ptt_vihrea)))
  }

  zoom_off <- function(p) {
    layout(p, xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE))
  }

  change_ticks <- function(p) {
    layout(p, yaxis = list(ticksuffix = " "))
  }

  minimal_modebar <- function(p) {
    robonomist <- list(
      name = "Powered by Robonomist",
      icon = list(name = "Robonomist",
                  svg = '<svg version="1.1" viewBox="0 0 71.447 32" xmlns="http://www.w3.org/2000/svg"><g id="XMLID_248_" transform="scale(.31159)"><polyline id="XMLID_132_" points="229.3 53.2 174.3 90.1 174.3 69.1 199.5 53.2 174.3 37.3 174.3 16.3"/><g id="XMLID_40_"><path id="XMLID_41_" d="m112 0c14.2 0 23.3 1.8 30.7 7 6.3 4.4 10.3 10.8 10.3 20.5 0 11.3-6.4 22.8-22.3 26.5l18.4 32.5c5 8.7 7.7 9.7 12.5 9.7v6.5h-27.3l-23.7-45.8h-7v27.6c0 10.5 0.7 11.7 9.9 11.7v6.5h-43.2v-6.7c10.3 0 11.3-1.6 11.3-11.9v-65.7c0-10.2-1-11.7-11.3-11.7v-6.7zm-4.8 7.9c-3.3 0-3.6 1.5-3.6 8.6v32.3h6.4c15.8 0 20.2-8.7 20.2-21.3 0-6.3-1.7-11.5-5-15-2.9-3-7-4.6-13-4.6z"/></g><polyline id="XMLID_130_" points="0 53.2 55 16.3 55 37.3 29.8 53.2 55 69.1 55 90.1"/></g></svg>'),
      click = htmlwidgets::JS("function(gd) {window.open(\"https://robonomist.com\")  }"))

    plotly::config(p, displaylogo = FALSE,
                   modeBarButtons = list(list("toImage", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines", robonomist)))
  }

  sizing <-function(p, width = NULL, height = NULL, ...) {
    p$sizingPolicy <- htmlwidgets::sizingPolicy(defaultWidth = width, defaultHeight = height, padding = 0, ...)
    p
  }

  add_title <- function(p, title, subtitle = "", x = "", y = "", top_margin = 75) {
    t <- list(family = "sans-serif", color = ptt_vihrea, size = 16)
    p <- layout(p, title = list(text = paste0("<b>", title, "</b>", "<br>", tags$sub(style=paste0("color: ", ptt_vihrea, "; fontFamily: finlandicaregular"), subtitle)),
                                font = t,
                                xanchor = "left", x = 0, xref = "container"),
                margin = list(t = top_margin))
    layout(p,
           xaxis = list(title = x),
           yaxis = list(title = y))
  }

  add_fonts <- function(p) {
    dep <- htmlDependency("style", "0.1", src = c(href= getwd()),  stylesheet = "style.css")
    p$dependencies <- c(p$dependencies, list(dep))
    attachDependencies(p, dep)
    layout(p, font = list(family = "sans-serif"))
  }

  set_grid <- function(p) {
    layout(p,
           xaxis = list(showgrid = TRUE, gridcolor = ptt_dark_grey_custom),
           yaxis = list(showgrid = TRUE, gridcolor = ptt_dark_grey_custom))
  }

  set_locale <- function(p) {
    plotly::config(p, locale = "fi") %>%
      layout(separators = ", ")

  }

  list("lines" = plot_lines)
}

#' Plot PTT prediction data in plotly format from chosen file with specific time series
#'
#' @param excel_path Excel file's path that contains prediction data
#' @param  time_series_name Name of time series to be plotted
#' @return plotly object
#' @examples
#' ptt_plot_from("ptt_ennusteet_testi.xlsx", "bkt_private_consumption")
#' @export
ptt_plot_from <- function(excel_path, time_series_name){
  d <- excel_to_tibble(excel_path) %>% filter(name == time_series_name)

  ptt_plot()$lines(d,
                   title = d$title,
                   lahde = d$caption,
                   yksikko = d$ylab)
}

