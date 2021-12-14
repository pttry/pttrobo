#' Plot data with plotly
#'
#' @param data_tibble data for plotting in tibble format
#' @return plotly object
#' @examples
#' \dontrun{
#' ptt_plot()$lines(d,
#'                 title = "otsikko",
#'                 lahde = "lähde",
#'                 yksikko = "yksikkö")
#' }
#' @export
#' @importFrom plotly layout
#' @import plotly htmltools
ptt_plot <- function(){
  ptt_vihrea <- "#5B8233"
  ptt_sininen <- "#2f7ab9"
  ptt_ruskea <- "#C36B0D"
  ptt_keltainen <- "#FFCC00"
  ptt_white <- "white"

  ptt_dark_grey <- "#696969"
  ptt_light_grey <- "#E8E8E8"

  theme_ppt <- function() {
    theme_bw(base_size = 16,
             base_family = "sans-serif") +
      theme(text = element_text(color = ptt_dark_grey),
            legend.position = "top",
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            ## axis.line.x = element_line(lineend = "butt", color = bf_gray),
            panel.grid.major.y = element_line(linetype = 1, color = ptt_light_grey, size = 1.5),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(linetype = 1, color = ptt_light_grey, size = 1.5),
            panel.grid.minor.x = element_blank(),
            legend.background = element_rect(fill = ptt_white),
            legend.margin = margin(),
            legend.text = element_text(size = rel(0.9)),
            axis.text = element_text(size = rel(0.9), color = ptt_dark_grey),
            axis.ticks = element_blank(),
            axis.text.y = element_text(margin=unit(c(0.2, 0, 0, -0.5), "cm")),
            plot.title = element_text(hjust = 0, vjust = 1,
                                      margin = margin(b = 11/2), colour = ptt_dark_grey, family = "Finlandica Bold"),
            plot.subtitle = element_text(margin = margin(b = 10), size = rel(0.9)),
            plot.caption = element_text(size = rel(0.9), margin=unit(c(0.0,0.0,0.0,0.0), "cm"), hjust = 0),
            plot.margin = margin(2, 5, 5, 2, "mm"),
            plot.caption.position = "plot",
            plot.title.position = "plot"
      )
  }

  # PLOTLY GRAPH MÄÄRITTELYT
  plotly_korkeus <- 400

  add_source <- function(p, text, alaviite, padding = 60, source_y_adjustment = -0.07, source_x_adjustment = 0) {
    if (alaviite != "") {
      text <- paste0(alaviite,"<br>", text)
    }
    layout(p,
           margin = list(b = padding, l = 0),
          annotations = list(x = source_x_adjustment, y = source_y_adjustment, text = text, align = "left",
                             showarrow = F, xref = 'paper', yref = "paper",
                             xanchor='left', yanchor = 'top', xshift=0, yshift=0,
                             font = list(size = 12, family = "finlandicaregular, Open sans",
                                         color = ptt_dark_grey)))
  }

  add_custom_source <- function(p, text, padding = 0, size = 10) {
    layout(p,
           margin = list(b = padding, l = 0),
           annotations = list(x = 0, y = -0.07, text = text, align = "left",
                              showarrow = F, xref = 'paper', yref = "paper",
                              xanchor='left', yanchor = 'top', xshift=0, yshift=0,
                              font = list(size = size, family = "finlandicaregular, Open sans", color = ptt_dark_grey)))
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
    t <- list(family = "sans-serif", color = ptt_dark_grey, size = 16)
    p <- layout(p, title = list(text = paste0("<b>", title, "</b>", "<br>", tags$sub(style=paste0("color: ", ptt_dark_grey, "; fontFamily: finlandicaregular"), subtitle)),
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
           xaxis = list(showgrid = TRUE, gridcolor = ptt_light_grey, size = 1.5),
           yaxis = list(showgrid = TRUE, gridcolor = ptt_light_grey, size = 1.5))
  }

  set_locale <- function(p) {
    plotly::config(p, locale = "fi") %>%
      layout(separators = ", ")

  }

  plot_lines <- function(d, grouping_variable, title = "", subtitle = "", alaviite = "", lahde = "",
                         source_y_adjustment = -0.22, source_x_adjustment = 0, legend_orientation = "h", x_legend = 0, y_legend = -0.17,
                         top_margin = 80, yksikko = "%", bottom_margin = 85,
                         color_vector = c(ptt_vihrea, ptt_sininen, ptt_ruskea, ptt_keltainen),
                         rounding = 1
                         ){

    if(missing(grouping_variable)){
      stop("Grouping variable for the data needed (without quotes)\nFor example: plot_lines(time_series_data, grouping_variable=Alue)")
    }

    grouping_variable <- enquo(grouping_variable)

    plotly::plot_ly(d, x = d$time, y = d$value, color = grouping_variable, text = grouping_variable) %>%
      add_lines(hovertemplate = paste0("%{text}<br>%{y:.", rounding, "f} ", yksikko, "<extra></extra>"), line = list(width = 4),
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
      layout(legend = list(x= x_legend, y = y_legend, orientation = legend_orientation, xanchor = "left", yanchor = "bottom"),
             xaxis=list(tickfont=list(color=c(ptt_dark_grey)),
                        mirror=TRUE,
                        ticks='outside',
                        showline= TRUE),
             yaxis=list(tickfont=list(color=c(ptt_dark_grey)),
                        tickformat = "digit",
                        mirror=TRUE,
                        ticks='outside',
                        showline= TRUE),
             margin = list(l = 0),
             autosize = TRUE,
             dragmode = FALSE)
  }

  plot_line <- function(d, title = "", subtitle = "", alaviite = "", lahde = "",
                        source_y_adjustment = -0.12, source_x_adjustment = 0, legend_orientation = "h", x_legend = 0, y_legend = -0.14,
                        top_margin = 80, yksikko = "€", bottom_margin = 60,
                        color_vector = ptt_vihrea,
                        rounding = 1){
    plotly::plot_ly(d, x = d$time, y = d$value) %>%
      add_lines(hovertemplate = paste0("%{y:.", rounding, "f} ", yksikko, "<extra></extra>"), line = list(color = color_vector, width = 4),
                mode ='lines') %>%
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
      layout(legend = list(x= x_legend, y = y_legend, orientation =legend_orientation, xanchor = "left", yanchor = "top"),
             xaxis=list(tickfont=list(color=c(ptt_dark_grey)),
                        mirror=TRUE,
                        ticks='outside',
                        showline= TRUE),
             yaxis=list(tickfont=list(color=c(ptt_dark_grey)),
                        tickformat = "digit",
                        mirror=TRUE,
                        ticks='outside',
                        showline= TRUE),
             margin = list(l = 0),
             autosize = TRUE,
             dragmode = FALSE)
  }

  list(
    "line" = plot_line,
    "lines" = plot_lines
  )
}

