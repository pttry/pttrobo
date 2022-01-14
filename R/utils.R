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
#' @import plotly htmltools RCurl lubridate
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

  add_logo <- function(p, y_height=-0.35){
    image_file <- paste0(getwd(), "/ptt-logo.png")
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

    plotly::layout(p,
                   images = list(
                     source = paste('data:image/png;base64', txt, sep=','),
                     xref= "paper", yref="paper",
                     x=1, y=y_height,
                     sizex = 0.05, sizey = 0.05,
                     xanchor="right", yanchor = "bottom"
                   )
    )
  }

  plot_lines <- function(d, grouping_variable, title = "", subtitle = "", alaviite = "", lahde = "",
                         source_y_adjustment = -0.30, source_x_adjustment = 0, legend_orientation = "h", x_legend = 0, y_legend = -0.30,
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
             dragmode = FALSE) %>%
      add_logo()
  }

  plot_line <- function(d, title = "", subtitle = "", alaviite = "", lahde = "",
                        source_y_adjustment = -0.25, source_x_adjustment = 0, legend_orientation = "h", x_legend = 0, y_legend = -0.14,
                        top_margin = 80, yksikko = "€", bottom_margin = 85,
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
             dragmode = FALSE) %>%
      add_logo()
  }


  plot_line_with_preds <- function(d, excel_path, serie_name, title = "", subtitle = "", alaviite = "", lahde = "",
                                   source_y_adjustment = -0.12, source_x_adjustment = 0, legend_orientation = "h", x_legend = 0, y_legend = -0.14,
                                   top_margin = 80, yksikko = "", bottom_margin = 60,
                                   color_vector = ptt_vihrea,
                                   rounding = 1){

    ennuste_color <- "rgba(91, 130, 51, 0.5)"
    # jos värinä sininen, ennusteväriksi läpinäkyvä vihreä.
    if (color_vector == "#2f7ab9"){
      ennuste_color <- "rgba(47, 122, 185, 0.5)"
    }

    prediction_data <- readxl::read_excel(excel_path) %>%
      filter(str_detect(sarja, !!serie_name))

    sarjan_nimi <- prediction_data$sarja_nmi

    last_two_preds <- prediction_data[,(ncol(prediction_data)-1): ncol(prediction_data)]

    create_ennuste_trace <- function(time, value){
      times <- c(as.Date(paste0(time, "-02-01")), as.Date(paste0(time, "-11-01")))
      values <- c(value, value)
      tibble(
        time = times,
        value = values
      )
    }

    second_last_prediction <-
      list(
        time = names(last_two_preds) %>% first(),
        value = last_two_preds %>% first()
      )

    last_prediction <-
      list(
        time = names(last_two_preds) %>% last(),
        value = last_two_preds %>% last()
      )

    d_last_pred <- create_ennuste_trace(last_prediction$time, last_prediction$value)
    d_second_last_pred <- create_ennuste_trace(second_last_prediction$time, second_last_prediction$value)

    plotly::plot_ly(d, x = d$time, y = d$value, name=sarjan_nimi) %>%
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
      add_logo() %>%
      add_trace(x = d_second_last_pred$time,
                y = d_second_last_pred$value,
                mode='lines',
                line = list(color = ennuste_color, width = 4),
                name=paste0("Ennuste ", d_second_last_pred$time %>% lubridate::year() %>% first()),
                hovertemplate = paste0("%{y:.", rounding, "f} ", yksikko, "<br>", d_second_last_pred$time %>% lubridate::year() %>% first(),
                                       " vuosiennuste", "<extra></extra>")) %>%
      add_trace(x = d_last_pred$time,
                y = d_last_pred$value,
                mode='lines',
                line = list(color = ennuste_color, width = 4),
                name =paste0("Ennuste ", d_last_pred$time %>% lubridate::year() %>% first()),
                hovertemplate = paste0("%{y:.", rounding, "f} ", yksikko, "<br>", d_last_pred$time %>% lubridate::year() %>% first(),
                                       " vuosiennuste", "<extra></extra>")) %>%
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
             dragmode = FALSE
      )
  }

  ennusteet_from_excel <- function(excel_path, serie_name){
    prediction_data <- readxl::read_excel(excel_path) %>%
      filter(str_detect(sarja, !!serie_name))

    last_two_preds <- prediction_data[,(ncol(prediction_data)-1): ncol(prediction_data)]

    create_ennuste_trace <- function(time, value){
      times <- c(as.Date(paste0(time, "-02-01")), as.Date(paste0(time, "-11-01")))
      values <- c(value, value)
      tibble(
        time = times,
        value = values
      )
    }

    second_last_prediction <-
      list(
        time = names(last_two_preds) %>% first(),
        value = last_two_preds %>% first()
      )

    last_prediction <-
      list(
        time = names(last_two_preds) %>% last(),
        value = last_two_preds %>% last()
      )

    d_last_pred <- create_ennuste_trace(last_prediction$time, last_prediction$value)
    d_second_last_pred <- create_ennuste_trace(second_last_prediction$time, second_last_prediction$value)
    list("viimeisin_ennuste" = d_last_pred,
         "toiseksi_viimeisin_ennuste" = d_second_last_pred)
  }

  labels_from_excel <- function(excel_path, serie_name){
    prediction_data <- readxl::read_excel(excel_path) %>%
      fill(c(name,title,ylab,caption), .direction = "down") %>%
      filter(str_detect(serie, !!serie_name))

    list(
      "title" = prediction_data$title,
      "subtitle" = prediction_data$subtitle,
      "ylab" = prediction_data$ylab,
      "caption" = prediction_data$caption
    )
  }

  serie_name_from_excel <- function(excel_path, serie_name){
    prediction_data <- readxl::read_excel(excel_path) %>%
      filter(str_detect(sarja, !!serie_name))

    prediction_data$sarja_nmi
  }

  plot_2_series_with_preds <- function(labels, d1, d1_ennusteet, serie_name_1, d2, d2_ennusteet, serie_name_2,
                                       dataspec_1 = NULL, dataspec_2 = NULL, ennuste_path = NULL, from_year = NULL,
                                       alaviite = "",
                                       source_y_adjustment = -0.22, source_x_adjustment = 0, legend_orientation = "h", x_legend = 0, y_legend = -0.12,
                                       top_margin = 80, bottom_margin = 85,
                                       rounding = 1){

    color_vector  <- c(ptt_vihrea, ptt_sininen)
    color_ennusteet <- c("rgba(91, 130, 51, 0.7)", "rgba(47, 122, 185, 0.7)")

    if(is.na(labels$subtitle)){
      labels$subtitle <- ""
    }

    # jos määritelty dataspec_1, 2 sekä ennuste path, käytetään näitä
    if (!(is.null(dataspec_1) || is.null(dataspec_2) || is.null(ennuste_path))){
      # dataspec 1 perusteellla määrittelyt
       d1 <- dataspec_1$data
       # tehdään muunnos jos määritelty
       if (!is.null(dataspec_1$muunnos)){
         if(dataspec_1$muunnos == "vuosimuutos"){
           d1 <- d1 %>% yearly_change()
         }
       }
       serie_name_1 <- dataspec_1$serie_name
       d1_ennusteet <- ennuste_time_serie_from_excel(ennuste_path, dataspec_1$datahaku_id)

       # dataspec_2 perusteella määrittelyt
       d2 <- dataspec_2$data
       # tehdään muunnos jos määritelty
       if (!is.null(dataspec_2$muunnos)){
         if(dataspec_2$muunnos == "vuosimuutos"){
           d2 <- d2 %>% yearly_change()
         }
       }
       serie_name_2 <- dataspec_2$serie_name
       d2_ennusteet <- ennuste_time_serie_from_excel(ennuste_path, dataspec_2$datahaku_id)
    }

    if(!is.null(from_year)){
      d1 <- d1 %>% filter(lubridate::year(time) >= from_year)
      d2 <- d2 %>% filter(lubridate::year(time) >= from_year)
    }

    add_two_latest_ennuste_traces <- function(p, ennuste_datat, color_selection){
      p %>%
        add_trace(x = ennuste_datat$time,
                y = ennuste_datat$value,
                mode='lines',
                line = list(color = color_ennusteet[color_selection], width = 3),
                name=paste0("Ennuste"), #", ennuste_datat$viimeisin_ennuste$time %>% lubridate::year() %>% first()),
                hovertemplate = paste0("%{y:.", rounding, "f} ", labels$ylab, "<br>", "%{x|%Y}",
                                       " vuosiennuste", "<extra></extra>"))
    }

    plotly::plot_ly() %>%
      #plotly::plot_ly(d, x = d$time, y = d$value, name=sarjan_nimi) %>%
      #  add_lines(hovertemplate = paste0("%{y:.", rounding, "f} ", yksikko, "<extra></extra>"), line = list(color = color_vector, width = 4),
      #            mode ='lines') %>%
      layout(hovermode = "compare") %>%
      add_title(labels$title, labels$subtitle, top_margin = top_margin) %>%
      add_source(labels$caption, alaviite, source_y_adjustment = source_y_adjustment, source_x_adjustment = source_x_adjustment, padding = bottom_margin) %>%
      #add_fonts() %>%
      set_grid() %>%
      set_locale() %>%
      minimal_modebar() %>%
      zoom_off() %>%
      change_ticks() %>%
      sizing(width = "100%", height = plotly_korkeus)  %>%
      add_logo(y_height=-0.25) %>%
      add_trace(x = d1$time,
                y = d1$value,
                mode ='lines',
                line = list(color = color_vector[1], width = 4),
                name = serie_name_1,
                hovertemplate = paste0("%{y:.", rounding, "f} ", labels$ylab, "<extra></extra>")
      ) %>%
      add_two_latest_ennuste_traces(d1_ennusteet, color_selection = 1) %>%
      add_trace(x = d2$time,
                y = d2$value,
                mode ='lines',
                line = list(color = color_vector[2], width = 4),
                name = serie_name_2,
                hovertemplate = paste0("%{y:.", rounding, "f} ", labels$ylab, "<extra></extra>")
      ) %>%
      add_two_latest_ennuste_traces(d2_ennusteet, color_selection = 2) %>%
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
             dragmode = FALSE
      )
  }
  list(
    "line" = plot_line,
    "lines" = plot_lines,
    "line_with_preds" = plot_line_with_preds,
    "two_lines_with_preds" = plot_2_series_with_preds,
    "ennusteet_from_excel" = ennusteet_from_excel,
    "labels_from_excel" = labels_from_excel
  )
}

#' Read output kuvio yaml file, containing specification for picture. Can handle up to 3 different data spesifications.
#'
#' Output list containing data for plotting and label
#'
#' @param file Path to yaml file
#' @return list object
#' @examples
#' \dontrun{
#'yaml_to_plotly_data(file = system.file("ennustekuvat/test_without_ennusteet.yaml", package="pttrobo"),
#'                    kuvion_nimi = "bkt_ja_kulutus")
#'}
#' @export
#' @import yaml rlang
yaml_to_plotly_data <- function(file, kuvion_nimi) {
  y <- yaml::read_yaml(file) %>% .[[kuvion_nimi]]


  get_data <- function(y, sarja_nro){
    d_specs <- y$sarjat[[sarja_nro]]$robonomist_data

    d <- robonomistClient::data_get(d_specs$id, tidy_time=TRUE)
    for(name in names(d_specs$Tiedot)){
      d <- d |>
        filter(d[name] == d_specs$Tiedot[name][[1]])
    }
    d
  }

  id_from_yaml <- function(y, sarja_nro){
    y$sarjat[[sarja_nro]]$ennuste_id
  }

  datas <- list()
  length_datas <- y$sarjat %>% length()
  for(i in 1:length_datas){
    listan_nimi <- paste0("data_",i)
    listan_objekti <- list(
      "data" = get_data(y,i),
      "serie_name" = y$sarjat[[i]]$nimi,
      "muunnos" = y$sarjat[[i]]$robonomist_data$muunnos,
      "datahaku_id" = id_from_yaml(y, i)
    )
    lista_objekti <- rlang::list2(!!listan_nimi := listan_objekti)
    datas <- c(datas, lista_objekti)
  }

  return(
    list(
      "datas" = datas,
      labels = list(
        "title" = y$otsikko,
        "subtitle" = y$alaotsikko, # toimii y-akselin selitteenä
        "ylab"= y$alaotsikko,
        "caption" = y$viite
      )
    )
  )
}

#' Read ennustedata from excel file and put it into plotly friendly time serie
#'
#' @param excel_path Path to excel file
#' @param serie_name name of the serie/identifier
#'
#' @return list object containing last two predictions for the serie
#' @examples
#' \dontrun{
#'ennusteet_from_excel(excel_path = "ptt_ennusteet_KT.xlsx",
#'                    serie_name = "StatFin/kan/ntp/statfin_ntp_pxt_132h.px§B1GMH§kausitvv2015")
#' }
#' @export
#' @import readxl
ennuste_time_serie_from_excel <- function(excel_path, serie_name){
  prediction_data <- readxl::read_excel(excel_path) %>%
    filter(sarja ==serie_name)

  last_two_preds <- prediction_data[,(ncol(prediction_data)-1): ncol(prediction_data)]

  create_ennuste_trace <- function(time, value){
    times <- c(as.Date(paste0(time, "-02-01")),
               as.Date(paste0(time, "-03-01")),
               as.Date(paste0(time, "-04-01")),
               as.Date(paste0(time, "-05-01")),
               as.Date(paste0(time, "-06-01")),
               as.Date(paste0(time, "-07-01")),
               as.Date(paste0(time, "-08-01")),
               as.Date(paste0(time, "-09-01")),
               as.Date(paste0(time, "-10-01")),
               as.Date(paste0(time, "-11-01")),
               NA,
               NA)
    values <- rep(value,12)
    tibble(
      time = times,
      value = values
    )
  }

  second_last_prediction <-
    list(
      time = names(last_two_preds) %>% first(),
      value = last_two_preds %>% first()
    )

  last_prediction <-
    list(
      time = names(last_two_preds) %>% last(),
      value = last_two_preds %>% last()
    )

  d_last_pred <- create_ennuste_trace(last_prediction$time, last_prediction$value)
  d_second_last_pred <- create_ennuste_trace(second_last_prediction$time, second_last_prediction$value)

  # palautetaan "pseudo" ennustesarja, joka voidaan esittää plotly kuvaajassa
  d_second_last_pred %>%
    rbind( d_last_pred)
}


#' Transforms absolute values to yearly change
#' @param tibble data to be transformed
#' @return tibble
#' @examples
#' \dontrun{
#'yearly_change(data = robonomistClient::data("StatFin/kan/ntp/statfin_ntp_pxt_132h.px", tidy_time = TRUE) %>%
#'                      filter(str_detect(Taloustoimi, "B1GMH")) %>%
#'                      filter(Tiedot %in% c("Kausitasoitettu ja työpäiväkorjattu sarja, viitevuosi 2015, miljoonaa euroa")))
#'}
#' @export
yearly_change <- function(data){
  data %>%
    mutate(value = ((value/ lag(value, 4)) -1) * 100) %>%
    drop_na()
}

#' Draw using yaml specification and corresponding prediction data from excel
#' @param path path to yaml file specifying picture to draw, which datas used etc.
#' @param path path to excel file containing prediction data
#'
#' @return plotly object
#' @examples
#' \dontrun{
#' piirtaja$using_yaml_and_ennuste_excel(yaml_path = system.file("ennustekuvat/test_without_ennusteet.yaml", package="pttrobo"),
#'  excel_path = "ptt_ennusteet_KT_uusi_sarja_id.xlsx")
#'}
#' @export
#' @import plotly htmltools RCurl lubridate
draw_ennuste <- function(yaml_path, yaml_kuvion_nimi, excel_path, from_year=NULL){
  piirtaja <- pttrobo::ptt_plot()
  kuvio_spec <- pttrobo::yaml_to_plotly_data(file=yaml_path, kuvion_nimi=yaml_kuvion_nimi)

  # jos on 2 datasarjaa, käytetään piirtäjää 2:lle datasarjalle (tällä hetkellä ei vielä 1:lle datasarjalle, tai 3:lle tai useammalle toteutettu käsittelyä)
  if(kuvio_spec$datas %>% length() == 2){
    return(
      piirtaja$two_lines_with_preds(
        dataspec_1 = kuvio_spec$datas$data_1,
        dataspec_2 = kuvio_spec$datas$data_2,
        ennuste_path = excel_path,
        labels = kuvio_spec$labels,
        from_year = from_year
      )
    )
  }
  return(
    NULL
  )
}
