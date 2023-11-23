#' @importFrom plotly layout
ptt_plot_set_grid <- function(p, grid_color) {
  p |> layout(
    #xaxis = list(showgrid = TRUE, gridcolor = grid_color, size =1.5),
    xaxis = list(showgrid = TRUE, gridcolor = "#FFFFFF", size =1.5),
    yaxis = list(showgrid = TRUE, gridcolor = grid_color, size = 1.5)
  )
}

#' @importFrom plotly layout config
ptt_plot_set_defaults <- function(p, range = list(x = c(NA,NA), y = c(NA,NA))) {
  if(!"x" %in% names(range)) { range$x <- c(NA,NA) }
  if(!"y" %in% names(range)) { range$y <- c(NA,NA) }
  if(!all(is.na(range$y)) & any(is.na(range$y)) || !all(is.na(range$x)) & any(is.na(range$x))) {
    message("Provide both ends for any axis limits!")
  }
  config(p, locale = "fi") |>
    layout(separators = ", ") |>
    layout(xaxis = if(all(is.na(range$x))) { list(fixedrange = T) } else { list(fixedrange = T, range = range$x) },
           yaxis = list(fixedrange = T, range = range$y)) |>
    layout(hovermode = "compare") |>
    ptt_plot_set_axis_labels()

}

#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
#' @importFrom htmlwidgets JS
#' @importFrom plotly config
#' @importFrom purrr pmap
#' @importFrom tidyr drop_na
#' @importFrom fontawesome fa
ptt_plot_set_modebar <- function(p, title, subtitle, png_layout, reset = F) {

  if (reset == T) { p <- config(p, modeBarButtons = NULL) }

  # data_dl_icon <- "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"
  # chalkboard_icon <- "M69.53,91.55l13.23,13.23c3.2,0.09,5.77,2.72,5.77,5.95c0,3.29-2.66,5.95-5.95,5.95c-3.29,0-5.95-2.67-5.95-5.95 c0-0.36,0.03-0.72,0.09-1.07L65.21,98.16v8.46l-8.24,0v-8.23l-11.69,11.7c0.02,0.21,0.03,0.43,0.03,0.65c0,0,0,0,0,0 c0,3.29-2.66,5.95-5.96,5.95c-3.29,0-5.95-2.67-5.95-5.95c0-3.29,2.67-5.95,5.95-5.95c0.1,0,0.2,0,0.29,0.01l13.23-13.23L0,91.55 V15.71c0-0.05,0-0.09,0-0.14V7.52c0-0.87,0.72-1.57,1.61-1.57h55.36V0h8.24v5.95h56.06c0.89,0,1.61,0.71,1.61,1.57v8.05 c0,0.03,0,0.05,0,0.08v75.89H69.53L69.53,91.55z M26.89,62.71l23.26-22.74c5.76,5.76,11.46,11.46,17.28,17.21l15.41-15.6 l-7.15-7.15l20.12-0.18v20.29l-6.87-6.87c-7.16,7.25-14.29,14.48-21.47,21.67L50.03,52.12L32.99,68.81L26.89,62.71L26.89,62.71 L26.89,62.71z M113.79,21.73H8.92v60.64h104.87V21.73L113.79,21.73z"
  # twitter_icon <- "M640.012 121.513c-23.528 10.524-48.875 17.516-75.343 20.634 27.118-16.24 47.858-41.977 57.756-72.615-25.347 14.988-53.516 25.985-83.363 31.866-24-25.5-58.087-41.35-95.848-41.35-72.508 0-131.21 58.736-131.21 131.198 0 10.228 1.134 20.232 3.355 29.882-109.1-5.528-205.821-57.757-270.57-137.222a131.423 131.423 0 0 0-17.764 66c0 45.497 23.102 85.738 58.347 109.207-21.508-.638-41.74-6.638-59.505-16.359v1.642c0 63.627 45.225 116.718 105.32 128.718-11.008 2.988-22.63 4.642-34.606 4.642-8.48 0-16.654-.874-24.78-2.35 16.783 52.11 65.233 90.095 122.612 91.205-44.989 35.245-101.493 56.233-163.09 56.233-10.63 0-20.988-.65-31.334-1.89 58.229 37.359 127.206 58.997 201.31 58.997 241.42 0 373.552-200.069 373.552-373.54 0-5.764-.13-11.35-.366-16.996 25.642-18.343 47.87-41.493 65.469-67.844l.059-.059z"
  # png_icon <- "M7.5,0h107.9c4.1,0,7.5,3.4,7.5,7.5v70.6c0,4.1-3.4,7.5-7.5,7.5H7.5c-4.1,0-7.5-3.4-7.5-7.5V7.5 C0,3.4,3.4,0,7.5,0L7.5,0z M69.9,63.3h28.5v4H69.9V63.3L69.9,63.3z M69.9,53.1H109v4H69.9V53.1L69.9,53.1z M92.1,35h5.6 c0.3,0,0.5,0.2,0.5,0.5v11c0,0.3-0.2,0.5-0.5,0.5h-5.6c-0.3,0-0.5-0.2-0.5-0.5v-11C91.6,35.3,91.8,35,92.1,35L92.1,35L92.1,35z M70.5,28.3h5.6c0.3,0,0.5,0.2,0.5,0.5v17.8c0,0.3-0.2,0.5-0.5,0.5h-5.6c-0.3,0-0.5-0.2-0.5-0.5V28.8 C69.9,28.5,70.2,28.3,70.5,28.3L70.5,28.3L70.5,28.3L70.5,28.3z M81.3,24.5h5.6c0.3,0,0.5,0.2,0.5,0.5v21.6c0,0.3-0.2,0.5-0.5,0.5 h-5.6c-0.3,0-0.5-0.2-0.5-0.5V25C80.8,24.7,81,24.5,81.3,24.5L81.3,24.5L81.3,24.5z M39.3,48.2l17,0.3c0,6.1-3,11.7-8,15.1 L39.3,48.2L39.3,48.2L39.3,48.2z M37.6,45.3l-0.2-19.8l0-1.3l1.3,0.1h0h0c1.6,0.1,3.2,0.4,4.7,0.8c1.5,0.4,2.9,1,4.3,1.7 c6.9,3.6,11.7,10.8,12.1,19l0.1,1.3l-1.3,0l-19.7-0.6l-1.1,0L37.6,45.3L37.6,45.3L37.6,45.3z M39.8,26.7L40,44.1l17.3,0.5 c-0.7-6.8-4.9-12.7-10.7-15.8c-1.2-0.6-2.5-1.1-3.8-1.5C41.7,27.1,40.8,26.9,39.8,26.7L39.8,26.7L39.8,26.7z M35.9,47.2L45.6,64 c-3,1.7-6.3,2.6-9.7,2.6c-10.7,0-19.4-8.7-19.4-19.4c0-10.4,8.2-19,18.6-19.4L35.9,47.2L35.9,47.2L35.9,47.2z M115.6,14.1H7.2v64.4 h108.4V14.1L115.6,14.1L115.6,14.1z"

  if(is.na(title)) {
    dl_title <- "img"
  } else {
    dl_title <- str_extract_all(title, "[a-zåäö,A-ZÅÄÖ,\\s,_,\\.,0-9]", simplify = T) |>
      str_c(collapse = "") |>
      str_squish() |>
      tolower() |>
      str_replace_all(c("ä" = "a", "å" = "o", "ö" = "o", " |\\." = "_"))
  }

  js_string <- function(wd, ht, suffix, layout, ttl = dl_title) {
    split_title <- list(
      str_c("\\<span>\\<b>",p$title,"\\</b><span style='font-size: 75%'><br>",p$subtitle,"\\</span>\\</span>"),
      str_c("\\<span>\\<b>",str_replace(str_wrap(p$title, round((p$title %>% str_length())/2))%>% str_replace_all(c("\n" = " ")),"\n","\\<br>"),"\\</b>\\<span style='font-size: 75%'>\\<br>",p$subtitle,"\\</span>\\</span>"))
    str_c('
          function(gd) {
          let oldlayout = JSON.parse(JSON.stringify(gd.layout))
          delete gd.layout.xaxis.rangeslider;
          delete gd.layout.height
          Plotly.relayout(gd, {height: ',ht,', width: ',wd,',
          "annotations[0].y": ',layout$font_sizing$caption,',
          "annotations[1].y": ',layout$font_sizing$caption,',
          "annotations[2].y": ',layout$font_sizing$caption,',
          "xaxis.tickfont.size": ',layout$font_sizing$main,',
          "yaxis.tickfont.size": ',layout$font_sizing$main,',
          "title.font.size": ',layout$font_sizing$title,'})
          setVerticalLayout({"width": true}, gd, ',layout$font_sizing$main,', ["',split_title[[1]],'","',split_title[[2]],'"])
          Plotly.downloadImage(gd, {format: "png", width: ',wd,', height: ',ht,', filename: "',ttl,'_',suffix,'"});
          Plotly.relayout(gd, oldlayout)
          delete oldlayout
          }
   ')
  }

  robonomist_btn <-
    list(
      name = "Powered by Robonomist",
      icon = list(
        name = "Robonomist",
        svg = '<svg version="1.1" viewBox="0 0 71.447 32" xmlns="http://www.w3.org/2000/svg"><g id="XMLID_248_" transform="scale(.31159)"><polyline id="XMLID_132_" points="229.3 53.2 174.3 90.1 174.3 69.1 199.5 53.2 174.3 37.3 174.3 16.3"/><g id="XMLID_40_"><path id="XMLID_41_" d="m112 0c14.2 0 23.3 1.8 30.7 7 6.3 4.4 10.3 10.8 10.3 20.5 0 11.3-6.4 22.8-22.3 26.5l18.4 32.5c5 8.7 7.7 9.7 12.5 9.7v6.5h-27.3l-23.7-45.8h-7v27.6c0 10.5 0.7 11.7 9.9 11.7v6.5h-43.2v-6.7c10.3 0 11.3-1.6 11.3-11.9v-65.7c0-10.2-1-11.7-11.3-11.7v-6.7zm-4.8 7.9c-3.3 0-3.6 1.5-3.6 8.6v32.3h6.4c15.8 0 20.2-8.7 20.2-21.3 0-6.3-1.7-11.5-5-15-2.9-3-7-4.6-13-4.6z"/></g><polyline id="XMLID_130_" points="0 53.2 55 16.3 55 37.3 29.8 53.2 55 69.1 55 90.1"/></g></svg>'
      ),
      click = JS("function(gd) {window.open(\"https://robonomist.com\")  }")
    )

  dl_icon <- function(fa_icon, scale = 0.032, translate = c(0,0)) {
    transform_string <- str_c("translate(",translate[1],",",translate[2],") scale(",scale,")")
    list(path = fa(name = fa_icon) |> as.character() |> str_extract("(?<=d\\=\")[^\"]{1,}") |> str_replace_all(" ",","),
         transform = transform_string)
  }

  dl_conference_btn <- list(
    name = "Lataa kuva (leveä)",
    icon = dl_icon("image"),
    click = JS(js_string(1280,720,"levea",png_layout$lg))
  )

  dl_btn <- list(
    name = "Lataa kuva (kapea)",
    icon = dl_icon("file-image", 0.025, c(2.7,2)),
    click = JS(js_string(810,720,"kapea",png_layout$lg)))

  dl_twitter_btn <- list(
    name = "Lataa kuva (pieni)",
    icon = dl_icon("twitter-square"),
    click = JS(js_string(889,500,"pieni",png_layout$sm)))

  dl_string <- (function() {
    row.data <- p$data |> drop_na() |> pmap(function(...) {
      as.character(list(...)) |> str_replace_all(c(";"=",","'"="’")) |> str_c(collapse = ";")
    }) |> unlist() |> str_c(collapse = "\\n")
    col.names <- names(p$data) |> str_replace("csv\\.data\\.tiedot","tiedot") |> str_c(collapse = ";")
    str_c(col.names,"\\n",row.data)
  })()


  data_dl_btn <- list(
    name = "Lataa tiedot",
    icon = dl_icon("file-csv"),
    click = JS(str_c("
          function(gd) {
            let text = '",dl_string,"';
            //for(var i = 0; i < gd.data.length; i++){
              //console.log(gd.data[i])
              //var array1 = gd.data[i].y
              //array1.forEach(element => console.log(element));
              //console.log((gd.data[i].y))
              //text = text + gd.data[i].name + ';' + gd.data[i].x + '\\n';
              //text = text + gd.data[i].name + ';' + gd.data[i].y + '\\n';
            //};
            var blob = new Blob([text], {type: 'text/plain;charset=UTF-8'});
            var a = document.createElement('a');
            const object_URL = URL.createObjectURL(blob);
            a.href = object_URL;
            a.download = '",dl_title,"_data.csv';
            document.body.appendChild(a);
            a.click();
            URL.revokeObjectURL(object_URL);
          }
   "))
  )

  p |> config(
    displaylogo = FALSE,
    modeBarButtons = list(list(
      dl_twitter_btn,
      dl_btn,
      dl_conference_btn,
      data_dl_btn,
      robonomist_btn
    ))
  )
}

#' @importFrom plotly layout
# ptt_plot_set_ticks <- function(p, font,ticktypes) {
#   p |>
#     layout(xaxis=list(tickfont = font,
#                       mirror = TRUE,
#                       ticks = 'outside',
#                       type = "date",
#                       tickformatstops = list(
#                         list(
#                           dtickrange = list(NULL, 604800000),
#                           value = "%d.%m.%Y"
#                         ),
#                         list(
#                           # end is year in milliseconds
#                           dtickrange = list(604800000, 3.15e10),
#                           value = "%m/%Y"
#                         ),
#                         list(
#                           dtickrange = list(3.15e10, NULL),
#                           value = "%Y"
#                         )
#                       ),
#                       showline = TRUE),
#            yaxis=list(tickfont = font,
#                       tickformat = ",.3~r",
#                       ticksuffix = " ",
#                       mirror = TRUE,
#                       ticks = 'outside',
#                       showline = TRUE))
#
# }
ptt_plot_set_ticks <- function(p, font, ticktypes) {
  set_ticks <- function(ticktype) {
    if (ticktype == "date") {
      list(tickfont = font,
           mirror = F,
           ticks = F,
           type = "date",
           tickformatstops = list(
             list(
               dtickrange = list(NULL, 604800000),
               value = "%d.%m.%Y"
             ),
             list(
               # end is year in milliseconds
               dtickrange = list(604800000, 3.15e10),
               value = "%m/%Y"
             ),
             list(
               dtickrange = list(3.15e10, NULL),
               value = "%Y"
             )
           ),
           showline = TRUE
           )
    } else if (ticktype == "double") {
      list(tickfont = font,
           tickformat = ",.3~f",
           ticksuffix = " ",
           mirror = FALSE,
           ticks = 'outside',
           showline = TRUE)
    } else if (ticktype == "character") {
      font$size <- font$size - 4
      list(tickfont = font,
           ticksuffix = " ",
           tickmode = "linear",
           mirror = F,
           type = "category",
           ticks = 'outside',
           showline = F)
    }

  }
  p |>
    layout(xaxis= set_ticks(ticktypes$x),
           yaxis= set_ticks(ticktypes$y))

}

#' @importFrom plotly layout
ptt_plot_set_margin <-function(p, margin) {

  if (!is.list(margin)) {
    stop("Insert plot margins as list (default is list(t,r,b,l,pad))\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else if (any(!names(margin) %in% c("t","r","b","l","pad")) | any(!is.double(unlist(margin)))) {
    stop("All plot margins must be of double type, and named one or more of t, r, b, l or pad.\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else {
    p |>
      layout(
        margin = margin,
        autosize = T,
        dragmode = FALSE
      )
    # p
  }
}

#' @importFrom plotly layout
#' @importFrom RCurl base64Encode
ptt_plot_add_logo <- function(p){

  image_file <- system.file("image", "ptt-logo.png", package = "pttrobo")
  txt <- base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  p |> plotly::layout(
    images = list(
      source = paste('data:image/png;base64', txt, sep=','),
      xref = "paper",
      yref = "paper",
      x = 1,
      sizex = 1,
      sizey = 1,
      xanchor="right",
      yanchor = "bottom"
    )
  )
}

#' @importFrom plotly layout
#' @importFrom dplyr case_when
ptt_plot_add_rangeslider <- function(p, enable = F, height = 0.1, slider_range = NULL) {
  if(enable == T) {
    height <- case_when(height > 0.5 ~ 0.5, height < 0.1 ~ 0.1, TRUE ~ height)
    p |> rangeslider(slider_range[1], slider_range[2], thickness = height)
  } else { p }
}

#' @importFrom dplyr case_when
#' @importFrom stringr str_length
#' @importFrom plotly plot_ly
#' @importFrom lubridate as_date today
ptt_plot_config <- function(p,
                            title, subtitle = "", caption,
                            font_color = "#696969",
                            font_size = 14,
                            legend_position, legend_orientation,
                            tick_color, grid_color,
                            height, width = 800,
                            margin = NA,
                            zeroline = F,
                            axis_range = list(x = c(NA,NA), y = c(NA,NA)),
                            enable_rangeslider = list(rangeslider = F, max = as_date(today)),
                            ticktypes = list(x = "time", y = "double")) {

  if(missing(title) | missing(caption)) {
    stop("Title and caption must be strings, or NA for no plot title or plot caption.", call. = F)
  }

  if(!is.logical(enable_rangeslider$rangeslider) & !is.character(enable_rangeslider$rangeslider)) {
    stop("Rangeslider must be TRUE, FALSE or a string interpretable as date, eg. \"2015-01-01\".", call. = F)
  } else if (is.logical(enable_rangeslider$rangeslider)) {
    enable_rangeslider <- enable_rangeslider$rangeslider
    slider_range <- NULL
  } else if (is.character(enable_rangeslider$rangeslider)) {
    if (is.na(suppressWarnings(as_date(enable_rangeslider$rangeslider)))) {
      stop("Rangeslider must be TRUE, FALSE or a string interpretable as date, eg. \"2015-01-01\".", call. = F)
    } else {
      slider_range <- list(enable_rangeslider$rangeslider, enable_rangeslider$max)
      enable_rangeslider <- T
    }
  }

  if(!is.list(margin)) {
    if(is.na(margin)) {
      margin <- list(t = 0, r = 20, b = 0)
    }
  }

  main_font <- list(size = font_size, family = "sans-serif", color = font_color)
  title_font <- list(size = round(font_size*1.3), family = "sans-serif", color = font_color)
  caption_font <- list(size = round(font_size*1), family = "sans-serif", color = font_color)

  rangeslider_size <- 0.1

  png_attrs <- (function(small_ht = 500, large_ht = 720) {
    font_sizing_lg <- list(title = 31, main = 24, caption = 19)
    font_sizing_sm <- list(title = 23, main = 18, caption = 14)
    list(sm = list(font_sizing = font_sizing_sm),
         lg = list(font_sizing = font_sizing_lg))
  })()

  p$enable_rangeslider <- list(enable = enable_rangeslider, size = rangeslider_size, range = slider_range)
  p$add_zeroline <- zeroline
  p$png_attrs <- png_attrs
  p |>
    ptt_plot_attach_js(title, subtitle) |>
    ptt_plot_set_defaults(axis_range) |>
    ptt_plot_set_grid(grid_color) |>
    ptt_plot_set_modebar(title, p$subtitle, png_attrs) |>
    ptt_plot_set_ticks(main_font, ticktypes) |>
    ptt_plot_set_margin(margin) |>
    #ptt_plot_add_logo() |>
    ptt_plot_set_legend(legend_position, legend_orientation, main_font) |>
    ptt_plot_set_title(title, subtitle, title_font) |>
    ptt_plot_set_caption(caption, caption_font) |>
    ptt_plot_add_zeroline(zeroline) |>
    ptt_plot_add_rangeslider(enable_rangeslider, rangeslider_size, slider_range = slider_range)
}


#' @importFrom plotly layout
#' @importFrom stringr str_extract
ptt_plot_set_legend <- function(p, position, orientation, font) {
  if (is.na(position)) {
    p |> layout(showlegend = F)
  } else if (!position %in% c("right","bottom")) {
    stop("Position must be one of \"right\", \"left\", or NA")
  } else if (!orientation %in% c("vertical","horizontal","auto")) {
    stop("Orientation must be one of \"horizontal\", \"vertical\", or \"auto\"")
  } else {
    x.pos <- ifelse(position == "right", 100, 0)
    y.pos <- ifelse(position == "right", 1, 0) #-0.05
    orientation <- case_when(orientation == "auto" ~ ifelse(position == "right", "v","h"), TRUE ~ str_extract(orientation, "^(v|h)"))
    p |> layout(
      showlegend = T,
      legend = list(font = font,
                    bgcolor = 'rgba(0,0,0,0)',
                    x = x.pos, y = y.pos,
                    traceorder = "normal",
                    orientation = orientation,
                    xanchor = "left",
                    yanchor = "bottom"))
  }
}


#' @importFrom plotly layout
ptt_plot_set_caption <- function(p, caption, font) {
  if(is.na(caption)) {
    p
  } else if (!is.character(caption)) {
    stop("Caption and caption footer must be a string, or caption = NA for no plot caption.", call. = F)
  } else {
    font$size <- round(font$size * 0.8)
    p |>
      layout(
        annotations = list(
          x = 0, text = caption, align = "left",
          showarrow = F, xref = 'paper', yref = "paper",
          xanchor='left', yanchor = 'bottom', xshift=0, yshift=0,
          font = font
        )
      )
  }
}

#' @importFrom plotly layout
#' @importFrom htmltools tag HTML
ptt_plot_set_title <- function(p, title, subtitle, font) {
  if (is.na(title)) {
    p
  } else if (!is.character(title) || !is.character(subtitle)) {
    stop("Title and subtitle must be strings, or title = NA for no plot title.", call. = F)
  } else {
    p <- p |>
      layout(
        title = list(
          text = paste0(
            "<span><b>", title, "</b>",
            tags$span(HTML(str_c("<br>",subtitle)), style = "font-size: 75%"),"</span>"
          ),
          font = font,
          xanchor = "left",
          x = 0,
          xref = "paper")
      )
  }
}

#' @importFrom plotly layout
ptt_plot_set_axis_labels <- function(p, label_x = NA, label_y = NA) {
  if (any(!is.na(c(label_x, label_y)))||any(is.null(c(label_x, label_y)))) {
    if(!any(is.character(na.omit(c(label_x, label_y))))) {
      stop("Axis labels must be strings or NA.", call. = F)
    }
  }
  p |>
    layout(
      xaxis = list(title = label_x),
      yaxis = list(title = label_y)
    )
}

#' @importFrom plotly layout
ptt_plot_add_zeroline <- function(p, z) {
  if(!is.logical(z$zeroline) & !is.double(z$zeroline)) {
    stop("Zeroline must be TRUE, FALSE or of type double.", call. = F)
  } else if (z$zeroline == F & !is.numeric(z$zeroline)) {
    p
  } else {
    zero_line <- ifelse(z$zeroline == T, 0, z$zeroline)
    p |> layout(shapes= list(type = "line", x0 = z$xrange$min, x1 = z$xrange$max, xref = "x", y0 = zero_line, y1 = zero_line, yref = "y", layer = "below", line = list(width = 2))) |>
      onRender(jsCode = "
function(gd, params, data) {
let zeroline_relayout = {'shapes[0].x0': gd.layout.xaxis.range[0], 'shapes[0].x1': gd.layout.xaxis.range[1]}
Plotly.relayout(gd, zeroline_relayout)

gd.on('plotly_afterplot', function() {
  var line_label = data.zeroline
  let yticks = $(gd).find('g.ytick text')
  let zeroline = $(gd).find('path.zl')
  if(line_label == 0) {
    if (zeroline.length > 0) {
    zeroline[0].style['stroke'] = 'black'
    }
  } else {
  let label_translate
  if (zeroline.length > 0) { zeroline[0].style['stroke'] = 'rgb(232, 232, 232)' };
  yticks.filter(function(d, i) {
  if(this.textContent.trim().replace(',','.').replace('−','-').replace(' ','') == line_label) {
  label_translate = i.getAttribute('transform')
    let lines = $(gd).find('path.ygrid')
    lines.filter(function(d, i) {
    if(i.getAttribute('transform') == label_translate) {
      i.style['stroke'] = 'black'
    }
    })
  }
  })
  }

})
}
                                                         ", data = list(zeroline = zero_line))
  }
}

#' @importFrom htmltools tags
#' @importFrom htmlwidgets appendContent onRender
#' @importFrom RCurl base64Encode
#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
ptt_plot_attach_js <- function(p, title, subtitle) {
  split_title <- list(
    str_c("<span><b>",title,"</b><span style=\"font-size: 75%\"><br>",subtitle,"</span></span>"),
    str_c("<span><b>",str_replace(str_wrap(p$title, round((p$title %>% str_length())/2)),"\n","<br>"),"</b><span style=\"font-size: 75%\"><br>",subtitle,"</span></span>"))
  # dep <- htmltools::htmlDependency("relayout", "0.1", src = c(href= system.file("www/js", package = "panoshinta")),  script = "relayout.js")
  # p$dependencies <- c(p$dependencies, list(dep))
  # if(!shiny::isRunning()) {
  js_file <- system.file("www", "js", "relayout.js", package = "pttrobo")
  css_file <- system.file("www", "css", "style.css", package = "pttrobo")
  base_string <- base64Encode(readBin(js_file, "raw", file.info(js_file)[1, "size"]), "txt")
  css_string <- base64Encode(readBin(css_file, "raw", file.info(css_file)[1, "size"]), "txt")
  p <- appendContent(
    p,
    tagList(
      tags$script(src = str_c('data:application/javascript;base64', base_string, sep=',')),
      tags$link(rel = "stylesheet", type = "text/css", href = str_c('data:text/css;base64', css_string, sep=',')))
  )
  # }
  rangeslider_sums <- F
  if(p$plot_mode == "relative" && any(p$trace_types == "bar")) { rangeslider_sums = T }
  p %>%
    onRender(jsCode = str_c("
                        function (gd, params, data){
                        let legendFontsize = 0;
                        if ('legend' in gd.layout) { legendFontsize = gd.layout.legend.font.size };
                        let alt_title = data.splitTitle;
                        setVerticalLayout({'width': true}, gd, legendFontsize, alt_title);
                        gd.on('plotly_relayout',function(eventdata, lf = legendFontsize) {
                        plotlyRelayoutEventFunction(eventdata, gd, lf, alt_title, data.rangesliderSums);
                        });
                        }"), data = list(splitTitle = split_title, rangesliderSums = rangeslider_sums))
}


#' @importFrom stringr str_subset
ptt_plot_hovertemplate <- function(specs) {

  hovertemplate_freq <- function(f) {
    if (is.null(f)) { "%Y-%m-%d" } else {
      switch(f,
             "Annual" = "%Y",
             "Quarterly" = "%YQ%q",
             "Monthly" = "%Y-%m-%d",
             "Weekly" = "%YW%V",
             "Daily" = "%d.%m.%Y",
             "%Y-%m-%d"
      )
    }
  }

  if (is.null(specs)) {
    NA
  } else if (!is.list(specs)) {
    stop("Hovertext must be either a list with any of \"rounding\", \"unit\", \"extra\" or \"dateformat\", or NULL", call. = F)
  } else if (!any(c("rounding","unit","","extra", "dateformat") %in% names(specs))) {
    stop("Hovertext must be either a list with any of \"rounding\", \"unit\", \"extra\" or \"dateformat\", or NULL", call. = F)
  } else {
    specs_template <- list(rounding = 1, unit = "", extra = "", dateformat = hovertemplate_freq("Monthly"))
    for (spec.name in (names(specs) |> str_subset("^(rounding|unit|extra|dateformat)$"))) {
      if(spec.name == "rounding") {
        if ((specs$rounding)%%1!=0) { stop("Hovertext rounding must be an integer.", call. = F) }
      } else if(spec.name == "dateformat") {
        if (is.null(specs$dateformat)) {
          specs$dateformat <- hovertemplate_freq(specs$dateformat)
        } else if (!is.character(specs$dateformat)) {
          stop("Hovertext dateformat must be one of \"Annual\", \"Quarterly\", \"Monthly\", \"Weekly\" or \"Daily\", or NULL.", call. = F)
        } else if (!(specs$dateformat %in% c("Annual", "Quarterly","Monthly","Weekly","Daily"))) {
          stop("Hovertext dateformat must be one of \"Annual\", \"Quarterly\", \"Monthly\", \"Weekly\" or \"Daily\", or NULL.", call. = F)
        } else {
          specs$dateformat <- hovertemplate_freq(specs$dateformat)
        }
      } else if (!is.character(specs[[spec.name]])) {
        stop(str_c("Hovertext ",spec.name," must be a string.", call. = F))
      }
      specs_template[[spec.name]] <- specs[[spec.name]]
    }
    paste0("%{text}<br>%{y:.",specs_template$rounding,"f} ",specs_template$unit,"<br>%{x|",specs_template$dateformat,"}",ifelse(specs_template$extra == "", "", paste0("<br>",str_c(specs_template$extra, collapse = " "))),"<extra></extra>")
  }
}


#' Gives a vector of desired length of colors.
#'
#' Outputs a vector of colors.
#'
#' @param n_unique How many colors are needed.
#' @param color_vector Optional. A vector or colors.
#' @param accessibility_params Placeholder for accessibility features.
#' @return plotly object
#' @return Vector of colors.
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly plot_ly

ptt_plot_set_colors <- function(n_unique, color_vector, accessibility_params) {
  ptt_vihrea <- "#009346"
  ptt_sininen <- "#2f7ab9"
  ptt_ruskea <- "#C36B0D"
  ptt_keltainen <- "#FFCC00"
  ptt_white <- "white"
  # ptt_dark_grey <- "#696969"
  # ptt_light_grey <- "#E8E8E8"
  ptt_cols <- c(ptt_vihrea, ptt_sininen, ptt_ruskea, ptt_keltainen)

  if(n_unique <= 4) {
    cols <- ptt_cols[1:n_unique]
  } else {
    cols <- colorRampPalette(colors = ptt_cols)(n_unique)
  }
  cols# |> alter_color_for_accessibility() ## tähän tulee saavutettavuuskoodi
}

ptt_plot_get_frequency <- function(d) {
  d_attrs <- attributes(d)
  tf <- if(is.null(d_attrs$frequency)) {
    message("No frequency attribute detected for hovertext time format, resorting to default %Y/%m/%d.")
    NULL
  } else if (is.list(d_attrs$frequency)) {
    if(is.null(d_attrs$frequency$en)) {
      message("No frequency attribute detected for hovertext time format, resorting to default %Y/%m/%d.")
      NULL
    } else{
      as.character(d_attrs$frequency$en)
    }
  } else {
    as.character(d_attrs$frequency)
  }
  tf
}

ptt_plot_set_highlight <- function(highlight, df) {
  if(is.null(highlight)) {
    T
  } else if (is.double(highlight)) {
    max(df$value, na.rm = T) >= highlight
  } else if (is.list(highlight)) {
    if (!all(c("value",".fun") %in% names(highlight))) {
      stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
    } else {
      highlight$.fun(df$value, na.rm = T) >= highlight$value
    }
  } else {
    stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
  }
}

#' @importFrom stringr str_length
ptt_plot_are_colors <- function(x) {
  (str_detect(toupper(x), "#[0-9A-F]{6}") | x %in% colors()) %>% replace_na(F)
}

#' Creates a plotly object with visual specifications of ptt.
#'
#' Outputs a plotly object.
#'
#' @param d Data for plotting (Tibble).
#' @param grouping Tibble column from tibble d to use for grouping (Unquoted string).
#' @param title,subtitle,caption labeling for plot
#' @param legend_orientation,legend_position Legend positioning.
#' @param margin Plot margins if calculated margins do not work (List).
#' @param font_color,grid_color,font_size Plot background element colors and font sizes.
#' @param zeroline Determines zeroline inclusion (Logical or double).
#' @param rangeslider Determines rangeslider inclusion (Logical).
#' @param axis_limits Determines the limits of the axes (list(x = c(NA,NA), y = c(NA,NA)))".
#' @param hovertext A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)")".
#' @param highlight Either NULL, a double that limits, or a function that returns a logical determining if a given trace is included in legend and assigned a color.
#' @param plot_type Determines the trace type for either the whole plot, or for all variables defined by grouping as name-value pairs (Character vector).
#' @param plot_mode Determines the barmode for a barplot. Disregarded for scatterplots. (Character).
#' @param trace_color Trace color for all traces. Either a character string or a character vector of name-value pairs interpretable as colors (Character vector).
#' @param line_width Line width for line plots. Either a double or a double vector of name-value pairs (Double vector).
#' @param height Height of the plot.
#' @return plotly object
#' @examples
#' d <- ptt_data_robo_l("StatFin/kan/ntp/statfin_ntp_pxt_132h.px") |>
#'   dplyr::filter(stringr::str_detect(taloustoimi, "B1GMH|P3KS14_S15"), tiedot == "Kausitasoitettu ja työpäiväkorjattu sarja, viitevuosi 2015, miljoonaa euroa", lubridate::year(time) >= 2010) |>
#'   dplyr::group_by(taloustoimi) |>
#'   dplyr::mutate(value = ((value/ lag(value, 4)) -1) * 100) |>
#'   dplyr::ungroup() |>
#'   dplyr::mutate(tiedot = dplyr::case_when(stringr::str_detect(taloustoimi, "B1GMH") ~ "BKT", TRUE ~ "Yksityinen kulutus") |> forcats::as_factor()) |>
#'   tidyr::drop_na()
#'   p <- d |> ptt_plot(tiedot, "BKT ja kulutus", subtitle = "Vuosimuutos",
#'   caption =  "Lähde: Tilastokeskus ja PTT",
#'   rangeslider = "2015-01-01",
#'   zeroline = T)
#'   p
#' @export
#' @importFrom plotly plot_ly add_trace
#' @importFrom rlang enquo as_name set_names
#' @importFrom dplyr group_split coalesce
#' @importFrom stringr str_length str_replace str_c


ptt_plot <- function(d,
                     grouping,
                     title,subtitle = "",
                     caption,
                     legend_orientation = "auto",
                     legend_position = "bottom",
                     margin = NA,
                     font_color = "#696969",
                     grid_color = "#E8E8E8",
                     trace_color = NULL,
                     highlight = NULL,
                     zeroline = F,
                     rangeslider = FALSE,
                     line_width = 4,
                     font_size = 14,
                     hovertext,
                     axis_limits = list(x = c(NA,NA), y = c(NA,NA)),
                     plot_type = "scatter",
                     plot_mode = "dodge",
                     height = 550,
                     facet_split,
                     ...
){


  if(missing(d) || missing(grouping)){
    stop("Data and unquoted grouping variable are needed!\nFor example: ptt_plot(a_tibble, grouping=tiedot)", call. = F)
  }

  if(missing(hovertext)) {
    d_attrs <- attributes(d)
    tf <- ptt_plot_get_frequency(d)
    hovertext <- list(rounding = 1, unit = "", extra = "", dateformat = tf)
  } else if (!"dateformat" %in% names(hovertext)) {
    hovertext$dateformat <- ptt_plot_get_frequency(d)
  }

  if(!missing(facet_split)) {
    facet_split <- enquo(facet_split)
    message("PREDICTION TRACES AND SECONDARY TRACES WILL NOT WORK PROPERLY WITH FACETED PLOTS!")
    if(rangeslider == T | zeroline == T | any(!is.na(axis_limits$y))) message("Rangeslider, zeroline and y-axis range are not currently enabled for faceted plots.")
    rangeslider <- F
    zeroline <- F
    ymin <- min(d$value)
    ymax <- max(d$value)
    axdif <- diff(c(ymin, ymax)) * 0.04
    ymin <- ymin - axdif
    ymax <- ymax + axdif
    axis_limits$y <- c(ymin, ymax)
  }

  grouping <- enquo(grouping)

  if(plot_mode == "horizontal") {
    if(is.null(names(plot_mode))) { stop("Horizontal plot mode needs the name of the y-axis categorial variable as the name of plot mode (ie. \"(c('tavara' = 'horizontal'\")", call. = F) }
    xaxis <- "value"
    yaxis <- names(plot_mode)
    ticktypes <- list(x="double", y = "character")
  } else {
    xaxis <- "time"
    yaxis <- "value"
    ticktypes <- list(x="date",y = "double")
  }

  if(!is.factor(d[[as_name(grouping)]])) {
    d[[as_name(grouping)]] <- fct_inorder(d[[as_name(grouping)]])
  }

  d <- d|> group_by(!!grouping) %>% filter(!all(is.na(value))) |> ungroup() |> droplevels()

  unique_groups <- d[[as_name(grouping)]] |> unique() |> sort()

  if(!all(plot_type %in% c("scatter","bar"))) {
    stop("Plot type must be \"scatter\" or \"bar\", or a vector of name_value pairs!", call. = F)
  } else if (length(plot_type) == 1 & is.null(names(plot_type))){
    d <- d |> mutate(plot.type = plot_type)
  } else if (!all(unique_groups %in% names(plot_type))) {
    stop(str_c("All variables in column \"",as_name(grouping),"\" must have a corresponding plot type!"), call. = F)
  } else {
    d <- mutate(d, plot.type = str_replace_all(!!grouping, plot_type))
  }

  if(!all(typeof(line_width) == "double")) {
    stop("Line width must be a double, or a double vector of name_value pairs!", call. = F)
  } else if (length(line_width) == 1 & is.null(names(line_width))){
    d <- d |> mutate(line.width = line_width)
  } else if (!all(unique_groups %in% names(line_width)) & !(".other" %in% names(line_width)) ) {
    stop(str_c("All variables in column \"",as_name(grouping),"\" must have a corresponding line width, or key \".other\" must be included!"), call. = F)
  } else {
    missing_groups <- unique_groups %>% subset(!. %in% names(line_width))
    if(length(missing_groups) > 0) {
      detected_widths <- map2(unname(line_width), names(line_width), function(lw,nm) {
        miss <- missing_groups %>% subset(str_detect(., str_c(nm, collapse = "|")))
        rep(lw, length(miss)) %>% setNames(miss)
      }) %>% purrr::compact() %>% unlist()
      line_width <- c(line_width, detected_widths)
    }
    d <- mutate(d, line.width = line_width[as.character(!!grouping)] %>% dplyr::coalesce(line_width[".other"]))
  }
  color_vector <- ptt_plot_get_color_vector(trace_color, unique_groups, highlight, d, grouping)

  if(!missing(facet_split)) {
    p <- ptt_plot_get_facet_plot(d, facet_split, height, grouping, plot_type, trace_color, highlight, color_vector, grid_color, hovertext, plot_mode, font_size, font_color, ticktypes, axis_limits)
  } else {
    p <- ptt_plot_get_plot(d, xaxis, yaxis, height, grouping, plot_type, trace_color, highlight, color_vector, grid_color, hovertext, plot_mode)
  }

  # print(plot_mode)
  p$data <- ptt_plot_transform_data_for_download(d, grouping, facet_split, plot_mode)

  p$elementId <- str_c("widget",runif(1)) |> str_replace("0\\.","-")
  p$color_vector <- color_vector
  p$hover_template <- hovertext
  p$legend_ranks <- ((levels(unique_groups) |> factor() |> as.numeric())*100) |> set_names(as.character(levels(unique_groups)))
  p$title <- title
  p$subtitle <- subtitle
  p$trace_types <- (function() {
    traces <- distinct(d, !!grouping, plot.type)
    traces$plot.type |> set_names(traces[[as_name(grouping)]])
  })()
  p$plot_mode <- plot_mode
  p$line_width <- line_width

  maxtime <- max(d$time)
  p <- p |>
    ptt_plot_config(title = title, subtitle = subtitle, caption = caption,
                    font_color = font_color, font_size = font_size,
                    legend_position = legend_position, legend_orientation = legend_orientation,
                    tick_color = font_color, grid_color = grid_color, margin = margin,
                    height = height,
                    axis_range = axis_limits,
                    zeroline = list(zeroline = zeroline, xrange = list(min = min(d$time), max = maxtime)),
                    enable_rangeslider = list(rangeslider = rangeslider, max = maxtime),
                    ticktypes = ticktypes)

  ## add labels for facet plot. Has to be done here for the relayout js to work properly for captions.
  if(!missing(facet_split)) {
    yloc <- max(d$value)
    split_facet <- group_split(d,!!facet_split)
    for(i in seq(length(split_facet))) {
      xloc <- split_facet[[i]]$time %>% unique() %>% sort() %>% median
      ann_text <- split_facet[[i]][[as_name(facet_split)]] %>% unique() %>% str_pad(width = 10, side = "both", pad = " ")
      p <- p %>%
        layout(annotations =
                 list(text = ann_text, yref = str_c("y",i), xref = str_c("x",i), showarrow = F, y = yloc, x = xloc, bgcolor = "white", borderwidth = 1, borderpad = 4, bordercolor = "black"))
    }
  }

  p

}


#' Add prediction traces to an existing ptt_plot.
#'
#' Outputs a plotly object.
#'
#' @param p Plotly object created with ptt_plot to add prediction traces to.
#' @param pred_data Tibble in ptt prediction data format.
#' @param grouping Tibble column used for grouping in plot, should be labeled the same as data used for parent ptt_plot.
#' @param n_obs Number of observations (counted from the latest) used from the prediction set pred_data.
#' @param showlegend,with_labs Controls prediction trace legend. Hovertemplate takes labeling based on with_labs.
#' @param hovertext A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)")".
#' @param value_multiplier A number. Value of 0.001 would cause values to be divided by 1000, value of 1000 would cause values to be multiplied by 1000 hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)")".
#' @param custom_pred_data A boolean. If set to true, pred_data is required to be a dataframe/tibble with columns, year, sarja_nmi and value. With two prediction values per sarja_nmi.
#' @return plotly object
#' @examples
#' e <- readxl::read_excel("ptt_ennusteet_KT.xlsx") |> dplyr::filter(stringr::str_detect(filter, "B1GMH|P3KS14"))
#' p <- p |> ptt_plot_add_prediction(e)
#' p
#' @importFrom rlang enquo as_name
#' @importFrom tidyr uncount pivot_longer
#' @importFrom dplyr slice_tail
#' @importFrom plotly plot_ly add_lines
#' @importFrom stringr str_replace
#' @importFrom purrr reduce map_dfr
#' @importFrom lubridate floor_date years
#' @export
ptt_plot_add_prediction <- function(p,
                                    pred_data,
                                    grouping = sarja_nmi,
                                    n_obs = 2,
                                    with_labs = T,
                                    showlegend = F,
                                    hovertext = list(rounding = 1, unit = "", extra = "(ennuste)", dateformat = "Annual"),
                                    value_multiplier = 1,
                                    custom_pred_data = FALSE,
                                    satovuosi = FALSE
) {
  grouping <- enquo(grouping)

  months_shift <- if (satovuosi) 6 else 0

  if (custom_pred_data == FALSE){
    pred_series <- pred_data |>
      filter(!!grouping %in% names(p$color_vector)) |>
      mutate(across(matches("[0-9]{4}"), ~ as.double(.x))) |>
      pivot_longer(cols = matches("[0-9]{4}"), names_to = "year") |>
      select(year, !!grouping, value) |> group_by(!!grouping) |> slice_tail(n = n_obs) |>
      mutate(count = 2) |> uncount(count) |> group_by(year) |>
      mutate(
        time = paste0(year, c("-02-01","-11-01")) |> lubridate::as_date(),
        year = lubridate::as_date(paste0(year, c("-01-01")))) |> mutate(time = time + months(months_shift)) |>
      ungroup() |>
      relocate(value, .after = time) |>
      mutate(value = value * value_multiplier)
  } else {
    pred_series <- pred_data |>
      mutate(count = 2) |> uncount(count) |> group_by(year) |>
      mutate(
        time = paste0(year, c("-02-01","-11-01")) |> lubridate::as_date(),
        year = lubridate::as_date(paste0(year, c("-01-01")))) |> mutate(time = time + months(months_shift)) |>
      ungroup() |>
      relocate(value, .after = time) |>
      mutate(value = value * value_multiplier)
  }

  line_width <- p$line_width
  pred_series <- mutate(pred_series, line.width = (if(length(line_width) == 1) { line_width } else { line_width[as.character(!!grouping)] }) %>% dplyr::coalesce(line_width[".other"]))
  range.slider <- p$enable_rangeslider
  range.slider$range[[2]] <- max(range.slider$range[[2]],pred_series$time)
  plot.mode <- p$plot_mode
  pred_series <- pred_series |> droplevels() |>
    arrange(year) |>
    # group_by(year, time) |> mutate(barmax = cumsum(value), barmin = replace_na(lag(barmax),0)) |> ungroup() |>
    arrange(!!grouping) %>%
    mutate(plot.type = str_replace_all(!!grouping, p$trace_types)) |> group_by(year, !!grouping) |> group_split()
  color_vector <- p$color_vector |> farver::decode_colour() |> farver::encode_colour(alpha = 0.5)
  pred_groups <- (pred_series |> reduce(bind_rows))[[as_name(grouping)]] |> unique()
  if(!all(pred_groups %in% names(color_vector))) {
    stop("All prediction traces must have a correspondingly named trace in original plot.", call. = F)
  }
  legend.items <- c()
  for (s in pred_series) {
    s.name <- unique(s[[as_name(grouping)]]) %>% as.character()
    s.level <- p$legend_ranks[s.name]
    s.linewidth <- unique(s$line.width)
    show.legend <- ifelse(!s.name %in% legend.items, showlegend, F)
    legend.items <- c(legend.items, s.name) |> unique()
    legend.rank <- s.level * 1.1 + 1
    s.type <- unique(s$plot.type)
    if(s.type == "bar") {

      s <- s %>% mutate(time = year) %>% select(-year) %>% distinct()
      template <- ptt_plot_hovertemplate(hovertext) %>% str_replace("\\{text\\}","\\{customdata\\}")

      p <- p |>
        add_trace(y = s$value , x = s$time, text = NA, type = "bar",
                  color = I(color_vector[s.name]),
                  name = ifelse(with_labs == T, str_c(s.name,", ennuste"), "Ennuste"),
                  legendgroup = s.name,
                  hoverinfo = NA,
                  texttemplate = NA,
                  customdata = s.name,
                  offsetgroup = s.name,
                  legendrank = legend.rank,
                  showlegend = show.legend,
                  hovertemplate = template)
    } else {
      p <- p |>
        add_trace(y = s$value , x = s$time, text = s[[as_name(grouping)]], type = "scatter", mode = "lines",
                  line = if(s.type == "scatter") { list(width = s.linewidth) } else { NULL },
                  color = I(color_vector[s.name]),
                  name = ifelse(with_labs == T, str_c(s.name,", ennuste"), "Ennuste"),
                  legendgroup = s.name,
                  hoverinfo = text,
                  legendrank = legend.rank,
                  showlegend = show.legend,
                  hovertemplate = ptt_plot_hovertemplate(hovertext))
    }
  }
  pred_d <- pred_series |> map_dfr(~.x) |>
    mutate(time = year) %>% select(-year) %>% distinct() %>%
    rename(csv.data.tiedot = !! rlang::sym(rlang::quo_name(grouping))) |>
    select(csv.data.tiedot, time, value) |>
    distinct(csv.data.tiedot, time, value) |>
    mutate(
      csv.data.tiedot = str_c(csv.data.tiedot, ", ennuste"),
      across(everything(), ~ as.character(.x)),
      value = str_replace(value, "\\.",",")
    )
  p$data <- p$data |>
    bind_rows(pred_d) |>
    arrange(time, csv.data.tiedot)
  p |>
    ptt_plot_add_rangeslider(enable = range.slider$enable, height = range.slider$size, slider_range = range.slider$range) |>
    ptt_plot_set_modebar(p$title, p$subtitle, p$png_attrs, T)
}


#' Add secondary traces of existing traces with reduced linewidth to an existing ptt_plot.
#'
#' Outputs a plotly object.
#'
#' @param p Plotly object created with ptt_plot to add prediction traces to.
#' @param secondary_data Tibble with secondary variables of one grouping in the parent ptt_plot object.
#' @param relates_to The name of the grouping that the secondary data relates.
#' @param grouping Tibble column used for grouping in plot, should be labeled the same as data used for parent ptt_plot.
#' @param hovertext Uses parent ptt_plot specification if undefined. A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)", dateformat = "%Y-%m-&d")".
#' @param showlegend A locigal to show legend for secondary trace.
#' @return plotly object
#' @examples
#' d2 <- d |> dplyr::filter(tiedot == "BKT") |> dplyr::mutate(value = statfitools::trend_series(value, time)) |> dplyr::mutate(tiedot = "esimerkkisarja")
#  p |> ptt_plot_add_secondary_traces(d2, BKT, tiedot)
#' @export
#' @importFrom tidyr uncount pivot_longer
#' @importFrom dplyr slice_tail
#' @importFrom forcats fct_inorder
#' @importFrom plotly plot_ly add_lines
#' @importFrom stringr str_replace

ptt_plot_add_secondary_traces <-
  function(p, secondary_data, relates_to, grouping,
           hovertext, showlegend = TRUE) {

    if(showlegend == T) { message("Secondary trace showlegend == T does not currently perform as expected.") }

    grouping <- enquo(grouping)

    if (missing(relates_to) | missing(secondary_data)) {
      stop("Define the relation to parent ptt_plot by providing both the data with the secondary data and the relates_to variable.", call. = F)
    }

    relates_to <- as_name(enquo(relates_to))

    if(!relates_to %in% names(p$color_vector)) {
      stop("Provided relates_to not in parent ptt_plot variables!", call. = F)
    }

    if (missing(hovertext)) { hovertext <- p$hover_template }

    d <- droplevels(secondary_data)

    if(!is.factor(d[[as_name(grouping)]])) {
      d[[as_name(grouping)]] <- fct_inorder(d[[as_name(grouping)]])
    }

    unique_groups <- d[[as_name(grouping)]] |> unique() |> sort()

    split_d <- group_split(d, !!grouping) |> rev()
    for (g in split_d) {
      g.name <- unique(g[[as_name(grouping)]])
      g.level <- which(g.name == levels(g.name))
      lw <- seq.int(2,1,length.out = length(levels(g.name)))[g.level]
      g.name <- str_c(relates_to,", ",tolower(g.name))
      legend.rank <- p$legend_ranks[as_name(relates_to)] + (g.level*10)
      p <-
        p |>
        add_trace(data=g, y = ~value, text = g.name,
                  hovertemplate = ptt_plot_hovertemplate(hovertext),
                  line = list(width = lw),
                  legendgroup = relates_to,
                  legendrank = legend.rank,
                  showlegend = showlegend,
                  name = g.name,
                  color = I(p$color_vector[relates_to]), type = "scatter", mode ='lines'
        )
    }

    sec_d <- map_dfr(split_d, ~ .x) |>
      rename(csv.data.tiedot = !! rlang::sym(rlang::quo_name(grouping))) |>
      select(csv.data.tiedot, time, value) |>
      mutate(
        csv.data.tiedot = str_c(as_name(relates_to),", ",csv.data.tiedot),
        across(everything(), ~ as.character(.x)),
        value = str_replace(value, "\\.",",")
      )
    p$data <- bind_rows(p$data, sec_d) |> arrange(time, csv.data.tiedot)
    p |> ptt_plot_set_modebar(p$title, p$subtitle, p$png_attrs, T)
  }

#' @importFrom dplyr group_split
#' @importFrom purrr map
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom stringr str_replace_all
ptt_plot_get_facet_plot <- function(d, facet_split, height, grouping, plot_type, trace_color, highlight, color_vector, grid_color, hovertext, plot_mode, font_size, font_color, ticktypes, axis_limits) {

  split_facet <- d %>% group_split(!!facet_split)

  p <- map2(split_facet,seq(length(split_facet)), function(facet, i) {

    p <- plot_ly(facet, x = ~time, height = height)

    split_d <- group_split(facet, !!grouping)
    split_d <- if(any(plot_type == "scatter")) { rev(split_d) } else { split_d }

    for (g in split_d) {
      g.color <- unique(g[[as_name(grouping)]])
      g.name <- unique(g[[as_name(grouping)]])
      g.level <-  which(g.name == levels(g.name))
      g.type <- unique(g$plot.type)
      g.linewidth <- unique(g$line.width)
      legend.rank <- g.level * 100
      show.legend <- if (i > 1) {F} else if(!is.null(trace_color)) { T } else { ptt_plot_set_highlight(highlight, g) }
      trace.color <- I(color_vector[as.character(g.color)])
      if(is.na(trace.color)) {trace.color <- I(grid_color)}
      p <- p |>
        add_trace(data=g, y = ~value, text = g.name,
                  texttemplate = NA,
                  hovertemplate = ptt_plot_hovertemplate(hovertext),
                  line = if(g.type == "scatter") { list(width = g.linewidth) } else { NULL },
                  offsetgroup = if(g.type == "bar") { g.name } else { NULL },
                  legendgroup = g.name,
                  legendrank = legend.rank,
                  showlegend = show.legend,
                  name = g.name,
                  color = trace.color,
                  type = g.type, mode = if(g.type == "scatter") { "lines" } else { NULL }
        )
    }

    if(!plot_mode %in% c("dodge","stack","horizontal")) {
      stop("Plot mode must be \"dodge\", \"stack\" or \"horizontal\"!", call. = F)
    } else {
      p_mode <- str_replace_all(plot_mode, c("dodge|horizontal" = "group", "stack" = "relative"))
      p  <- layout(p, barmode = p_mode)
    }
    if(i > 1) {
      p <- p %>%
        ptt_plot_set_ticks(list(size = font_size, family = "sans-serif", color = font_color), ticktypes = ticktypes) %>%
        layout(yaxis = list(range = axis_limits$y, showticklabels = F, showline = T))
    }

    p

  })

  subplot(p)
}

#' @importFrom rlang sym
#' @importFrom dplyr mutate group_by ungroup
get_bar_widths <- function(df, width_col) {
  get_offset <- function(the_count) {
    seq(-0.45, length.out = the_count, by = 1/the_count)
  }
  df %>%
    add_count((!!sym(width_col)) , name = "bar_width") %>%
    group_by((!!sym(width_col)) ) %>%
    mutate(bar_offset = get_offset(max(bar_width)),
           bar_width = ifelse(row_number() == last(row_number()), 1/bar_width*0.9, 1/bar_width)) %>%
    ungroup()

}

#' @importFrom dplyr group_split
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom stringr str_replace_all str_trunc
#' @importFrom stats as.formula
ptt_plot_get_plot <- function(d, xaxis, yaxis, height, grouping, plot_type, trace_color, highlight, color_vector, grid_color, hovertext, plot_mode) {

  # print(plot_mode)
  if(plot_mode == "horizontal") {
    d <- get_bar_widths(d, yaxis)
    hovertemplate <- "%{text}<br>%{customdata} <br>%{x}<extra></extra>"
  } else {
    hovertemplate <- ptt_plot_hovertemplate(hovertext)

  }

  p <- plot_ly(d, x = as.formula(str_c("~",xaxis)), height = height)

  split_d <- group_split(d, !!grouping)
  split_d <- if(any(plot_type == "scatter")) { rev(split_d) } else { split_d }

  for (g in split_d) {
    g.text <- unique(g[[as_name(grouping)]])
    if(plot_mode == "horizontal") {
      g.fullname <- g[[names(plot_mode)]]
      g[[names(plot_mode)]] <- fct_relabel(g[[names(plot_mode)]], ~ str_trunc(.,40, ellipsis = ".."))
    } else { g.fullname <- NA}
    g.color <- unique(g[[as_name(grouping)]])
    g.name <- unique(g[[as_name(grouping)]])
    g.text <- unique(g[[as_name(grouping)]])
    g.level <-  which(g.name == levels(g.name))
    g.type <- unique(g$plot.type)
    g.linewidth <- unique(g$line.width)
    legend.rank <- g.level * 100
    show.legend <- if(!is.null(trace_color)) { T } else { ptt_plot_set_highlight(highlight, g) }
    trace.color <- I(color_vector[as.character(g.color)])
    if(is.na(trace.color)) {trace.color <- I(grid_color)}
    p <- p |>
      add_trace(data=g, y = as.formula(str_c("~",yaxis)),
                text = g.text,
                customdata = g.fullname,
                texttemplate = NA,
                hovertemplate = hovertemplate,
                line = if(g.type == "scatter") { list(width = g.linewidth) } else { NULL },
                offsetgroup = if(g.type == "bar") { g.name } else { NULL },
                legendgroup = g.name,
                legendrank = legend.rank,
                showlegend = show.legend,
                orientation = ifelse(plot_mode == "horizontal","h","v"),
                offset = if(plot_mode == "horizontal") { ~bar_offset } else { NULL },
                width = if(plot_mode == "horizontal") { ~bar_width } else { NULL },
                name = g.name,
                color = trace.color,
                type = g.type,
                mode = if(g.type == "scatter") { "lines" } else { NULL }
      )
  }

  if(!plot_mode %in% c("dodge","stack","horizontal")) {
    stop("Plot mode must be \"dodge\", \"stack\" or \"horizontal\"!", call. = F)
  } else {
    plot_mode <- str_replace_all(plot_mode, c("dodge|horizontal" = "group", "stack" = "relative"))
    p  <- layout(p, barmode = plot_mode)
  }

  p

}


#' @importFrom rlang as_name
#' @importFrom dplyr summarize filter pull
ptt_plot_get_color_vector <- function(trace_color, unique_groups, highlight, d, grouping) {
  if(!is.null(trace_color)) {

    if(!all(ptt_plot_are_colors(trace_color))) {
      stop("Trace colors must be 6-character hexadecimal colors or among strings provided by grDevices::colors!", call. = F)
    } else if (length(trace_color) == 1 & is.null(names(trace_color))){
      color_vector <- rep(trace_color, length(unique_groups)) %>% set_names(unique_groups)
    } else if (!all(unique_groups %in% names(trace_color)) & !(".other" %in% names(trace_color)) ) {
      stop(str_c("Either trace color must be a single color string, or all variables in column \"",as_name(grouping),"\" must have a corresponding trace color, or key \".other\" must be included, or trace_color must be NULL!"), call. = F)
    } else {
      ug <- as.character(unique_groups)
      missing_groups <- ug %>% subset(!. %in% names(trace_color))
      if(length(missing_groups) > 0) {
        detected_traces <- map2(unname(trace_color), names(trace_color), function(tc,nm) {
          miss <- missing_groups %>% subset(str_detect(., str_c(nm, collapse = "|")))
          rep(tc, length(miss)) %>% setNames(miss)
        }) %>% purrr::compact() %>% unlist()
        trace_color <- c(trace_color, detected_traces)
      }
      color_vector <- c(trace_color[ug[ug %in% names(trace_color)]],
                        ug[!ug %in% names(trace_color)] %>% length() %>% rep(trace_color[".other"], .) %>%
                          set_names(ug[!ug %in% names(trace_color)]))
    }
    if(!is.null(highlight)) {message("Highlight is ignored when providing trace colors.")}

  } else if(!is.null(highlight)) {
    if (is.double(highlight)) {
      un_groups <- d %>% group_by(!!grouping) %>% summarize(value = max(value, na.rm = T), .groups = "drop") %>% filter(value >= highlight) %>% pull(!!grouping)
    } else if (is.list(highlight)) {
      if (!all(c("value",".fun") %in% names(highlight))) {
        stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
      } else {
        un_groups <- d %>% group_by(!!grouping) %>% summarize(value = highlight$.fun(value, na.rm = T), .groups = "drop") %>% filter(value >= highlight$value) %>% pull(!!grouping)
      }
    } else {
      stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
    }
    un_groups <- unique_groups %>% subset(unique_groups %in% un_groups) |> droplevels()
    if(length(un_groups) == 0) { stop(str_c("No trace in \"",as_name(grouping),"\" fulfill the highlight requirements."), call. = F) }
    color_vector <- ptt_plot_set_colors(length(un_groups)) %>% set_names(un_groups)
    d[[as_name(grouping)]] <- fct_relevel(d[[as_name(grouping)]], levels(un_groups))
  } else {
    color_vector <- ptt_plot_set_colors(length(unique_groups)) |> set_names(unique_groups)
  }

  color_vector
}


#' @importFrom rlang sym quo_name
#' @importFrom tidyr unite
#' @importFrom dplyr select mutate across everything rename
#' @importFrom stringr str_replace
ptt_plot_transform_data_for_download <- function(d, grouping, facet_split, plot_mode) {
  d <- d |> rename(csv.data.tiedot = !! sym(quo_name(grouping)))
  if(!missing(facet_split)) { d <- unite(d, csv.data.tiedot, csv.data.tiedot, !!facet_split, sep = ", ")}
  if(plot_mode == "horizontal") { d <- unite(d, csv.data.tiedot, csv.data.tiedot, sym(quo_name(names(plot_mode))))}
  d |>
    select(csv.data.tiedot, time, value) |>
    mutate(
      across(everything(), ~ as.character(.x)),
      value = str_replace(value, "\\.",",")
    )
}
