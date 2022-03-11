#' @importFrom plotly layout
ptt_plot_set_grid <- function(p, grid_color) {
  p |> layout(
    xaxis = list(showgrid = TRUE, gridcolor = grid_color, size = 1.5),
    yaxis = list(showgrid = TRUE, gridcolor = grid_color, size = 1.5)
  )
}

#' @importFrom plotly layout config
ptt_plot_set_defaults <- function(p) {
  config(p, locale = "fi") |>
    layout(separators = ", ") |>
    layout(xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE)) |>
    layout(hovermode = "compare") |>
    ptt_plot_set_axis_labels()

}

#' @importFrom stringr str_c str_extract_all str_replace_all str_squish
#' @importFrom htmlwidgets JS
#' @importFrom plotly config
ptt_plot_set_modebar <- function(p, dl_title,png_layout) {

  data_dl_icon <- "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"
  chalkboard_icon <- "M69.53,91.55l13.23,13.23c3.2,0.09,5.77,2.72,5.77,5.95c0,3.29-2.66,5.95-5.95,5.95c-3.29,0-5.95-2.67-5.95-5.95 c0-0.36,0.03-0.72,0.09-1.07L65.21,98.16v8.46l-8.24,0v-8.23l-11.69,11.7c0.02,0.21,0.03,0.43,0.03,0.65c0,0,0,0,0,0 c0,3.29-2.66,5.95-5.96,5.95c-3.29,0-5.95-2.67-5.95-5.95c0-3.29,2.67-5.95,5.95-5.95c0.1,0,0.2,0,0.29,0.01l13.23-13.23L0,91.55 V15.71c0-0.05,0-0.09,0-0.14V7.52c0-0.87,0.72-1.57,1.61-1.57h55.36V0h8.24v5.95h56.06c0.89,0,1.61,0.71,1.61,1.57v8.05 c0,0.03,0,0.05,0,0.08v75.89H69.53L69.53,91.55z M26.89,62.71l23.26-22.74c5.76,5.76,11.46,11.46,17.28,17.21l15.41-15.6 l-7.15-7.15l20.12-0.18v20.29l-6.87-6.87c-7.16,7.25-14.29,14.48-21.47,21.67L50.03,52.12L32.99,68.81L26.89,62.71L26.89,62.71 L26.89,62.71z M113.79,21.73H8.92v60.64h104.87V21.73L113.79,21.73z"
  twitter_icon <- "M640.012 121.513c-23.528 10.524-48.875 17.516-75.343 20.634 27.118-16.24 47.858-41.977 57.756-72.615-25.347 14.988-53.516 25.985-83.363 31.866-24-25.5-58.087-41.35-95.848-41.35-72.508 0-131.21 58.736-131.21 131.198 0 10.228 1.134 20.232 3.355 29.882-109.1-5.528-205.821-57.757-270.57-137.222a131.423 131.423 0 0 0-17.764 66c0 45.497 23.102 85.738 58.347 109.207-21.508-.638-41.74-6.638-59.505-16.359v1.642c0 63.627 45.225 116.718 105.32 128.718-11.008 2.988-22.63 4.642-34.606 4.642-8.48 0-16.654-.874-24.78-2.35 16.783 52.11 65.233 90.095 122.612 91.205-44.989 35.245-101.493 56.233-163.09 56.233-10.63 0-20.988-.65-31.334-1.89 58.229 37.359 127.206 58.997 201.31 58.997 241.42 0 373.552-200.069 373.552-373.54 0-5.764-.13-11.35-.366-16.996 25.642-18.343 47.87-41.493 65.469-67.844l.059-.059z"
  png_icon <- "M7.5,0h107.9c4.1,0,7.5,3.4,7.5,7.5v70.6c0,4.1-3.4,7.5-7.5,7.5H7.5c-4.1,0-7.5-3.4-7.5-7.5V7.5 C0,3.4,3.4,0,7.5,0L7.5,0z M69.9,63.3h28.5v4H69.9V63.3L69.9,63.3z M69.9,53.1H109v4H69.9V53.1L69.9,53.1z M92.1,35h5.6 c0.3,0,0.5,0.2,0.5,0.5v11c0,0.3-0.2,0.5-0.5,0.5h-5.6c-0.3,0-0.5-0.2-0.5-0.5v-11C91.6,35.3,91.8,35,92.1,35L92.1,35L92.1,35z M70.5,28.3h5.6c0.3,0,0.5,0.2,0.5,0.5v17.8c0,0.3-0.2,0.5-0.5,0.5h-5.6c-0.3,0-0.5-0.2-0.5-0.5V28.8 C69.9,28.5,70.2,28.3,70.5,28.3L70.5,28.3L70.5,28.3L70.5,28.3z M81.3,24.5h5.6c0.3,0,0.5,0.2,0.5,0.5v21.6c0,0.3-0.2,0.5-0.5,0.5 h-5.6c-0.3,0-0.5-0.2-0.5-0.5V25C80.8,24.7,81,24.5,81.3,24.5L81.3,24.5L81.3,24.5z M39.3,48.2l17,0.3c0,6.1-3,11.7-8,15.1 L39.3,48.2L39.3,48.2L39.3,48.2z M37.6,45.3l-0.2-19.8l0-1.3l1.3,0.1h0h0c1.6,0.1,3.2,0.4,4.7,0.8c1.5,0.4,2.9,1,4.3,1.7 c6.9,3.6,11.7,10.8,12.1,19l0.1,1.3l-1.3,0l-19.7-0.6l-1.1,0L37.6,45.3L37.6,45.3L37.6,45.3z M39.8,26.7L40,44.1l17.3,0.5 c-0.7-6.8-4.9-12.7-10.7-15.8c-1.2-0.6-2.5-1.1-3.8-1.5C41.7,27.1,40.8,26.9,39.8,26.7L39.8,26.7L39.8,26.7z M35.9,47.2L45.6,64 c-3,1.7-6.3,2.6-9.7,2.6c-10.7,0-19.4-8.7-19.4-19.4c0-10.4,8.2-19,18.6-19.4L35.9,47.2L35.9,47.2L35.9,47.2z M115.6,14.1H7.2v64.4 h108.4V14.1L115.6,14.1L115.6,14.1z"

  if(is.na(dl_title)) {
    dl_title <- "img"
  } else {
    dl_title <- str_extract_all(dl_title, "[a-zåäö,A-ZÅÄÖ,\\s,_,\\.,0-9]", simplify = T) |>
      str_c(collapse = "") |>
      str_squish() |>
      tolower() |>
      str_replace_all(c("ä" = "a", "å" = "o", "ö" = "o", " |\\." = "_"))
  }

  js_string <- function(wd, ht, suffix, layout, ttl = dl_title) {
    #legend_offset = legend_offset_sm, caption_offset = caption_offset_sm
    str_c('
          function(gd) {
          delete gd.layout.xaxis.rangeslider;
          //gd.layout.margin.b = ',layout$margin,';
          gd.layout.images[0].sizey = ',layout$logo_sizing,';
          gd.layout.images[0].y = ',layout$logo_offset,';
          gd.layout.annotations[0].y = ',layout$caption_offset,';
          gd.layout.annotations[1].y = ',layout$caption_offset,';
          gd.layout.annotations[2].y = ',layout$caption_offset,';
          gd.layout.legend.y = ',layout$legend_offset,';
          gd.layout.xaxis.tickfont.size = ',layout$font_sizing$main,';
          gd.layout.yaxis.tickfont.size = ',layout$font_sizing$main,';
          gd.layout.annotations[0].font.size = ',layout$font_sizing$caption,';
          gd.layout.annotations[1].font.size = ',layout$font_sizing$caption,';
          gd.layout.annotations[2].font.size = ',layout$font_sizing$caption,';
          gd.layout.title.font.size = ',layout$font_sizing$title,';
          //alert(JSON.stringify((gd.layout.title)));
          Plotly.downloadImage(gd, {format: "png", width: ',wd,', height: ',ht,', filename: "',ttl,'_',suffix,'"});
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

  dl_conference_btn <- list(
    name = "Lataa kuva (leveä)",
    icon = list(
      path = chalkboard_icon,
      transform = "scale(0.14) translate(-4.5, 0.2)"
    ),
    click = JS(js_string(1280,720,"levea",png_layout$lg))
    )

  dl_btn <- list(
    name = "Lataa kuva (kapea)",
    icon = list(
      path = png_icon,
      transform = "scale(0.14) translate(-4.5, 0.2)"
    ),
    click = JS(js_string(640,720,"kapea",png_layout$lg)))

  dl_twitter_btn <- list(
    name = "Lataa kuva (pieni)",
    icon = list(
      path = twitter_icon,
      transform = "scale(0.022) translate(0.2, 0.4)"
    ),
    click = JS(js_string(889,500,"pieni",png_layout$sm)))

  data_dl_btn <- list(
    name = "Lataa tiedot",
    icon = list(
      path = data_dl_icon,
      transform = "scale(0.84) translate(-1, -1)"
    ),
    click = JS(str_c("
          function(gd) {
            let text = ''
            for(var i = 0; i < gd.data.length; i++){
              console.log(gd.data[i])
              text = text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
              text = text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
            };
            var blob = new Blob([text], {type: 'text/plain'});
            var a = document.createElement('a');
            const object_URL = URL.createObjectURL(blob);
            a.href = object_URL;
            a.download = '",dl_title,"_data.csv2';
            document.body.appendChild(a);
            a.click();
            URL.revokeObjectURL(object_URL);
          }
   ")))

  p |> config(
    displaylogo = FALSE,
    modeBarButtons = list(list(
      dl_btn,
      dl_twitter_btn,
      dl_conference_btn,
      data_dl_btn,
      robonomist_btn
    ))
  )
}

#' @importFrom plotly layout
ptt_plot_set_ticks <- function(p, font) {
  p |>
    layout(xaxis=list(tickfont = font,
                      mirror = TRUE,
                      ticks = 'outside',
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
                      showline = TRUE),
           yaxis=list(tickfont = font,
                      tickformat = ",.2~r",
                      ticksuffix = " ",
                      mirror = TRUE,
                      ticks = 'outside',
                      showline = TRUE))

}

#' @importFrom plotly layout
ptt_plot_set_margin <-function(p, margin) {

  if (!is.list(margin)) {
    stop("Insert plot margins as list (default is list(t,r,b,l,pad))")
  } else if (any(!names(margin) %in% c("t","r","b","l","pad")) | any(!is.double(unlist(margin)))) {
    stop("All plot margins must be of double type, and named one or more of t, r, b, l or pad.")
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
ptt_plot_add_logo <- function(p, offset, plot_height){

  image_file <- system.file("image", "ptt-logo.png", package = "pttrobo")
  txt <- base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  img_size <- 20 / plot_height
  p |> plotly::layout(
    images = list(
      source = paste('data:image/png;base64', txt, sep=','),
      xref = "paper",
      yref = "paper",
      x = 1,
      y = offset$y,
      sizex = 1,
      sizey = img_size,
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
    p |> rangeslider(slider_range[1],slider_range[2], thickness = height)
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
                            xaxis_offset = 0,
                            margin = NA,
                            zeroline = F,
                            enable_rangeslider = list(rangeslider = F, max = as_date(today))) {

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
      margin <- list(t = 35, r = 20, b = 0, l = 0, pad = 0)
      margin$t <- case_when(str_length(subtitle) > 0 ~ margin$t + 50, TRUE ~ margin$t)
      margin$b <- case_when(!is.na(caption) || enable_rangeslider ~ margin$b + round(0.35*height),
                            TRUE ~ margin$b)
    }
  }



  main_font <- list(size = font_size, family = "sans-serif", color = font_color)
  title_font <- list(size = round(font_size*1.3), family = "sans-serif", color = font_color)

  rangeslider_size <- 0.1#round(1-((height/1000)^(1/2)),2)

  ht_constants <- 4 # 2 pikseliä korkeudesta häviää aina + 1*2 plotin reunat, tick pituus ei lasketa eli 0
  tickfont_ht <- round((font_size+2)*1.4)#20#+max(0,ceiling(main_font$size-7)) #20+tickfontin korkeus mikä ylittää tämän: fontti 14 = 3 ei ole täysin johdonmukainen y-akselin yläreunan vuoksi. -Suunnilleen - +1.6 / piste yli fontti 12
  rangeslider_ht <- ifelse(enable_rangeslider, round((height * rangeslider_size)+15-(margin$t*rangeslider_size)-(margin$b*rangeslider_size)),0)
  legend_ht <- ifelse(legend_position %in% c("auto","bottom"), font_size, 0)
  plot_ht <- height-sum(ht_constants,rangeslider_ht,tickfont_ht,legend_ht,margin$t,ifelse(enable_rangeslider, margin$b, margin$b-25))
  caption_ht <- ifelse(!is.na(caption), round(font_size*0.8), 0)
  legend_offset <- list(x = 0,#-((38)/plot_wd),
                        y = -((rangeslider_ht+(margin$b*ifelse(enable_rangeslider, 0.12, 0.24)))/plot_ht))
  caption_offset <- list(x = 0,#-((36)/plot_wd),
                         y = -((tickfont_ht+4+rangeslider_ht+legend_ht+(margin$b*ifelse(enable_rangeslider, 0.12, 0.24)))/plot_ht))
  logo_offset <- list(x = 0, y = -((tickfont_ht+4+rangeslider_ht+caption_ht+legend_ht+margin$b*ifelse(enable_rangeslider, 0.12, 0.24))/plot_ht))

  png_attrs <- (function(small_ht = 500, large_ht = 720) {
    png_margin_b_sm <- max(0, ifelse(is.na(caption), round(0.35*small_ht), 0)-15)
    png_margin_b_lg <- max(0, ifelse(is.na(caption), round(0.35*large_ht), 0)-15)
    ht_sm <- small_ht-sum(ht_constants,tickfont_ht,legend_ht,margin$t,png_margin_b_sm)
    ht_lg <- large_ht-sum(ht_constants,tickfont_ht,legend_ht,margin$t,png_margin_b_lg)
    legend_offset_sm <- -(60/ht_sm)
    legend_offset_lg <- -(50/ht_lg)
    caption_offset_sm <- -((tickfont_ht*4+legend_ht*3+(png_margin_b_sm*0.24))/ht_sm)
    caption_offset_lg <- -((tickfont_ht*3+legend_ht*2+(png_margin_b_lg*0.24))/ht_lg)
    logo_offset_sm <- -((tickfont_ht*4+caption_ht*2+legend_ht*3+(png_margin_b_sm*0.24))/ht_sm)
    logo_offset_lg <- -((tickfont_ht*3+caption_ht*2+legend_ht*2+(png_margin_b_lg*0.24))/ht_lg)
    logo_sizing_sm <-  40 / ht_sm
    logo_sizing_lg <- 30 / ht_lg
    font_sizing_lg <- list(title = 23, main = 20, caption = 12)
    font_sizing_sm <- list(title = 23, main = 18, caption = 14)
    list(sm = list(legend_offset = legend_offset_sm, caption_offset = caption_offset_sm, logo_offset = logo_offset_sm, logo_sizing = logo_sizing_sm, margin = png_margin_b_sm, font_sizing = font_sizing_sm),
         lg = list(legend_offset = legend_offset_lg, caption_offset = caption_offset_lg, logo_offset = logo_offset_lg, logo_sizing = logo_sizing_lg, margin = png_margin_b_lg, font_sizing = font_sizing_lg))
  })()

  p$enable_rangeslider <- list(enable = enable_rangeslider, size = rangeslider_size, range = slider_range)
  p$add_zeroline <- zeroline

  p |>
    ptt_plot_set_defaults() |>
    ptt_plot_set_grid(grid_color) |>
    ptt_plot_set_modebar(title, png_attrs) |>
    ptt_plot_set_ticks(main_font) |>
    ptt_plot_set_margin(margin) |>
    ptt_plot_add_logo(logo_offset, plot_ht) |>
    ptt_plot_add_rangeslider(enable_rangeslider, rangeslider_size, slider_range = slider_range) |>
    ptt_plot_set_legend(legend_position, legend_orientation, offset = legend_offset, main_font) |>
    ptt_plot_set_title(title, subtitle, title_font) |>
    ptt_plot_set_caption(caption, offset = caption_offset, main_font) %>%
    ptt_plot_add_zeroline(zeroline)
}


#' @importFrom plotly layout
#' @importFrom stringr str_extract
ptt_plot_set_legend <- function(p, position, orientation, offset, font) {
  if (is.na(position)) {
    p |> layout(showlegend = F)
  } else if (!position %in% c("right","bottom")) {
    stop("Position must be one of \"right\", \"left\", or NA")
  } else if (!orientation %in% c("vertical","horizontal","auto")) {
    stop("Orientation must be one of \"horizontal\", \"vertical\", or \"auto\"")
  } else {
    x.pos <- ifelse(position == "right", 100, offset$x)
    y.pos <- ifelse(position == "right", 1, offset$y) #-0.05
    orientation <- case_when(orientation == "auto" ~ ifelse(position == "right", "v","h"), TRUE ~ str_extract(orientation, "^(v|h)"))
    p |> layout(
      showlegend = T,
      legend = list(font = font,
                    x = x.pos, y = y.pos,
                    traceorder = "normal",
                    orientation = orientation,
                    xanchor = "left",
                    yanchor = "top"))
  }
}


#' @importFrom plotly layout
ptt_plot_set_caption <- function(p, caption, offset = list(x = 0, y = 0), font) {
  if(is.na(caption)) {
    p
  } else if (!is.character(caption)) {
    stop("Caption and caption footer must be a string, or caption = NA for no plot caption.", call. = F)
  } else {
    font$size <- round(font$size * 0.8)
    p |>
      layout(
        annotations = list(
          x = offset$x, y = offset$y, text = caption, align = "left",
          showarrow = F, xref = 'paper', yref = "paper",
          xanchor='left', yanchor = 'top', xshift=0, yshift=0,
          font = font
        )
      )
  }
}

#' @importFrom plotly layout
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
            "<span><b>", title, "</b>", "<br>",
            tags$sub(subtitle),"</span>"
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
    stop("Zeroline must be TRUE, FALSE or of type double.")
  } else if (z$zeroline == F) {
    p
  } else {
    zero_line <- ifelse(z$zeroline == T, 0, z$zeroline)
    p %>% layout(shapes= list(type = "line", x0 = z$xrange$min, x1 = z$xrange$max, xref = "x", y0 = zero_line, y1 = zero_line, yref = "y"))
  }
}

#' @importFrom stringr str_subset
ptt_plot_hovertemplate <- function(specs) {
  if (is.null(specs)) {
    NA
  } else if (!is.list(specs)) {
    stop("Hovertemplate must be a list with any of \"rounding\", \"unit\", or \"extra\", or NULL", call. = F)
  } else if (!any(c("rounding","unit","extra") %in% names(specs))) {
    stop("Hovertemplate must be a list with any of \"rounding\", \"unit\", or \"extra\", or NULL", call. = F)
  } else {
    specs_template <- list(rounding = 1, unit = "%", extra = "")
    for (spec.name in (names(specs) |> str_subset("^(rounding|unit|extra)$"))) {
      if(spec.name == "rounding") {
        if ((specs$rounding)%%1!=0) { stop("Hovertemplate rounding must be an integer.", call. = F) }
      } else if (!is.character(specs[[spec.name]])) {
        stop(str_c("Hovertemplate ",spec.name," must be a string.", call. = F))
      }
      specs_template[[spec.name]] <- specs[[spec.name]]
    }
    paste0("%{text}<br>%{y:.",specs_template$rounding,"f} ",specs_template$unit,"<br>%{x|%Y-%m-%d}",ifelse(specs_template$extra == "", "", paste0("<br>",str_c(specs_template$extra, collapse = " "))),"<extra></extra>")
  }
}

#' Writes an html element for embedding.
#'
#' @param p A plotly object.
#' @param title The filename of the html element (without file format). The function will clean the name up, or try to extract it from param p if missing.
#' @examples
#' p |> ptt_plot_create_widget()
#' @return None
#' @importFrom stringr str_extract_all
#' @importFrom htmlwidgets saveWidget
ptt_plot_create_widget <- function(p,title) {
  tofilename <- function(str) {
    str_extract_all(str, "[a-zåäö,A-ZÅÄÖ,\\s,_,\\.,0-9]", simplify = T) |>
      str_c(collapse = "") |>
      str_squish() |>
      tolower() |>
      str_replace_all(c("ä" = "a", "å" = "o", "ö" = "o", " |\\." = "_"))
  }
  if(missing(title)) {
    title <- (p$x$layoutAttrs |> unlist())[grep("title.text", names((p$x$layoutAttrs |> unlist())))] |>
      str_extract_all("(?<=\\>)[^\\<\\>]{2,}(?=\\<)") %>% unlist() %>% str_c(collapse = "_") %>% tofilename()
    message(str_c("Using \"",title,"\" for htmlwidget filename.."))
  } else {
    title <- tofilename(str)
  }

  p |>
    htmlwidgets::saveWidget(str_c(title,".html"), selfcontained = F, libdir = "plot_dependencies")

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
  ptt_vihrea <- "#5B8233"
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
#' @param hovertext A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)")".
#' @param isolate_primary Separates the first factor in grouping by line width. Use ptt_add_secondary_traces for multiple series with multiple secondary traces.
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
#' @importFrom scales extended_breaks
#' @importFrom rlang enquo as_name set_names
#' @importFrom dplyr group_split
#' @importFrom stringr str_length
ptt_plot <- function(d,
                     grouping,
                     title,subtitle = "",
                     caption,
                     legend_orientation = "auto",
                     legend_position = "bottom",
                     margin = NA,
                     font_color = "#696969",
                     grid_color = "#E8E8E8",
                     zeroline = F,
                     rangeslider = T,
                     isolate_primary = F,
                     font_size = 18,
                     hovertext = list(rounding = 1, unit = "", extra = ""),
                     height = 600,
                     ...
){

  if(missing(d) || missing(grouping)){
    stop("Data and grouping variable (without quotes)\nFor example: ptt_plot(a_tibble, grouping=tiedot)", call. = F)
  }
  grouping <- enquo(grouping)

  d <- droplevels(d)

  unique_groups <- d[[as_name(grouping)]] |> unique() |> sort()

  color_vector <- (function() {
    color_vector <- if (isolate_primary == T) {
      ptt_plot_set_colors(1) %>% rep(length(unique_groups))
    } else {
      ptt_plot_set_colors(length(unique_groups))
    }
    color_vector |> set_names(unique_groups)
  })()

  p <- plot_ly(d, x = ~ time, height = height)

  split_d <- group_split(d, !!grouping) |> rev()

  for (g in split_d) {
    g.color <- unique(g[[as_name(grouping)]])
    g.name <- unique(g[[as_name(grouping)]])
    g.level <-  which(g.name == levels(g.name))
    lw <- if(!isolate_primary) { 4 } else {seq.int(4,1,length.out = length(levels(g.name)))[g.level]}
    legend.rank <- g.level * 100
    p <- p |>
      add_trace(data=g, y = ~value, text = g.name,
                hovertemplate = ptt_plot_hovertemplate(hovertext),
                line = list(width = lw),
                legendgroup = g.name,
                legendrank = legend.rank,
                name = g.name,
                color = I(color_vector[g.color]), type = "scatter", mode ='lines'
      )
  }
  p$color_vector <- color_vector
  p$hover_template <- hovertext
  p$legend_ranks <- ((levels(unique_groups) |> factor() |> as.numeric())*100) |> set_names(as.character(levels(unique_groups)))


  maxtime <- max(d$time)

  p |>
    ptt_plot_config(title = title, subtitle = subtitle, caption = caption,
                    font_color = font_color, font_size = font_size,
                    legend_position = legend_position, legend_orientation = legend_orientation,
                    tick_color = font_color, grid_color = grid_color, margin = margin,
                    height = height,
                    zeroline = list(zeroline = zeroline, xrange = list(min = min(d$time), max = maxtime)),
                    enable_rangeslider = list(rangeslider = rangeslider, max = maxtime))
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
#' @param isolate_primary Separates the first factor in parent plot grouping by line width.
#' @param hovertext A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)")".
#' @return plotly object
#' @examples
#' e <- readxl::read_excel("ptt_ennusteet_KT.xlsx") |> dplyr::filter(stringr::str_detect(filter, "B1GMH|P3KS14"))
#' p <- p |> ptt_plot_add_prediction_traces(e)
#' p
#' @importFrom rlang enquo as_name
#' @importFrom tidyr uncount pivot_longer
#' @importFrom dplyr slice_tail
#' @importFrom plotly plot_ly add_lines
#' @export

ptt_plot_add_prediction_traces <- function(p,
                                           pred_data,
                                           grouping = sarja_nmi,
                                           n_obs = 2,
                                           with_labs = T,
                                           isolate_primary = F,
                                           showlegend = F,
                                           hovertext = list(rounding = 1, unit = "%", extra = "(ennuste)")) {
  grouping <- enquo(grouping)
  pred_series <- pred_data |>
    filter(!!grouping %in% names(p$color_vector)) |>
    mutate(across(matches("[0-9]{4}"), ~ as.double(.x))) |>
    pivot_longer(cols = matches("[0-9]{4}"), names_to = "year") |>
    select(year, !!grouping, value) |> group_by(!!grouping) |> slice_tail(n = n_obs) |>
    mutate(count = 2) |> uncount(count) |> group_by(year) |> mutate(time = paste0(year, c("-02-01","-11-01")) |> lubridate::as_date()) |>
    ungroup() |>
    relocate(value, .after = time)
  range.slider <- p$enable_rangeslider
  range.slider$range[[2]] <- max(range.slider$range[[2]],pred_series$time)
  zero.line <- p$add_zeroline
  zero.line$xrange$max <- max(zero.line$xrange$max,pred_series$time)
  pred_series <- pred_series |> group_by(year, !!grouping) |> group_split()
  color_vector <- p$color_vector |> farver::decode_colour() |> farver::encode_colour(alpha = 0.5)
  pred_groups <- pred_data[[as_name(grouping)]] |> unique()
  if(!all(names(color_vector) %in% pred_groups)) {
    stop("All prediction traces must have a correspondingly named trace in original plot.", call. = F)
  }
  legend.items <- c()
  for (s in pred_series) {
    s.name <- unique(s[[as_name(grouping)]])
    s.level <- p$legend_ranks[s.name]
    lw <- if(!isolate_primary) { 4 } else {ifelse(s.level == 100, 4, 2)}
    show.legend <- ifelse(!s.name %in% legend.items, showlegend, F)
    legend.items <- c(legend.items, s.name) |> unique()
    legend.rank <- s.level * 1.1 + 1
    p <- p |>
      add_lines(data = s, y = ~value, x = ~time, text = s[[as_name(grouping)]],
                type = "scatter", mode="lines",
                line = list(width = lw),
                color = I(color_vector[s.name]),
                name = ifelse(with_labs == T, str_c(s.name,", ennuste"), "Ennuste"),
                legendgroup = s.name,
                legendrank = legend.rank,
                showlegend = show.legend,
                hovertemplate = ptt_plot_hovertemplate(hovertext))
  }
  p %>%
    ptt_plot_add_rangeslider(enable = range.slider$enable, height = range.slider$size, slider_range = range.slider$range) %>%
    ptt_plot_add_zeroline(zero.line)
}


#' Add secondary traces of existing traces with reduced linewidth to an existing ptt_plot.
#'
#' Outputs a plotly object.
#'
#' @param p Plotly object created with ptt_plot to add prediction traces to.
#' @param secondary_data Tibble with secondary variables of one grouping in the parent ptt_plot object.
#' @param relates_to The name of the grouping that the secondary data relates.
#' @param grouping Tibble column used for grouping in plot, should be labeled the same as data used for parent ptt_plot.
#' @param hovertext Uses parent ptt_plot specification if undefined. A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)")".
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

ptt_plot_add_secondary_traces <-
  function(p, secondary_data, relates_to, grouping,
           hovertext, showlegend = TRUE) {

    grouping <- enquo(grouping)

    if (missing(relates_to) | missing(secondary_data)) {
      message("Define the relation to parent ptt_plot by providing both the data with the secondary data and the relates_to variable.")
      stop()
    }

    relates_to <- as_name(enquo(relates_to))

    if(!relates_to %in% names(p$color_vector)) {
      message("Provided relates_to not in parent ptt_plot variables!")
      stop()
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

    p
  }
