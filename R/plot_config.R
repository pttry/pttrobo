ptt_plot_set_grid <- function(p, grid_color) {
  p |> layout(
    xaxis = list(showgrid = TRUE, gridcolor = grid_color, size = 1.5),
    yaxis = list(showgrid = TRUE, gridcolor = grid_color, size = 1.5)
  )
}

ptt_plot_set_defaults <- function(p) {
  plotly::config(p, locale = "fi") %>%
    layout(separators = ", ") %>% 
    layout(xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE)) %>% 
    layout(hovermode = "compare")
  
}

ptt_plot_set_modebar <- function(p, dl_title, rangeslider) {
  
  data_dl_icon <- "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"
  chalkboard_icon <- "M69.53,91.55l13.23,13.23c3.2,0.09,5.77,2.72,5.77,5.95c0,3.29-2.66,5.95-5.95,5.95c-3.29,0-5.95-2.67-5.95-5.95 c0-0.36,0.03-0.72,0.09-1.07L65.21,98.16v8.46l-8.24,0v-8.23l-11.69,11.7c0.02,0.21,0.03,0.43,0.03,0.65c0,0,0,0,0,0 c0,3.29-2.66,5.95-5.96,5.95c-3.29,0-5.95-2.67-5.95-5.95c0-3.29,2.67-5.95,5.95-5.95c0.1,0,0.2,0,0.29,0.01l13.23-13.23L0,91.55 V15.71c0-0.05,0-0.09,0-0.14V7.52c0-0.87,0.72-1.57,1.61-1.57h55.36V0h8.24v5.95h56.06c0.89,0,1.61,0.71,1.61,1.57v8.05 c0,0.03,0,0.05,0,0.08v75.89H69.53L69.53,91.55z M26.89,62.71l23.26-22.74c5.76,5.76,11.46,11.46,17.28,17.21l15.41-15.6 l-7.15-7.15l20.12-0.18v20.29l-6.87-6.87c-7.16,7.25-14.29,14.48-21.47,21.67L50.03,52.12L32.99,68.81L26.89,62.71L26.89,62.71 L26.89,62.71z M113.79,21.73H8.92v60.64h104.87V21.73L113.79,21.73z"
  twitter_icon <- "M640.012 121.513c-23.528 10.524-48.875 17.516-75.343 20.634 27.118-16.24 47.858-41.977 57.756-72.615-25.347 14.988-53.516 25.985-83.363 31.866-24-25.5-58.087-41.35-95.848-41.35-72.508 0-131.21 58.736-131.21 131.198 0 10.228 1.134 20.232 3.355 29.882-109.1-5.528-205.821-57.757-270.57-137.222a131.423 131.423 0 0 0-17.764 66c0 45.497 23.102 85.738 58.347 109.207-21.508-.638-41.74-6.638-59.505-16.359v1.642c0 63.627 45.225 116.718 105.32 128.718-11.008 2.988-22.63 4.642-34.606 4.642-8.48 0-16.654-.874-24.78-2.35 16.783 52.11 65.233 90.095 122.612 91.205-44.989 35.245-101.493 56.233-163.09 56.233-10.63 0-20.988-.65-31.334-1.89 58.229 37.359 127.206 58.997 201.31 58.997 241.42 0 373.552-200.069 373.552-373.54 0-5.764-.13-11.35-.366-16.996 25.642-18.343 47.87-41.493 65.469-67.844l.059-.059z"
  png_icon <- "M7.5,0h107.9c4.1,0,7.5,3.4,7.5,7.5v70.6c0,4.1-3.4,7.5-7.5,7.5H7.5c-4.1,0-7.5-3.4-7.5-7.5V7.5 C0,3.4,3.4,0,7.5,0L7.5,0z M69.9,63.3h28.5v4H69.9V63.3L69.9,63.3z M69.9,53.1H109v4H69.9V53.1L69.9,53.1z M92.1,35h5.6 c0.3,0,0.5,0.2,0.5,0.5v11c0,0.3-0.2,0.5-0.5,0.5h-5.6c-0.3,0-0.5-0.2-0.5-0.5v-11C91.6,35.3,91.8,35,92.1,35L92.1,35L92.1,35z M70.5,28.3h5.6c0.3,0,0.5,0.2,0.5,0.5v17.8c0,0.3-0.2,0.5-0.5,0.5h-5.6c-0.3,0-0.5-0.2-0.5-0.5V28.8 C69.9,28.5,70.2,28.3,70.5,28.3L70.5,28.3L70.5,28.3L70.5,28.3z M81.3,24.5h5.6c0.3,0,0.5,0.2,0.5,0.5v21.6c0,0.3-0.2,0.5-0.5,0.5 h-5.6c-0.3,0-0.5-0.2-0.5-0.5V25C80.8,24.7,81,24.5,81.3,24.5L81.3,24.5L81.3,24.5z M39.3,48.2l17,0.3c0,6.1-3,11.7-8,15.1 L39.3,48.2L39.3,48.2L39.3,48.2z M37.6,45.3l-0.2-19.8l0-1.3l1.3,0.1h0h0c1.6,0.1,3.2,0.4,4.7,0.8c1.5,0.4,2.9,1,4.3,1.7 c6.9,3.6,11.7,10.8,12.1,19l0.1,1.3l-1.3,0l-19.7-0.6l-1.1,0L37.6,45.3L37.6,45.3L37.6,45.3z M39.8,26.7L40,44.1l17.3,0.5 c-0.7-6.8-4.9-12.7-10.7-15.8c-1.2-0.6-2.5-1.1-3.8-1.5C41.7,27.1,40.8,26.9,39.8,26.7L39.8,26.7L39.8,26.7z M35.9,47.2L45.6,64 c-3,1.7-6.3,2.6-9.7,2.6c-10.7,0-19.4-8.7-19.4-19.4c0-10.4,8.2-19,18.6-19.4L35.9,47.2L35.9,47.2L35.9,47.2z M115.6,14.1H7.2v64.4 h108.4V14.1L115.6,14.1L115.6,14.1z"
  
  if(is.na(dl_title)) {
    dl_title <- "img"
  } else {
    dl_title <- str_extract_all(dl_title, "[a-zåäö,A-ZÅÄÖ,\\s,_,\\.,0-9]", simplify = T) %>% 
      str_c(collapse = "") %>% 
      str_squish() %>% 
      tolower() %>% 
      str_replace_all(c("ä" = "a", "å" = "o", "ö" = "o", " |\\." = "_")) 
  }
  
  js_string <- function(wd, ht, ttl = dl_title, rs = rangeslider) {
  str_c('
          function(gd) { ',
          ifelse(rs, 'gd.layout.xaxis.rangeslider.visible = false
                 ',''),
          # 'alert(JSON.stringify(dd.layout.xaxis))'
          'Plotly.downloadImage(gd, {format: "png", width: ',wd,', height: ',ht,', filename: "',ttl,'_large"});
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
      click = htmlwidgets::JS("function(gd) {window.open(\"https://robonomist.com\")  }")
    )
  
  dl_conference_btn <- list(
    name = "Lataa kuva (esityskoko)",
    icon = list(
      path = chalkboard_icon,
      transform = "scale(0.12) translate(-1, 0.2)"
    ),
    click = htmlwidgets::JS(js_string(1200,800)))
  
  dl_btn <- list(
    name = "Lataa kuva",
    icon = list(
      path = png_icon,
      transform = "scale(0.14) translate(-4.5, 0.2)"
    ),
    click = htmlwidgets::JS(js_string(800,600)))
  
  dl_twitter_btn <- list(
    name = "Lataa kuva (Twitter)",
    icon = list(
      path = twitter_icon,
      transform = "scale(0.022) translate(0.2, 0.4)"
    ),
    click = htmlwidgets::JS(js_string(600,400)))
  
  data_dl_btn <- list(
    name = "Lataa tiedot",
    icon = list(
      path = data_dl_icon,
      transform = "scale(0.84) translate(-1, -1)"
    ),
    click = htmlwidgets::JS(str_c("
          function(gd) {
            var text = '';
            for(var i = 0; i < gd.data.length; i++){
              console.log(gd.data[i])
            };
          }
   ")))
  
  # function(gd) {
  #   var text = '';
  #   for(var i = 0; i < gd.data.length; i++){
  #     text += gd.layout.xaxis.title.text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
  #     text += gd.layout.yaxis.title.text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
  #   };
  #   var blob = new Blob([text], {type: 'text/plain'});
  #   var a = document.createElement('a');
  #   const object_URL = URL.createObjectURL(blob);
  #   a.href = object_URL;
  #   a.download = '",dl_title,"_data.csv';
  #   document.body.appendChild(a);
  #   a.click();
  #   URL.revokeObjectURL(object_URL);
  # }
  
  p |> plotly::config(
    displaylogo = FALSE,
    modeBarButtons = list(list(
      dl_btn,
      dl_twitter_btn,
      dl_conference_btn,
      data_dl_btn,
      "hoverClosestCartesian",
      "hoverCompareCartesian",
      "toggleSpikelines",
      robonomist_btn
    ))
  )
}

ptt_plot_set_ticks <- function(p, font) {
  p |> 
    layout(xaxis=list(tickfont = font,
                      mirror = TRUE,
                      ticks = 'outside',
                      showline = TRUE),
           yaxis=list(tickfont = font,
                      tickformat = "digit",
                      ticksuffix = " ",
                      mirror = TRUE,
                      ticks = 'outside',
                      showline = TRUE))
  
}

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


ptt_plot_add_logo <- function(p, offset, plot_height){
  
  image_file <- system.file("image", "ptt-logo.png", package = "pttrobo")
  txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  img_size <- 30 / plot_height
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
      yanchor = "top"
    )
  )
}

ptt_plot_add_rangeslider <- function(p, enable = F, height = 0.5) {
  if(enable == T) {
    height <- case_when(height > 0.5 ~ 0.5, height < 0.1 ~ 0.1, TRUE ~ height)
    p %>% 
      layout(
        xaxis = list(
          rangeslider = list(type = "date", thickness = height))) 
  } else { p }
}

ptt_plotly_config <- function(p, 
                              title, subtitle = "", label_x, label_y, 
                              caption, 
                              font_color = "#696969",
                              font_size = 14,
                              legend_position, legend_orientation,
                              tick_color, grid_color,
                              height, width = 800,
                              xaxis_offset = 0,
                              margin = NA,
                              enable_rangeslider = F) {
  
  if(missing(title) | missing(caption)) {
    stop("Title and caption must be strings, or NA for no plot title or plot caption.")
  }
  
  if(!is.list(margin)) {
    if(is.na(margin)) {
      margin <- list(t = 30, r = 20, b = 0, l = 0, pad = 0)
      margin$t <- case_when(str_length(subtitle) > 0 ~ margin$t + 50, TRUE ~ margin$t)
      margin$l <- case_when(is.na(label_y) ~ margin$l + 10, TRUE ~ margin$l)
      margin$b <- case_when(!is.na(caption) && !enable_rangeslider ~ margin$b + 60,
                            enable_rangeslider ~ margin$b + 60, 
                            TRUE ~ margin$b)
    }
  }
  

  main_font <- list(size = font_size, family = "sans-serif", color = font_color)
  title_font <- list(size = round(font_size*1.3), family = "sans-serif", color = font_color)

  rangeslider_size <- round(1-((height/1000)^(1/2)),2)
  
  ht_constants <- 4 # 2 pikseliä korkeudesta häviää aina + 1*2 plotin reunat, tick pituus ei lasketa eli 0
  tickfont_ht <- round((font_size+2)*1.4)#20#+max(0,ceiling(main_font$size-7)) #20+tickfontin korkeus mikä ylittää tämän: fontti 14 = 3 ei ole täysin johdonmukainen y-akselin yläreunan vuoksi. -Suunnilleen - +1.6 / piste yli fontti 12 
  rangeslider_ht <- ifelse(enable_rangeslider, round((height * rangeslider_size)+15-(margin$t*rangeslider_size)-(margin$b*rangeslider_size)),0)
  legend_ht <- ifelse(legend_position %in% c("auto","bottom"), font_size, 0)
  plot_ht <- height-sum(ht_constants,rangeslider_ht,tickfont_ht,legend_ht,margin$t,ifelse(enable_rangeslider, 0, margin$b-25)) 
  label_x_offset <- ifelse(is.na(label_y),0,round(tickfont_ht/2))
  plot_wd <- width-sum(margin$r,margin$l,label_x_offset, (xaxis_offset*(font_size-1)))
  
  caption_ht <- ifelse(!is.na(caption), font_size, 0)
  caption_offset <- list(x = -((36)/plot_wd), 
                         y = -((tickfont_ht+rangeslider_ht+margin$b)/plot_ht))
  legend_offset <- list(x = -((38)/plot_wd), y = -((tickfont_ht+caption_ht+font_size+rangeslider_ht+margin$b)/plot_ht))
  logo_offset <- list(x = 0, y = -((tickfont_ht+caption_ht+round(2800/plot_ht)+rangeslider_ht+margin$b)/plot_ht))

  # message(str_c("Plot height: ",plot_ht,", plot width: ",plot_wd))
  
  
  p |> 
    ptt_plot_set_defaults() |>
    ptt_plot_set_grid(grid_color) |>
    ptt_plot_set_modebar(title, enable_rangeslider) |>
    ptt_plot_set_ticks(main_font) |>
    ptt_plot_set_margin(margin) |>
    ptt_plot_add_logo(logo_offset, plot_ht) |>
    ptt_plot_add_rangeslider(enable_rangeslider, rangeslider_size) |>
    ptt_plot_set_legend(legend_position, legend_orientation, offset = legend_offset, main_font) |>
    ptt_plot_set_title(title, subtitle, title_font) |>
    ptt_plot_set_caption(caption, offset = caption_offset, main_font) |> 
    ptt_plot_set_axis_labels(label_x, label_y)
}



ptt_plot_set_legend <- function(p, position, orientation, offset, font) {
  if (is.na(position)) {
    p %>% layout(showlegend = F)
  } else if (!position %in% c("right","bottom")) {
    stop("Position must be one of \"right\", \"left\", or NA")
  } else if (!orientation %in% c("vertical","horizontal","auto")) {
    stop("Orientation must be one of \"horizontal\", \"vertical\", or \"auto\"")
  } else {
    x.pos <- ifelse(position == "right", 100, offset$x)
    y.pos <- ifelse(position == "right", 1, offset$y) #-0.05
    orientation <- case_when(orientation == "auto" ~ ifelse(position == "right", "v","h"), TRUE ~ str_extract(orientation, "^(v|h)"))
    p |> layout(
      legend = list(font = font,
                    x = x.pos, y = y.pos,
                    orientation = orientation,
                    xanchor = "left",
                    yanchor = "top")) 
  }
}


ptt_plot_set_caption <- function(p, caption, offset = list(x = 0, y = 0), font) {
  if(is.na(caption)) {
    p
  } else if (!is.character(caption)) {
    stop("Caption and caption footer must be a string, or caption = NA for no plot caption.")
  } else {
    p %>% 
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

ptt_plot_set_title <- function(p, title, subtitle, font) {
  if (is.na(title)) {
    p
  } else if (!is.character(title) || !is.character(subtitle)) {
    stop("Title and subtitle must be strings, or title = NA for no plot title.")
  } else {
    p <- p |>
      layout(
        title = list(
          text = paste0(
            "<span><b>", title, "</b>", "<br>",
            tags$sub(subtitle),"</span>"
          ),
          font = font,
          xanchor = "left", x = 0.025, xref = "container")
      )
  }
}

ptt_plot_set_axis_labels <- function(p, label_x, label_y) {
  if (any(!is.na(c(label_x, label_y)))) {
    if(!any(is.character(na.omit(c(label_x, label_y))))) {
      stop("Axis labels must be strings or NA.")
    }
  }
  p |>
    layout(
      xaxis = list(title = label_x),
      yaxis = list(title = label_y)
    ) 
}


ptt_plot_hovertemplate <- function(specs) {
  if (is.null(specs)) {
    NA
  } else if (!is.list(specs)) {
    stop("Hovertemplate must be a list with any of \"rounding\", \"unit\", and \"extra\" or NULL")
  } else if (!any(c("rounding","unit","extra") %in% names(specs))) {
    stop("Hovertemplate must be a list with any of \"rounding\", \"unit\", and \"extra\" or NULL")
  } else {
    specs_template <- list(rounding = 1, unit = "%", extra = "")
    for (spec.name in (names(specs) %>% str_subset("^(rounding|unit|extra)$"))) {
      if(spec.name == "rounding") { 
        if ((specs$rounding)%%1!=0) { stop("Hovertemplate rounding must be an integer.") } 
      } else if (!is.character(specs[[spec.name]])) {
        stop(str_c("Hovertemplate ",spec.name," must be a string."))
      }
      specs_template[[spec.name]] <- specs[[spec.name]]
    } 
    paste0("%{text}<br>%{y:.",specs_template$rounding,"f} ",specs_template$unit,ifelse(specs_template$extra == "", "", paste0("<br>",str_c(specs_template$extra, collapse = " "))),"<extra></extra>")
  }
}

ptt_plot_create_widget <- function(p,title) {
  title <- str_extract_all(title, "[a-zåäö,A-ZÅÄÖ,\\s,_,\\.,0-9]", simplify = T) %>% 
    str_c(collapse = "") %>% 
    str_squish() %>% 
    tolower() %>% 
    str_replace_all(c("ä" = "a", "å" = "o", "ö" = "o", " |\\." = "_"))
  p %>% 
    htmlwidgets::saveWidget(str_c(title,".html"), selfcontained = F, libdir = "plot_dependencies")
  
}

ptt_plot_set_colors <- function(n_unique, pred = F) {
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
  if (pred == T) {
    cols <- cols %>% farver::decode_colour() %>% farver::encode_colour(alpha = 0.5)
  }
  cols# %>% alter_color_for_accessibility()
}
