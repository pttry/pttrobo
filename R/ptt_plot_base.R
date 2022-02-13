ptt_plot <- function(d,
                       grouping_variable,
                       title,subtitle = "",
                       caption,
                       legend_orientation = "auto",
                       legend_position = "bottom",
                       margin = NA,
                       label_x = NA,
                       label_y = NA,
                       font_color = "#696969",
                       grid_color = "#E8E8E8",
                       rangeslider = T,
                       font_size = 14,
                       hovertext = list(rounding = 1, unit = "", extra = ""),
                       height = 400,
                       ...
){
  
  if(missing(grouping_variable)){
    stop("Grouping variable for the data needed (without quotes)\n",
         "For example: plot_lines(time_series_data, grouping_variable=Alue)")
  }
  
  d <- droplevels(d)
  
  grouping_variable <- enquo(grouping_variable)
  
  unique_groups <- d[[quo_name(grouping_variable)]] %>% unique() %>% sort()

    color_vector <- (function() {
    color_vector <- ptt_plot_set_colors(length(unique_groups))
    color_vector %>% setNames(unique_groups)
  })()
  
  xaxis_offset <- scales::extended_breaks(5)((d$value)) %>% str_length() %>% max()
  
  p <- plotly::plot_ly(d, x = ~ time, height = height)
  
  split_d <- group_split(d, !!grouping_variable)
  
  for (g in split_d) {
    g.name <- unique(g[[quo_name(grouping_variable)]])
    # legend.group <- which(g.name == unique_groups)
    p <- 
      p %>%  
      add_trace(data=g, y = ~value, text = g[[quo_name(grouping_variable)]],
                hovertemplate = ptt_plot_hovertemplate(hovertext),
                line = list(width = 4),
                legendgroup = g.name,
                name = g.name,
                color = I(color_vector[g.name]), type = "scatter", mode ='lines'
      )
  }
  p |>
    ptt_plotly_config(title = title, subtitle = subtitle, caption = caption, 
                             label_x = label_x, label_y = label_y,
                             font_color = font_color, font_size = font_size,
                             legend_position = legend_position, legend_orientation = legend_orientation,
                             tick_color = font_color, grid_color = grid_color, margin = margin, 
                             height = height, xaxis_offset = xaxis_offset,
                             enable_rangeslider = rangeslider)
}

add_prediction_traces <- function(p, pred_data, grouping = sarja_nmi, n_obs = 2, with_labs = T, showlegend = T, hovertext = list(rounding = 1, unit = "%", extra = "(ennuste)")) {
  grouping <- enquo(grouping)
  pred_series <- pred_data %>%  
    pivot_longer(cols = matches("[0-9]{4}"), names_to = "year") %>% 
    select(year, !!grouping, value) %>% group_by(!!grouping) %>% slice_tail(n = n_obs) %>% 
    mutate(count = 2) %>% uncount(count) %>% group_by(year) %>% mutate(time = paste0(year, c("-02-01","-11-01")) %>% lubridate::as_date()) %>% 
    ungroup() %>% 
    relocate(value, .after = time) %>% 
    group_by(year, !!grouping) %>% 
    group_split()
  color_vector <- (function() {
    color_vector <- ptt_plot_set_colors(nrow(pred_data), T)
    unique_groups <- pred_data[[quo_name(grouping)]] %>% unique() %>% sort()
    color_vector %>% setNames(unique_groups)
  })()
  legend.items <- c()
  for (s in pred_series) {
    s.name <- unique(s[[quo_name(grouping)]])
    show.legend <- ifelse(!s.name %in% legend.items, showlegend, F)
    legend.items <- c(legend.items, s.name) %>% unique()
    p <- p %>%
      add_lines(data = s, y = ~value, x = ~time, text = s[[quo_name(grouping)]],
                type = "scatter", mode="lines",
                line = list(width = 4),color = I(color_vector[s.name]),
                name = str_c("Ennuste ",ifelse(with_labs == T, s.name, "")),
                legendgroup = s.name,
                showlegend = show.legend,
                hovertemplate = ptt_plot_hovertemplate(hovertext))
  }
  p
}

d <- ptt_data_robo_l("StatFin/kan/ntp/statfin_ntp_pxt_132h.px") %>%
  filter(str_detect(taloustoimi, "B1GMH|P3KS14_S15"), tiedot == "Kausitasoitettu ja työpäiväkorjattu sarja, viitevuosi 2015, miljoonaa euroa") %>%
  group_by(taloustoimi) %>%
  mutate(value = ((value/ lag(value, 4)) -1) * 100) %>%
  ungroup()
e <- readxl::read_excel("ptt_ennusteet_KT.xlsx") |> filter(str_detect(sarja, "B1GMH|P3KS14"))

d <- d %>% filter(lubridate::year(time) >= 2010) %>%
  mutate(taloustoimi = case_when(str_detect(taloustoimi, "B1GMH") ~ "BKT", TRUE ~ "Yksityinen kulutus"))

d %>% #mutate(value = value) %>%
  ptt_plot(taloustoimi, "BKT ja kulutus", subtitle = "ennusteilla",
           # caption = NA,
           caption =  "Lähde: Tilastokeskus ja PTT",
           # label_y = "Vuosimuutos",
           height = 400,
           font_size = 14,
           # legend_position = "right",
           # margin = list(t = 0, r = 0, b = 00, l = 0, pad = 0),
           rangeslider = F, hovertext = NULL) %>%
  add_prediction_traces(e, sarja_nmi, 2, showlegend = F, with_labs = F) %>%
  # htmlwidgets::onRender(
  #   'function(el, x) {
  #   console.log(JSON.stringify(el.layout))
  #   }'
  # ) %>% 
  ptt_plot_create_widget("BKT ja kulutus")
