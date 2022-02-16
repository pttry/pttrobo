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
    for(name in names(d_specs$tiedot)){
      d <- d |>
        filter(d[name] == d_specs$tiedot[name][[1]])
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
#' @import dplyr
yearly_change <- function(data){
  data %>%
    mutate(value = ((value/ lag(value, 4)) -1) * 100) %>%
    tidyr::drop_na()
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

# piirtaja <- ptt_plot()
