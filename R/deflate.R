#' Deflate nominal series to real ones
#'
#' Gets a price index and aggregates it, if needed. Then
#' divides the series with the price index and rebase result to base year.
#'
#' Price index available:
#'
#'  * eki Elinkustannusindeksi
#'  * thi Tuottajahintaideksi
#'
#'  For thi index and class have be specified:
#'
#'  index, one of: "Teollisuuden tuottajahintaindeksi",
#'  "Teollisuuden tuottajahintaindeksi, kotimaiset tavarat",
#'  "Teollisuuden tuottajahintaindeksi, vientitavarat",
#'  "Vientihintaindeksi",
#'   "Tuontihintaindeksi",
#'    "Kotimarkkinoiden perushintaindeksi",
#'    "Kotimarkkinoiden perushintaindeksi, kotimaiset tavarat",
#'     "Kotimarkkinoiden perushintaindeksi, tuontitavarat",
#'     "Verollinen kotimarkkinoiden perushintaindeksi",
#'     "Verollinen kotimarkkinoiden perushintaindeksi, kotimaiset tavarat",
#'     "Verollinen kotimarkkinoiden perushintaindeksi, tuontitavarat"
#'
#'   class one of Tuotteet toimialoittain (CPA 2015, MIG) classification
#'
#' @param x A series to deflate
#' @param time A time vector.
#' @param deflator A name of the deflatior. "eki" or "thi".
#' @param series A name of index series for thi.
#' @param class A name of classification for thi
#' @param freq A frequency of `x`.
#' @param baseyear A base year to rebase.
#'
#' @import dplyr
#' @return A numeric vector
#' @export
#'
#' @examples
#' pttrobo::ptt_data_robo("StatFin/ati/statfin_ati_pxt_11zt.px") |>
#'   filter_recode(
#'     tiedot = c("Ansiotaso" = "Ansiotasoindeksi 1964=100")
#'   ) |>
#'     mutate(real = deflate(value, time, deflator = "eki", freq = "q", baseyear = 2015)) |>
#'     tail()
#'
#'  pttrobo::ptt_data_robo(
#'    "tulli/uljas_sitc",
#'    dl_filter = list(
#'      "Tavaraluokitus SITC2" = c("01 (2002--.) Liha ja lihatuotteet"),
#'      "Maa" = "AA",
#'      "Suunta" = c("Tuonti alkuperämaittain"),
#'      "Indikaattorit" = c("Tilastoarvo (euro)"))) |>
#'      mutate(real = deflate(value, time, deflator = "thi",
#'                      index = "Tuontihintaindeksi",
#'                      class = "10.1 Säilötty liha ja lihavalmisteet")) |>
#'    tail()

deflate <- function(x, time, deflator = "eki", index = NULL, class = NULL,
                    freq = "m",
                    baseyear = 2015) {

  series <- list(
    eki = function(index, class) {
      pttrobo::ptt_data_robo("StatFin/khi/statfin_khi_pxt_11xl.px") |>
        filter_recode(tiedot = c("Pisteluku")) |>
        select(time, p_ind = value)
    },
    thi = function(index, class) {
      pttrobo::ptt_data_robo("StatFin/thi/statfin_thi_pxt_118g.px") |>
        filter_recode(
          tuotteet_toimialoittain_cpa_2015_mig = class,
          indeksisarja = c(index),
          tiedot = c("Pisteluku (2015=100)")
        ) |>
        select(time, p_ind = value)
    }
  )

  price_dat <- series[[deflator]](index, class)

  freq_funs <- list(
    m = function(x) x,
    q = function(x){
      x |>
        mutate(time = lubridate::quarter(time, type = "date_first")) |>
        group_by(time) |>
        summarise(p_ind = mean(p_ind)) |>
        ungroup()
    },
    y = function(x){
      x |>
        mutate(time = as.Date(paste0(lubridate::year(time), "-01-01"))) |>
        group_by(time) |>
        summarise(p_ind = mean(p_ind)) |>
        ungroup()

    }
  )

  price_dat <- freq_funs[[freq]](price_dat)

  y = tibble::tibble(value = x, time = time) |>
    dplyr::left_join(price_dat, by = "time") |>
    mutate(value = value / p_ind) |>
    mutate(value =
             rebase(value, time, baseyear = baseyear,
                    basevalue = mean(value[lubridate::year(time) == baseyear])))

  y$value
}

