#' @export
ennustedatat_exceliin <- function(path = getwd()) {
  datat <- c("data_ntp_vuosisumma",
             "data_ntp_tunnit",
             "data_vtp_bkt",
             "data_tyti_vuosikeskiarvot"
             )
  for (i in datat) {
    filename <- file.path(path, paste0(i, ".xlsx"))
    writexl::write_xlsx(do.call(i, list()), filename)
  }
}

#' Neljännesvuositilinpito: Bruttokansantuote ja -tulo sekä tarjonta ja kysyntä
#'
#' https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__kan__ntp/statfin_ntp_pxt_132h.px/
#'
#' Muunnos: vuosisumma
#'
#' Tiedot:
#' Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa
#' Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa
#' @export
#' @import dplyr tidyr robonomistClient
data_ntp_vuosisumma <- function() {
  d <-
    data_get("StatFin/kan/ntp/statfin_ntp_pxt_132h.px", tidy_time = TRUE) %>%
    filter(
      Tiedot %in% c("Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa",
                    "Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa")
    ) %>%
    mutate(Vuosi = lubridate::year(time)) %>%
    group_by(Taloustoimi = forcats::fct_inorder(Taloustoimi), Tiedot, Vuosi) %>%
    add_tally() %>%
    filter(n == 4L) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    pivot_wider(names_from = Vuosi) %>%
    unite("Aikasarja", Taloustoimi, Tiedot, sep = "; ")

  ## dput(d$Aikasarja)

  valitut_sarjat <- c(
    "B1GMH Bruttokansantuote markkinahintaan; Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa",
    "B1GMH Bruttokansantuote markkinahintaan; Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa",
    "P7R Tavaroiden ja palvelujen tuonti, tulona; Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa",
    "P7R Tavaroiden ja palvelujen tuonti, tulona; Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa",
    "P6K Tavaroiden ja palvelujen vienti, menona; Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa",
    "P6K Tavaroiden ja palvelujen vienti, menona; Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa",
    "KADONNUT SARJA", ## TEST Jos tilastokeskus muuttaa sarjaa, tuloste rikkoutuu oikealla tavalla.
    "P51CK Kiinteän pääoman kuluminen, tulona; Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa",
    "P51CK Kiinteän pääoman kuluminen, tulona; Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa",
    "D1K Palkansaajakorvaukset, menona; Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa",
    "D1K Palkansaajakorvaukset, menona; Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa"
  )

  left_join(tibble(Aikasarja = valitut_sarjat), d, by = "Aikasarja")
}

#' Neljännesvuositilinpito: Työllisyys ja työtunnit
#'
#' https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__kan__ntp/statfin_ntp_pxt_11tj.px/
#'
#' Muunnos: vuosisumma (tunnit), vuosikeskiarvo (muut)
#'
#' Tiedot:
#' Alkuperäinen sarja
#' @export
#' @importFrom stringr str_detect
data_ntp_tunnit <- function() {
  data_get("StatFin/kan/ntp/statfin_ntp_pxt_11tj.px", tidy_time = TRUE) %>%
    filter(
      Tiedot == "Alkuperäinen sarja"
    ) %>%
    group_by(Taloustoimi, Toimiala = forcats::fct_inorder(Toimiala), Vuosi = lubridate::year(time)) %>%
    add_tally() %>%
    filter(n == 4L) %>%
    summarize(
      Muunnos = if_else(str_detect(Taloustoimi[1], "tunnit"), "Vuosisumma", "Vuosikeskiarvo"),
      value = if (Muunnos == "Vuosisumma") sum(value) else mean(value),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Vuosi)
}

#' Vuositilinpito: Bruttokansantuote ja -tulo sekä tarjonta ja kysyntä
#' https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__kan__vtp/statfin_vtp_pxt_11sf.px/
#'
#' Tiedot:
#' Käypiin hintoihin, miljoonaa euroa
#' Edellisen vuoden hinnoin, miljoonaa euroa
#' @export
data_vtp_bkt <- function() {
  data_get("StatFin/kan/vtp/statfin_vtp_pxt_11sf.px") %>%
    filter(
      Tiedot %in% c("Käypiin hintoihin, miljoonaa euroa",
                    "Edellisen vuoden hinnoin, miljoonaa euroa")
    ) %>%
    pivot_wider(names_from = Vuosi)
}

#' Työvoimatutkimus
#'
#' https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyti__kk/statfin_tyti_pxt_135y.px/
#'
#' Muunnos: vuosikeskiarvo
#'
#' Sukupuoli:
#' Yhteensä
#'
#' Ikäluokka:
#' 15-74
#' 15-64
#' @export
data_tyti_vuosikeskiarvot <- function() {
  data_get("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px", tidy_time = TRUE) %>%
    filter(
      Sukupuoli == "Yhteensä",
      Ikäluokka %in% c("15 - 74",
                       "15 - 64")
    ) %>%
    mutate(Vuosi = lubridate::year(time)) %>%
    group_by(Ikäluokka, Tiedot = forcats::fct_inorder(Tiedot), Vuosi) %>%
    add_tally() %>%
    filter(n == 12L) %>%
    summarize(value = mean(value), .groups = "drop") %>%
    pivot_wider(names_from = Vuosi)
}
