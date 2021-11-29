library(tidyverse)
library(robonomistServer)
library(lubridate)
library(vroom)


# Moi,
#
# Tässä on esimakua siitä miltä PTT:n ennustedatat tulevat näyttämään. Eli aikaulottuvuus vaan sarakkeisiin.
#
# Katsotaan näitä sitten ensi viikolla. Voitaisiin tehdä vaikka esimerkki taulukko ja sopia samalla formaatti tarkemmin.
#
# Hyvä viikonloppua!


write_ptt_data <- function(df, tk_osoite, filetype) {

  if (missing(filetype) || !filetype %in% c("csv", "xlsx")) {
    rlang::abort("Filetype must be provided from among the options: csv or xlsx.")
    }
  filename <- data(tk_osoite) %>% attributes() %>% .$title %>% .$fi %>% str_remove_all("muuttujina.{1,}$") %>%
    tolower() %>% str_squish() %>% str_replace_all(c("[^[:alnum:]]" = "_","ä" = "a", "ö" = "o","__" = "_")) %>%
    str_c(".",filetype)
  if(filetype == "csv") {
    vroom_write(df, filename)
  } else if (filetype == "xlsx") {
    openxlsx::write.xlsx(df, filename)
  } else {
    message("Filetype must be csv or xlsx!")
    }
}


# Neljännesvuositilinpito
#
# https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__kan__ntp/statfin_ntp_pxt_132h.px/
#
#   Muunnos: vuosisumma
#
# Tiedot:
#
#   Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa
#
# Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa
#
data("StatFin/kan/ntp/statfin_ntp_pxt_132h.px", tidy_time = TRUE) %>%
  filter(Tiedot %in% c("Alkuperäinen sarja käypiin hintoihin, miljoonaa euroa",
                       "Alkuperäinen sarja, viitevuosi 2015, miljoonaa euroa")
         ) %>%
  mutate(Vuosi = year(time)) %>%
  group_by(Taloustoimi, Tiedot, Vuosi) %>%
  add_tally() %>%
  filter(n == 4L) %>%
  summarize(value = sum(value)) %>%
  pivot_wider(names_from = Vuosi)

%>%
  write_ptt_data("StatFin/kan/ntp/statfin_ntp_pxt_132h.px", "xlsx")


https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__kan__ntp/statfin_ntp_pxt_11tj.px/

  Muunnos: vuosisumma (tunnit), vuosikeskiarvo (muut)

Tiedot:

  Alkuperäinen sarja
#

data("StatFin/kan/ntp/statfin_ntp_pxt_11tj.px", tidy_time = TRUE) %>%
  filter(Tiedot == "Alkuperäinen sarja") %>%
  group_by(Taloustoimi, Toimiala, Vuosi = lubridate::year(time)) %>%
  add_tally() %>%
  filter(n == 4L) %>%
  summarize(
    Muunnos = if_else(str_detect(Taloustoimi[1], "tunnit"), "Vuosisumma", "Vuosikeskiarvo"),
    value = if (Muunnos == "Vuosisumma") sum(value) else mean(value),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Vuosi)

  write_ptt_data("StatFin/kan/ntp/statfin_ntp_pxt_11tj.px","xlsx")

# Vuositilinpito
#
# https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__kan__vtp/statfin_vtp_pxt_11sf.px/
#
#   Tiedot:
#
#   Käypiin hintoihin, miljoonaa euroa
#
# Edellisen vuoden hinnoin, miljoonaa euroa
#

data("StatFin/kan/vtp/statfin_vtp_pxt_11sf.px") %>%
  filter(Tiedot %in% c("Käypiin hintoihin, miljoonaa euroa",
                       "Edellisen vuoden hinnoin, miljoonaa euroa")
         ) %>%
  pivot_wider(names_from = Vuosi) %>%
  write_ptt_data("StatFin/kan/vtp/statfin_vtp_pxt_11sf.px", "xslx")

# Työvoimatutkimus
#
# https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyti__kk/statfin_tyti_pxt_135y.px/
#
#   Muunnos: vuosikeskiarvo
#
# Sukupuoli:
#
#   Yhteensä
#
# Ikäluokka:
#
#   15-74
#
# 15-64


data("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px", tidy_time = TRUE) %>%
  filter(
    Sukupuoli == "Yhteensä",
    Ikäluokka %in% c("15 - 74","15 - 64")
  ) %>%
  mutate(Vuosi = year(time)) %>%
  group_by(Ikäluokka, Tiedot, Vuosi) %>%
  add_tally() %>%
  filter(n == 12L) %>%
  summarize(value = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = Vuosi) %>%
  write_ptt_data("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px","xlsx")
