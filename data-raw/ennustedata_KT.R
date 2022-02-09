
# Kansantalouden tilinpito

library(pttdatahaku)

data_get("StatFin/kan/ntp/statfin_ntp_pxt_132h.px") |>
  data_to_yaml(xlsx_tiedosto = "KTdata_QNA.xlsx")

data_get("StatFin/kan/ntp/statfin_ntp_pxt_11tj.px") |>
  data_to_yaml()

data_get("StatFin/kan/vtp/statfin_vtp_pxt_11sf.px") |>
  data_to_yaml(xlsx_tiedosto = "KTdata_NA.xlsx")

data_get("StatFin/kan/vtp/statfin_vtp_pxt_11t4.px") |>
  data_to_yaml()

data_get("StatFin/kan/vtp/statfin_vtp_pxt_11yx.px") |>
data_to_yaml()

ptt_update_ennustedata("KTdata_", start_year = 2012)
ptt_copy_ennustedata("KT")
