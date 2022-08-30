
# Markkinat

data_get("eia/PET.RBRTE.M") |>
  data_to_yaml()

# KV-talous

data_get("eurostat/namq_10_gdp") |>
  data_to_yaml()



# Kansantalouden tilinpito

library(pttdatahaku)

data_get("StatFin/kan/ntp/statfin_ntp_pxt_132h.px") |>
  data_to_yaml()

data_get("StatFin/kan/ntp/statfin_ntp_pxt_11tj.px") |>
  data_to_yaml()

data_get("StatFin/kan/vtp/statfin_vtp_pxt_11sf.px") |>
  data_to_yaml(xlsx_tiedosto = "KTdata_NA.xlsx")

data_get("StatFin/kan/vtp/statfin_vtp_pxt_11t4.px") |>
  data_to_yaml()

data_get("StatFin/kan/vtp/statfin_vtp_pxt_11yx.px") |>
data_to_yaml()

data_get("StatFin/kan/vtp/statfin_vtp_pxt_11t4.px") |>
  data_to_yaml()

# työvoimatutkimus

data_get("StatFin/tym/tyti/vv/statfin_tyti_pxt_13aj.px") |>
  data_to_yaml()

data_get("StatFin/vrm/vaenn/statfin_vaenn_pxt_128t.px") |>
  data_to_yaml()

data_get("StatFin/tyti/statfin_tyti_pxt_135z.px") |>
  data_to_yaml()

# Tulot

data_get("StatFin/pal/ati/nj/statfin_ati_pxt_13dy.px") |>
  data_to_yaml()

data_get("etk/200indeksiluvut/indeksit01.px") |>
  data_to_yaml()

# Hinnat

data_get("StatFin/hin/khi/kk/statfin_khi_pxt_11xq.px") |>
  data_to_yaml()

data_get("eurostat/prc_hicp_midx") |>
  data_to_yaml()

data_get("StatFin/khi/statfin_khi_pxt_11xx.px") |>
  data_to_yaml()

# Julkinen
data_get("StatFin/jul/jyev/statfin_jyev_pxt_12sy.px") |>
  data_to_yaml()
data_get("StatFin/jul/jyev/statfin_jyev_pxt_11yv.px") |>
  data_to_yaml()

ptt_update_ennustedata("KTdata_Tu", start_year = 2013)
ptt_copy_ennustedata("KT")

