# Maa- ja elintarviketalouden ennusteen datojen tuonti

library(pttdatahaku)
library(robonomistClient)

data_get("eurostat/aact_eaa01") |>
  # filter(indic_ag == "Production value at basic price",
  #        geo == "Finland",
  #        unit == "Million euro") |>
  data_to_yaml()

data_get("StatFin/maa/eaa/statfin_eaa_pxt_12d7.px") |>
  data_to_yaml(välilehti = "taloustili")

data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/07_Tuottajahinnat_Vilja_rypsi_rapsi_kk.px") |>
  filter(Hinta == "Perushinta 1)") |> View()
  data_to_yaml(file = "data-raw/v_hinta.yaml", muunnos = "vuosikeskiarvo", välilehti = "vilja_tuottajahinta")

  data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/08_Tuottajahinnat_Vilja_rypsi_rapsi_v.px") |>
    filter(Hinta == "Perushinta 1)") |>
  data_to_yaml(file = "data-raw/v_hinta.yaml", muunnos = "alkuperäinen", välilehti = "vilja_tuottajahinta")

# Myös lukella: luke/02_Maatalous/04_Tuotanto/14_Satotilasto/03_Vilja-_ja_perunasato_1920-.px
data_get("StatFin/maa/satot/statfin_satot_pxt_001.px") |>
  data_to_yaml(muunnos = "alkuperäinen", välilehti = "vilja_tuotanto")

data_get("luke/02_Maatalous/04_Tuotanto/22_Kaytossa_oleva_maatalousmaa/03_Peltoala_1910_ja_1920-.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "peltoala")

data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/01_Tuottajahinnat_Maito_kk.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "maito_tuottajahinta")

data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/02_Tuottajahinnat_Maito_v.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "maito_tuottajahinta_v")

data_get("luke/02_Maatalous/04_Tuotanto/02_Maito-_ja_maitotuotetilasto/04_Vuositilastot/02_Meijerimaidon_tuotanto_v.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "maito_tuotanto_v")

data_get("luke/02_Maatalous/04_Tuotanto/02_Maito-_ja_maitotuotetilasto/02_Kuukausitilastot/02_Meijerimaidon_tuotanto_kk.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "maito_tuotanto_kk")

data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/03_Tuottajahinnat_Liha_kk.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "liha_tuottajahinnat")

data_get("StatFin/vrm/vamuu/statfin_vamuu_pxt_11ll.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "liha_kulutus")

data_get("luke/02_Maatalous/04_Tuotanto/06_Lihantuotanto/02_Kuukausitilastot/02_Lihantuotanto_teurastamoissa_kk.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "liha_kulutus")

data_get("luke/02_Maatalous/08_Muut/02_Ravintotase/01_Elintarvikkeiden_kulutus.px") |>
  data_to_yaml(muunnos = "vuosisumma", välilehti = "liha_kulutus")

data_get("StatFin/hin/ttohi/statfin_ttohi_pxt_11gv.px") |>
  data_to_yaml(muunnos = "vuosikeskiarvo", välilehti = "ostajahinta")

data_get("StatFin/hin/khi/kk/statfin_khi_pxt_11xb.px") |>
  filter(stringr::str_starts(Hyödyke, "01\\.\\d\\.\\d ")) |>
  data_to_yaml(muunnos = "alkuperäinen", välilehti = "khi_2015")
data_get("StatFin/hin/khi/kk/statfin_khi_pxt_11xb.px") |>
  filter(stringr::str_starts(Hyödyke, "01")) |>
  data_to_yaml(muunnos = "alkuperäinen", välilehti = "khi_2015")
data_get("StatFin/hin/khi/vv/statfin_khi_pxt_11xe.px") |>
  filter(stringr::str_starts(Hyödyke, "01")) |>
  data_to_yaml(muunnos = "alkuperäinen", välilehti = "khi_2010")


# ME_yfile <- system.file("ennustedata", "ME_data.yaml", package = "pttrobo")
# yaml_to_excel(file = ME_yfile, start_year = 2010)

data_get("StatFin/teo/ttvi/statfin_ttvi_pxt_111i.px") |>
  data_to_yaml(muunnos = "alkuperäinen", välilehti = "teollisuus_vol_2015")

data_get("StatFin/teo/tlv/statfin_tlv_pxt_112c.px") |>
  data_to_yaml(muunnos = "alkuperäinen", välilehti = "teollisuus_liikvaihto_2015_kk") |>
  conc()

data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/07_Tuottajahinnat_Vilja_rypsi_rapsi_kk.px") |>
  data_to_yaml()

data_get("tidy/tulli_ulkomaankauppa") |>
  data_to_yaml()

data_get("luke/02_Maatalous/06_Talous/05_Maataloustuotteiden_ulkomaankauppa/Luke_maa_Ukaup_kk.px") |>
  # filter(maa == "KAIKKI MAAT YHTEENSÄ",
  #        muuttuja == "Tilastoarvo (1000 euro)",
  #        suunta %in% c("Tuonti alkuperämaittain", "Vienti määrämaittain")) |>
  data_to_yaml()

data_get("tulli/uljas_sitc",
     dl_filter = list("Tavaraluokitus SITC2" = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
                                                 "11", "12"),
                      "Vuosi" = c("2020"),
                      "Maa" = "AA",
                      "Suunta" = c("Vienti määrämaittain", "Tuonti alkuperämaittain"),
                      "Indikaattorit" = "Tilastoarvo (euro)")) |>
  data_to_yaml()


data_get()


# yaml_to_excel(file = system.file("ennustedata", "tt.yaml", package = "pttrobo"), start_year = 1980)
ptt_update_ennustedata(pattern = "MEdata_", start_year = 2013)
ptt_copy_ennustedata("ME")

data("unctad/") |> print(n = 100)

data_get("tidy/US_CommodityPrice_M") |>
  data_to_yaml()

data_get("tidy/dg_agri") |>
  data_to_yaml()
