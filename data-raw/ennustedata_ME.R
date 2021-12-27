# Maa- ja elintarviketalouden ennusteen datojen tuonti

data_get("eurostat/aact_eaa01") |>
  filter(indic_ag == "Production value at basic price",
         geo == "Finland",
         unit == "Million euro") |>
  data_to_yaml(file = "data-raw/eea.yaml", välilehti = "eaa")

data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/07_Tuottajahinnat_Vilja_rypsi_rapsi_kk.px") |>
  filter(Hinta == "Perushinta 1)") |> View()
  data_to_yaml(file = "data-raw/v_hinta.yaml", muunnos = "vuosikeskiarvo", välilehti = "vilja_tuottajahinta")

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



ME_yfile <- system.file("ennustedata", "ME_data.yaml", package = "pttrobo")
yaml_to_excel(file = ME_yfile, start_year = 2010)


yaml_to_excel(file = system.file("ennustedata", "tt.yaml", package = "pttrobo"), start_year = 1980)
ptt_update_ennustedata("Vilja", start_year = 2010)
