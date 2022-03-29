

id_ulkomaank <- c(
  "luke/04_Metsa/04_Talous/06_Metsateollisuuden_ulkomaankauppa/02_Tuonti_ja_vienti_kuukausittain.px"
)

id_tuotanto <-
  c(

    "luke/04_Metsa/08_Muut/Metsateollisuus/10.01_Sahatavaran_ja_puulevyjen_tuotanto_1955.px",
    "luke/04_Metsa/08_Muut/Metsateollisuus/10.02_Puumassan_seka_paperin_ja_kartongin_tuota.px"
  )

id_puunkaytto <-
  c(
    "luke/04_Metsa/04_Talous/08_Metsateollisuuden_puunkaytto/02_metsateol_puunk_toimialoittain.px",
    "luke/04_Metsa/04_Talous/14_Puun_kokonaiskaytto/02_Raakapuun_kaytto_kayttotark_1860.px"
  )

id_puukauppa <-
  c(
    "luke/04_Metsa/04_Talous/02_Teollisuuspuun_kauppa/02_Kuukausitilastot/01a_Kantohinnat_kk.px",
    "luke/04_Metsa/04_Talous/02_Teollisuuspuun_kauppa/04_Vuositilastot/01b_Kantohinnat_v.px",
    "luke/04_Metsa/04_Talous/02_Teollisuuspuun_kauppa/02_Kuukausitilastot/03a_Hankintahinnat_kk.px",
    "luke/04_Metsa/04_Talous/02_Teollisuuspuun_kauppa/04_Vuositilastot/01b_Kantohinnat_v.px",
    "luke/04_Metsa/04_Talous/16_Kantorahatulot/01a_Bruttokantorahat_v_omistajaryhma_mk.px")
id_puukauppa2 <- c(
    "luke/04_Metsa/04_Talous/02_Teollisuuspuun_kauppa/02_Kuukausitilastot/07a_Puukauppamaarat_kk_pystykaupat.px",
    "luke/04_Metsa/04_Talous/02_Teollisuuspuun_kauppa/02_Kuukausitilastot/09a_Puukauppamaarat_kk_hankintakaupat.px",
    "luke/04_Metsa/02_Rakenne_ja_tuotanto/06_Puun_markkinahakkuut/02_Kuukausitilastot/01_Teollisuuspuun_hakkuut_kk.px",
    "luke/04_Metsa/02_Rakenne_ja_tuotanto/06_Puun_markkinahakkuut/04_Vuositilastot/03_Teollisuuspuun_hakkuut_v_koko_maa.px"
  )

purrr::map(id_tuotanto_ulkomaank, ~data_to_yaml(data_get(.x), file = "inst/ennustedata/MAdata_Tuotanto_raw.yaml", append = TRUE))
purrr::map(id_puunkaytto, ~data_to_yaml(data_get(.x), file = "inst/ennustedata/MAdata_puunkaytto_raw.yaml", append = TRUE))
purrr::map(id_puukauppa2, ~data_to_yaml(data_get(.x), file = "inst/ennustedata/MAdata_puukaupp2_raw.yaml", append = TRUE))

data_get("luke/04_Metsa/04_Talous/06_Metsateollisuuden_ulkomaankauppa/02_Tuonti_ja_vienti_kuukausittain.px") |>
  data_to_yaml()

ptt_update_ennustedata("MAdata_", start_year = 2013)
ptt_copy_ennustedata("MA")

# Pysyvä alkuvuosi
ptt_update_ennustedata("MAdata_", start_year = 2010, transpose = TRUE)
ptt_copy_ennustedata("MA", path = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - MAdata_jatkuva")



ma_tulli_cn8 <- c("44071110", "44071120", "44071190", "44071210", "44071220", "44071290", "44123900", "44129985", "44123300")
ma_tulli_cn4 <- c("4407", "4412")

# Tilastoarvo (euro) Paljous

dat_ma_vienti <-
  data("tulli/uljas_cn",
     dl_filter = list("Tavaraluokitus CN4" = ma_tulli_cn4,
                      "Maa" = "AA",
                      "Suunta" = "Vienti määrämaittain",
                      "Indikaattorit" = "Tilastoarvo (euro)"))

data_to_yaml(dat_ma_vienti)
