

id_ulkomaank <- c(
  "luke/04_Metsa/04_Talous/06_Metsateollisuuden_ulkomaankauppa/02_Tuonti_ja_vienti_kuukausittain.px"
)

id_puunkaytto <-
  c(
    "luke/04_Metsa/04_Talous/07_Puun_kaytto/08_Metsateollisuuden_puunkaytto/02_metsateol_puunk_toimiala.px",
    "luke/04_Metsa/04_Talous/07_Puun_kaytto/00_Puun_kok_kaytto.px"
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

ma_tulli_cn8 <- c("44071110", "44071120", "44071190", "44071210", "44071220", "44071290", "44123900", "44129985", "44123300")
ma_tulli_cn4 <- c("4407", "4412")
ma_tulli_cn2 <- c("47")
ma_tulli_cn6 <- c("480411", "480419", "480442", "480449", "480451", "480452", "480459", "480511", "480512", "480519", "480524", "480525", "480550", "480592", "480593", "481032", "481039", "481092", "481151", "481159", "480100", "480210", "480220", "480230", "480240", "480254", "480255", "480256", "480257", "480258", "480261", "480262", "480269", "480300", "480421", "480429", "480431", "480439", "480441", "480530", "480540", "480591", "480610", "480620", "480630", "480640", "480820", "480830", "480890", "480910", "480920", "480990", "481013", "481014", "481019", "481022", "481029", "481031", "481099")

# Tilastoarvo (euro) Paljous

dat_ma_vienti <-
  data_get("tulli/uljas_cn",
           dl_filter = list("Tavaraluokitus CN8" = ma_tulli_cn8,
                            "Maa" = "AA",
                            "Suunta" = "Vienti määrämaittain",
                            "Indikaattorit" = "Tilastoarvo (euro)"))

data_to_yaml(dat_ma_vienti, file = "inst/ennustedata/test_raw.yaml")


## Tietojen päivitys


ptt_update_ennustedata(pattern = "MAdata_", start_year = 2013)
ptt_copy_ennustedata("MA")

# Pysyvä alkuvuosi
ptt_update_ennustedata(pattern = "MAdata_", start_year = 2010, transpose = TRUE)
ptt_copy_ennustedata("MA", path = "~/../Pellervon Taloustutkimus PTT ry/Ennuste - MAdata_jatkuva")




