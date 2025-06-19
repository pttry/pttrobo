

id_puunkaytto <-
  c(
    "luke/met/puukay/mt/0300_puukay.px",
    "luke/met/puukay/0100_puukay.px"
  )

id_puukauppa <-
  c(
    "luke/met/teokau/kk/0100_teokau",
    "luke/met/teokau/v/0400_teokau.px,
    "luke/met/kanrah/0100_kanrah.px")
id_puukauppa2 <- c(
    "luke/met/teokau/kk/0100_teokau.px",
    "luke/met/teokau/v/0400_teokau.px",
    "luke/met/marhak/kk/0100_marhak.px",
    "luke/met/marhak/v/0300_marhak.px"
  )

purrr::map(id_puunkaytto, ~data_to_yaml(data_get(.x), file = "inst/ennustedata/MAdata_puunkaytto_raw.yaml", append = TRUE))
purrr::map(id_puukauppa2, ~data_to_yaml(data_get(.x), file = "inst/ennustedata/MAdata_puukaupp2_raw.yaml", append = TRUE))


ma_tulli_cn8 <- c("44071110", "44071120", "44071190", "44071210", "44071220", "44071290", "44071910", "44071920", "44123310", "44123900", "44124900")
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




