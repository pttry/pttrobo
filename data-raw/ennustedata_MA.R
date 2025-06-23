

id_puunkaytto <-
  c(
    "luke/met/puukay/mt/0300_puukay.px",
    "luke/met/puukay/0100_puukay.px"
  )

id_puukauppa <-
  c(
    "luke/met/teokau/kk/0100_teokau.px",
    "luke/met/teokau/v/0400_teokau.px",
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
ma_tulli_raakapuu_cn8 <- c("44032110", "44032031", "44032190", "44032200", "44032039", "44032310", "44032011", "44032390", "44032400", "44032019", "44032510", "44032091", "44032590", "44032600", "44032099", "44039510", "44039951", "44039590", "44039600", "44039959", "44039800", "44034100", "44034200", "44034910", "44034920", "44034935", "44034940", "44034985", "44034995", "44039100", "44039110", "44039190", "44039210", "44039290", "44039300", "44039400", "44039700", "44039900", "44039910", "44039930", "44039995", "44011000", "44011100", "44011200", "44012100", "44012200", "44012210", "44012290", "44031000", "44031100", "44031200", "44013100", "44013200", "44013900", "44014010", "44014100", "44014090", "44014900")

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




