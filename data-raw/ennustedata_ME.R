# Maa- ja elintarviketalouden ennusteen datojen tuonti

data_get("eurostat/aact_eaa01") |>
  filter(indic_ag == "Production value at basic price",
         geo == "Finland",
         unit == "Million euro") |>
  data_to_yaml(file = "data-raw/eea.yaml")

data_get("luke/02_Maatalous/06_Talous/02_Maataloustuotteiden_tuottajahinnat/07_Tuottajahinnat_Vilja_rypsi_rapsi_kk.px") |>
  filter(Hinta == "Perushinta 1)") |>
  data_to_yaml(file = "data-raw/v_hinta.yaml")


ME_yfile <- system.file("ennustedata", "ME_data.yaml", package = "pttrobo")
yaml_to_excel(file = ME_yfile, start_year = 2010)
