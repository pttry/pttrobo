# Maa- ja elintarviketalouden ennusteen datojen tuonti

data_get("eurostat/aact_eaa01") |>
  filter(indic_ag == "Production value at basic price",
         geo == "Finland",
         unit == "Million euro") |>
  data_to_yaml("data-raw/t2.yaml")

file <- "data-raw/t2.yaml"
yaml_to_excel(file)
