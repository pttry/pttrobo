
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pttrobo

<!-- badges: start -->
<!-- badges: end -->

The goal of pttrobo is to …

Install from github:

``` r
# install.packages("devtools")
devtools::install_github("pttry/pttrobo")
```

## Tietojen haku Robonomist-tietokannasta

``` r
library(pttrobo)
#> v Loaded robonomistClient 2.1.7
#> i Set to connect ptt.robonomist.comv Set to connect ptt.robonomist.com [40ms]

# ptt_data_robo("StatFin/asu/asvu/statfin_asvu_pxt_11x4.px") |> head()
```

## Ennustedatojen määrittely ja päivitys

-   Ennustedatat määritellää .yaml-tiedostoissa kansiossa
    inst/ennustedata.
-   Koodit yaml-raakaversioden kirjoitukseen löytyvät: data-raw/
-   ptt_update_ennustedata() päivittää ennustedatan. Esimerkiksi
    ME-datojen päivitys: ptt_update_ennustedata(“MEdata\_”, start_year
    = 2012)
-   ptt_copy_ennustedata(“ME”) Kopioi (ME) datat teamsiin. Data kansio
    pitää olla synkronoituna omalla koneella.
