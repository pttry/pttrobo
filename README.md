
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pttrobo

<!-- badges: start -->
<!-- badges: end -->

PTT-paketti tietojen hakuun Robonomistista ja kuvioiden piirtämiseen.

Install from github:

``` r
# install.packages("devtools")
devtools::install_github("pttry/pttrobo")
```

## Tietojen haku ja käyttö Robonomist-tietokannasta

Tietojen hakuun käytetään `ptt_data_ropo()` -funktiota ja robonomist
id:tä.

``` r
library(pttrobo)
#> v Loaded robonomistClient 2.1.8
#> i Set to connect ptt.robonomist.comv Set to connect ptt.robonomist.com [46ms]

ptt_data_robo("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px") |> head()
#> i Connecting to robonomistServer at ptt.robonomist.comv Connecting to robonomistServer at ptt.robonomist.com [146ms]
#> i Connected successfully to robonomistServer 2.4.21v Connected successfully to robonomistServer 2.4.21 [38ms]
#> \ Requesting getv Requesting get [104ms]
#> # Robonomist id: StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px
#> # A tibble:      6 x 5
#> # Title:         135y -- Väestö työmarkkina-aseman, sukupuolen ja iän mukaan,
#> #   kuukausitiedot, 2009M01-2021M12
#> # Last updated:  2022-01-25 08:00:00
#> # Next update:   2022-02-22 08:00:00
#>   sukupuoli ikaluokka tiedot                                   time        value
#>   <fct>     <fct>     <fct>                                    <date>      <dbl>
#> 1 Yhteensä  15 - 74   Väestö, 1000 henkilöä                    2009-01-01 4015  
#> 2 Yhteensä  15 - 74   Työvoima, 1000 henkilöä                  2009-01-01 2614  
#> 3 Yhteensä  15 - 74   Työlliset, 1000 henkilöä                 2009-01-01 2427  
#> 4 Yhteensä  15 - 74   Työttömät, 1000 henkilöä                 2009-01-01  187  
#> 5 Yhteensä  15 - 74   Työvoiman ulkopuolella olevat, 1000 hen~ 2009-01-01 1401  
#> 6 Yhteensä  15 - 74   Työttömyysaste, %                        2009-01-01    7.2
```

Tietojen filtteröintiin kannattaa käyttää pttdatahaku-paketin

`filter_recode()` -funktiota, josta saa mallin
`print_full_filter_recode()` -funktiolla.

``` r
library(pttdatahaku)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.2.0     v stringr 1.4.0
#> v readr   2.1.2     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

ptt_data_robo("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px") |> 
  print_full_filter_recode()
#> \ Requesting get
#> v Requesting get [8ms]
#> filter_recode(
#>   sukupuoli = c("Yhteensä", "Miehet", "Naiset"),
#>   ikaluokka = c("15 - 74", "15 - 64", "15 - 24", "20 - 64", "20 - 69", "25 - 34", "35 - 44", "45 - 54", "55 - 64"),
#>   tiedot = c("Väestö, 1000 henkilöä", "Työvoima, 1000 henkilöä", "Työlliset, 1000 henkilöä", "Työttömät, 1000 henkilöä", "Työvoiman ulkopuolella olevat, 1000 henkilöä", "Työttömyysaste, %", "Työllisyysaste, %", "Työvoimaosuus, %")
#>   )

ptt_data_robo("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px") |> 
  filter_recode(
  sukupuoli = c("Yhteensä"),
  ikaluokka = c("15 - 74"),
  tiedot = c("Työvoima" = "Työvoima, 1000 henkilöä", 
            "Työlliset" = "Työlliset, 1000 henkilöä")
  ) |> 
  filter(time >= "2021-12-01")
#> \ Requesting getv Requesting get [10ms]
#> # Robonomist id: StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px
#> # A tibble:      2 x 5
#> # Title:         135y -- Väestö työmarkkina-aseman, sukupuolen ja iän mukaan,
#> #   kuukausitiedot, 2009M01-2021M12
#> # Last updated:  2022-01-25 08:00:00
#> # Next update:   2022-02-22 08:00:00
#>   sukupuoli ikaluokka tiedot    time       value
#>   <fct>     <fct>     <fct>     <date>     <dbl>
#> 1 Yhteensä  15 - 74   Työvoima  2021-12-01  2782
#> 2 Yhteensä  15 - 74   Työlliset 2021-12-01  2595
```

## Tietojen vienti muihin ohjelmiin

pttdatahaku paketin `conc()` kopioi tiedot leikepöydälle, josta ne voi
liittää vaikka excel-tauluun.

write.csv2() -kirjoittaa csv-tiedostoon esim. openxlsx-pakerin
write.xlsx excel-tiedostoon. haven-paketin write_dta - stata-tiedostoon

Tiedot voi myös levittää ennen vientiä (spread)

``` r
dat <- ptt_data_robo("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px") |> 
  filter_recode(
  sukupuoli = c("Yhteensä"),
  ikaluokka = c("15 - 74"),
  tiedot = c("Työvoima" = "Työvoima, 1000 henkilöä", 
            "Työlliset" = "Työlliset, 1000 henkilöä")
  ) |>
  spread(tiedot, value)
#> \ Requesting getv Requesting get [8ms]

# leikepöydälle
  # conc(dat)
  
 # csv-tiedostoon omaan Tiedostot kansioon
  # write.csv2(dat, "~/dat.csv")
```

Tietoja ja niiden id:tä voi etsiä Robonomist eye:sta:
<https://eye.robonomist.app/> tai R:n kautta robonomistClient paketin
avulla

``` r
library(robonomistClient)
#> 
#> Attaching package: 'robonomistClient'
#> The following object is masked from 'package:utils':
#> 
#>     data

data("luke/") |> 
  head()
#> \ Requesting data
#> v Requesting data [91ms]
#> 
#> -- Robonomist Database search results
#>   id                                              title                         
#> 1 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_~ Maatalous- ja puutarhayrityst~
#> 2 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_~ Maatalous- ja puutarhayrityst~
#> 3 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_~ Maatalous- ja puutarhayrityst~
#> 4 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_~ Maatalous- ja puutarhayrityst~
#> 5 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_~ Maatalous- ja puutarhayrityst~
#> 6 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_~ Maatilojen lukumäärä alueitta~
#> # ... with 1 more variable: lang <chr>
```

RobonomistClient ohjeet:
<https://robonomist.github.io/robonomistClient/>

## Ennustedatojen määrittely ja päivitys

-   Ennustedatat määritellää .yaml-tiedostoissa kansiossa
    inst/ennustedata.
-   Koodit yaml-raakaversioden kirjoitukseen löytyvät: data-raw/
-   ptt_update_ennustedata() päivittää ennustedatan. Esimerkiksi
    ME-datojen päivitys: ptt_update_ennustedata(“MEdata\_”, start_year
    = 2012)
-   ptt_copy_ennustedata(“ME”) Kopioi (ME) datat teamsiin. Data kansio
    pitää olla synkronoituna omalla koneella.
