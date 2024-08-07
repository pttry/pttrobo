
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

Ohjeet Robonomistin kirjautumistietojen asettamiseen löytyvät PTT:n
R-pakettien yleisistä ohjeista <https://github.com/pttry/ptt> kohdasta
“Asennus ja asetukset”.

## Tietojen haku ja käyttö Robonomist-tietokannasta

Tietojen hakuun käytetään `ptt_data_ropo()` -funktiota ja robonomist
id:tä.

``` r
library(pttrobo)
#> Loaded robonomistClient 2.2.20
#> ℹ Set to connect ptt.robonomist.com
#> ✔ Set to connect ptt.robonomist.com [49ms]
```

``` r

ptt_data_robo("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px") |> head()
#> ℹ Connecting to robonomistServer at ptt.robonomist.com✔ Connecting to robonomistServer at ptt.robonomist.com [415ms]
#> ℹ Connected successfully to robonomistServer 2.9.23✔ Connected successfully to robonomistServer 2.9.23 [113ms]
#> ⠙ Requesting get⠹ Requesting get✔ Requesting get [192ms]
#> # Robonomist id: StatFin/tyti/statfin_tyti_pxt_135y.px
#> # Title:         Väestö työmarkkina-aseman mukaan muuttujina Kuukausi,
#> #   Sukupuoli, Ikäluokka ja Tiedot
#> # Last updated:  2024-07-23 08:00:00
#> # Next update:   2024-08-20 08:00:00
#> # A tibble:      6 × 5
#>   sukupuoli ikaluokka tiedot                                   time        value
#>   <fct>     <fct>     <fct>                                    <date>      <dbl>
#> 1 Yhteensä  15 - 74   Väestö, 1000 henkilöä                    2009-01-01 4015  
#> 2 Yhteensä  15 - 74   Työvoima, 1000 henkilöä                  2009-01-01 2614  
#> 3 Yhteensä  15 - 74   Työlliset yhteensä, 1000 henkilöä        2009-01-01 2427  
#> 4 Yhteensä  15 - 74   Työttömät, 1000 henkilöä                 2009-01-01  187  
#> 5 Yhteensä  15 - 74   Työvoiman ulkopuolella olevat, 1000 hen… 2009-01-01 1401  
#> 6 Yhteensä  15 - 74   Työttömyysaste, %                        2009-01-01    7.2
```

Tietojen filtteröintiin kannattaa käyttää pttdatahaku-paketin

`filter_recode()` -funktiota, josta saa mallin
`print_full_filter_recode()` -funktiolla.

``` r
library(pttdatahaku)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r

ptt_data_robo("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px") |> 
  print_full_filter_recode()
#> ⠙ Requesting get                 ℹ Object retrieved from client cache (valid until 2024-08-07 10:38:40.013072).
#> ⠙ Requesting get✔ Requesting get [68ms]
#> filter_recode(
#>   sukupuoli = c("Yhteensä", "Miehet", "Naiset"),
#>   ikaluokka = c("15 - 74", "15 - 64", "15 - 24", "20 - 64", "20 - 69", "25 - 34", "35 - 44", "45 - 54", "55 - 64"),
#>   tiedot = c("Väestö, 1000 henkilöä", "Työvoima, 1000 henkilöä", "Työlliset yhteensä, 1000 henkilöä", "Työttömät, 1000 henkilöä", "Työvoiman ulkopuolella olevat, 1000 henkilöä", "Työttömyysaste, %", "Työllisyysaste, %", "Työvoimaosuus, %")
#>   )
```

``` r

ptt_data_robo("StatFin/tym/tyti/kk/statfin_tyti_pxt_135y.px") |> 
  filter_recode(
  sukupuoli = c("Yhteensä"),
  ikaluokka = c("15 - 74"),
  tiedot = c("Työvoima" = "Työvoima, 1000 henkilöä", 
            "Työlliset" = "Työlliset, 1000 henkilöä")
  ) |> 
  filter(time >= "2021-12-01")
#> ⠙ Requesting get                 ℹ Object retrieved from client cache (valid until 2024-08-07 10:38:40.013072).
#> ⠙ Requesting get✔ Requesting get [67ms]
#> # Robonomist id: StatFin/tyti/statfin_tyti_pxt_135y.px
#> # Title:         Väestö työmarkkina-aseman mukaan muuttujina Kuukausi,
#> #   Sukupuoli, Ikäluokka ja Tiedot
#> # Last updated:  2024-07-23 08:00:00
#> # Next update:   2024-08-20 08:00:00
#> # A tibble:      31 × 5
#>    sukupuoli ikaluokka tiedot   time       value
#>    <fct>     <fct>     <fct>    <date>     <dbl>
#>  1 Yhteensä  15 - 74   Työvoima 2021-12-01  2782
#>  2 Yhteensä  15 - 74   Työvoima 2022-01-01  2739
#>  3 Yhteensä  15 - 74   Työvoima 2022-02-01  2755
#>  4 Yhteensä  15 - 74   Työvoima 2022-03-01  2765
#>  5 Yhteensä  15 - 74   Työvoima 2022-04-01  2782
#>  6 Yhteensä  15 - 74   Työvoima 2022-05-01  2867
#>  7 Yhteensä  15 - 74   Työvoima 2022-06-01  2929
#>  8 Yhteensä  15 - 74   Työvoima 2022-07-01  2878
#>  9 Yhteensä  15 - 74   Työvoima 2022-08-01  2805
#> 10 Yhteensä  15 - 74   Työvoima 2022-09-01  2802
#> # ℹ 21 more rows
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
#> ⠙ Requesting get                 ℹ Object retrieved from client cache (valid until 2024-08-07 10:38:40.013072).
#> ⠙ Requesting get✔ Requesting get [66ms]
```

``` r

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
```

``` r

data("luke/") |> 
  head()
#> ⠙ Requesting data
#> ✔ Requesting data [153ms]
#> # Robonomist Database search results
#>   id                                                                 title lang 
#>   <r_id>                                                             <chr> <chr>
#> 1 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_puutarhayritysten_r… Maat… fi   
#> 2 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_puutarhayritysten_r… Maat… fi   
#> 3 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_puutarhayritysten_r… Maat… fi   
#> 4 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_puutarhayritysten_r… Maat… fi   
#> 5 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_puutarhayritysten_r… Maat… fi   
#> 6 luke/02_Maatalous/02_Rakenne/02_Maatalous-_ja_puutarhayritysten_r… Maat… fi
```

RobonomistClient ohjeet:
<https://robonomist.github.io/robonomistClient/>

## Ennustedatojen määrittely ja päivitys

- Ennustedatat määritellää .yaml-tiedostoissa kansiossa
  [inst/ennustedata](inst/ennustedata).
- Koodit yaml-raakaversioden kirjoitukseen löytyvät:
  [data-raw/](data-raw/)
- `ptt_update_ennustedata()` päivittää ennustedatan. Esimerkiksi
  ME-datojen päivitys:
  `ptt_update_ennustedata("MEdata_", start_year = 2012)`
- `ptt_copy_ennustedata("ME")` Kopioi (ME) datat teamsiin. Data kansio
  pitää olla synkronoituna omalla koneella.
