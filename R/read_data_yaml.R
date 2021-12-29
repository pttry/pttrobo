#' Read data yaml file, get data, and output xlxs files
#'
#' @details
#' A *data yaml file* describes a list of excel files, sheets within them, and data within the sheets. There's an example file within the package to demonstrate the structure of the yaml file: \code{system.file("ennustedata", "testi.yaml", package = "pttrobo")}. The top-level list defines file names and the 2nd level defines sheet names. Under each sheet there's a list of data objects that describe what data is retrieved to populate the sheets. Each data object should at least contain an *id* to identify a data table in Robonomist Database. Additionally the data object can contain the following items:
#' * **muunnos** This can take value "alkuperäinen", "vuosikeskiarvo", or "vuosisumma"
#' * **ajanjakso** This can be set to "satovuosi" to aggregate annualy to intervals from July to June.
#' * **tiedot** This shoud be a named list of variable names and values to be used as a filter for the data. The order of the named list is also used to arrage the data on to the excel sheets.
#'
#' @param file Path to yaml file
#' @param xlsx_path A path to save excel files.
#' @param start_year A numeric. Year to start data
#' @examples
#' \dontrun{
#' esimerkkitiedosto <- system.file("ennustedata", "testi.yaml", package = "pttrobo")
#' yaml_to_excel(esimerkkitiedosto, start_year = "2011")
#' }
#' @export
yaml_to_excel <- function(file, xlsx_path = system.file("ennustedata", package = "pttrobo"), start_year) {
  y <- yaml::read_yaml(file)
  for(i_file in seq_along(y)) {
    filename <- file.path(xlsx_path, paste0(names(y[i_file]), ".xlsx"))
    d <- koosta_tiedoston_datat(y[[i_file]], start_year = start_year)
    openxlsx::write.xlsx(d, filename, overwrite = TRUE)
    cli_alert_success("Wrote {filename}")
  }
  invisible(NULL)
}

koosta_tiedoston_datat <- function(x, start_year) {
  purrr::map(x, ~{ purrr::map(.x, muodosta_sarjat, start_year = start_year) |> bind_rows() })
}

#' @importFrom rlang %||%
muodosta_sarjat <- function(x, start_year) {
  ## Hae ja suodata

  d <-
    data_get(x$id, tidy_time = TRUE) |>
    filter(
      !!!unname(purrr::imap(x$tiedot, ~expr(!!sym(.y) %in% !!.x))),
      lubridate::year(time) >= start_year
    )

  x$muunnos <- x$muunnos %||% "alkuperäinen"

  freq <- switch(unlist(attr(d, "frequency"))[1],
                 "Annual" = 1,
                 "Quarterly" = 4,
                 "Monthly" = 12)

  ## Aggregoi
  if (x$muunnos == "alkuperäinen" && freq == 1) {

    d <-
      mutate(d, time = lubridate::year(time)) |>
      rename(Vuosi = time)

  } else if (x$muunnos == "alkuperäinen" && freq != 1) {
    ## Aika-akseli "Vuosi" jätetään tyyppiin Date
    d <-
      rename(d, Vuosi = time)

  } else {
    fun <- switch(x$muunnos, "vuosisumma" = sum, "vuosikeskiarvo" = mean)


    if (!is.null(x$ajanjakso) && x$ajanjakso == "satovuosi") {
      d <-
        d |>
        drop_na() |>
        mutate(Vuosi = lubridate::year(time - months(6)),
               Ajanjakso = "Satovuosi") |>
        group_by(across(c(-time, -value))) |>
        add_tally() %>%
        filter(n == freq) %>%
        summarize(value = fun(value), .groups = "drop")

    } else {

      d <-
        d |>
        mutate(Vuosi = lubridate::year(time)) |>
        drop_na() |>
        group_by(across(c(-time, -value))) |>
        add_tally() %>%
        filter(n == freq) %>%
        summarize(value = fun(value), .groups = "drop")

    }
  }

  ## Määritä taulukon järjestys
  Muuttujat <- setdiff(names(d), "value")
  Järjestys <-
    purrr::map(Muuttujat, ~{
      if (.x == "Vuosi") {
        if (lubridate::is.Date(d$Vuosi)) {
          seq(make_date(start_year,1,1), max(d$Vuosi),
              by = case_when(freq == 4 ~ "quarter", freq == 12 ~ "months"))
        } else {
          as.double(seq(as.numeric(start_year), max(d$Vuosi)))
        }
      } else if (is.null(x$tiedot[[.x]])){
        unique(d[[.x]])
      } else {
        x$tiedot[[.x]]
      }
    }) |>
    setNames(Muuttujat)

  ## Järjestä ja pakota kaikki vuodet taulukkoon
  d <-
    left_join(
      expand_grid(!!!Järjestys),
      d,
      by = intersect(c(names(x$tiedot), "Vuosi"), names(d))
    )

  ## Pivotoi
  d |>
    unite("Aikasarja", -Vuosi, -value, sep = "; ") |>
    pivot_wider(names_from = Vuosi) |>
    mutate(id = x$id, Muunnos = x$muunnos) |>
    relocate(id, Muunnos, Aikasarja)
}

#' @export
data_to_yaml <- function(d, file = NULL, xlsx_tiedosto = "file1", välilehti = "sheet1",
                         muunnos = c("alkuperäinen", "vuosisumma", "vuosikeskiarvo")) {
  y <-
    setNames(list(
      setNames(list(
        list(list(
          id = attr(d, "robonomist_id"),
          muunnos = match.arg(muunnos),
          tiedot = d |>
            select(!any_of(c("time", "value", "Vuosi", "Vuosineljännes", "Kuukausi"))) |>
            purrr::map(unique)
        ))
      ), välilehti)
    ), xlsx_tiedosto)

  handlers <- list(
    character = function(x) enc2utf8(x),
    list = function(x) {
      if (!is.null(names(x))) names(x) <- enc2utf8(names(x))
      x
    }
  )

  if (!is.null(file)) {
    yaml::write_yaml(y, file, handlers = handlers)
    cli_alert_success("Created {file}")
  }
  cat((yaml::as.yaml(y, handlers = handlers)))
}
