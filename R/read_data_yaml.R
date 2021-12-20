#' Read data yaml file, get data, and output xlxs files
#'
#' @param file Path to yaml file
#'
#' #' @export
yaml_to_excel <- function(file, start_year) {
  y <- yaml::read_yaml(file)
  for(i_file in seq_along(y)) {
    filename <- paste0(names(y[i_file]), ".xlsx")
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

  ## Aggregoi
  if (x$muunnos == "alkuperäinen") {
    d <-
      d |>
      mutate(time = lubridate::year(time)) |>
      rename(Vuosi = time)

  } else {
    fun <- switch(x$muunnos, "vuosisumma" = sum, "vuosikeskiarvo" = mean)
    freq <- switch(unlist(attr(d, "frequency"))[1],
                   "Quarterly" = 4,
                   "Monthly" = 12)

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

  ## Järjestä
  d <-
    left_join(
      expand_grid(!!!x$tiedot),
      d,
      by = intersect(names(x$tiedot), names(d)))

  ## Pivotoi
  d |>
    unite("Aikasarja", -Vuosi, -value, sep = "; ") |>
    pivot_wider(names_from = Vuosi) |>
    mutate(id = x$id, Muunnos = x$muunnos) |>
    relocate(id, Aikasarja, Muunnos)
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
  if (!is.null(file)) {
    yaml::write_yaml(y, file)
    cli_alert_success("Created {file}")
  }
  cat((yaml::as.yaml(y)))
}
