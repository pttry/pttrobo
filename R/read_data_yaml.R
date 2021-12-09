#' @export
yaml_to_excel <- function(file) {
  y <- yaml::read_yaml(file)
  for(i_file in seq_along(y)) {
    filename <- paste0(names(y[i_file]), ".xlsx")
    d <- koosta_tiedoston_datat(y[[i_file]])
    openxlsx::write.xlsx(d, filename, overwrite = TRUE)
  }
}

koosta_tiedoston_datat <- function(x) {
  purrr::map(x, ~{ purrr::map(.x, muodosta_sarjat) |> bind_rows() })
}

muodosta_sarjat <- function(x) {
  ## Hae ja suodata
  d <-
    data_get(x$id, tidy_time = TRUE) |>
    filter(
      !!!unname(purrr::imap(x$tiedot, ~expr(!!sym(.y) %in% !!.x)))
    )

  ## Aggregoi
  freq <- switch(unlist(attr(d, "frequency"))[1],
                 "Quarterly" = 4,
                 "Monthly" = 12)
  fun <- switch(x$muunnos, "vuosisumma" = sum, "vuosikeskiarvo" = mean)

  d <-
    d |>
    mutate(Vuosi = lubridate::year(time)) |>
    drop_na() |>
    group_by(across(c(-time, -value))) |>
    add_tally() %>%
    filter(n == freq) %>%
    summarize(value = fun(value), .groups = "drop")

  ## Järjestä
  d <-
    left_join(
      expand_grid(!!!x$tiedot),
      d,
      by = intersect(names(x$tiedot), names(d)))

  ## Pivotoi
  d |>
    unite("Aikasarja", -Vuosi, -value, sep = "; ") |>
    pivot_wider(names_from = Vuosi)
}


