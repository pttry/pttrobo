#' Read data yaml file, get data, and output xlxs files
#'

#' @details A *data yaml file* describes a list of excel files, sheets within
#' them, and data within the sheets. There's an example file within the package
#' to demonstrate the structure of the yaml file:
#' \code{system.file("ennustedata", "testi.yaml", package = "pttrobo")}. The
#' top-level list defines file names and the 2nd level defines sheet names.
#' Under each sheet there's a list of data objects that describe what data is
#' retrieved to populate the sheets. Each data object should at least contain an
#' *id* to identify a data table in Robonomist Database. Additionally the data
#' object can contain the following items: * **muunnos** This can take value
#' "alkuperäinen", "vuosikeskiarvo", or "vuosisumma" * **ajanjakso** This can be
#' set to "satovuosi" to aggregate annualy to intervals from July to June. *
#' **tiedot** This shoud be a named list of variable names and values to be used
#' as a filter for the data. The order of the named list is also used to arrage
#' the data on to the excel sheets.
#'
#' @param file Path to yaml file
#' @param xlsx_path A path to save excel files.
#' @param start_year A numeric. Year to start data
#' @param transpose A logical to also transpose data for excel.
#' @examples
#' \dontrun{
#' esimerkkitiedosto <- system.file("ennustedata", "testi.yaml",
#'                                  package = "pttrobo")
#' yaml_to_excel(esimerkkitiedosto, start_year = "2011")
#' }
#' @export
yaml_to_excel <- function(file,
                          xlsx_path = system.file("ennustedata_xlsx",
                                                  package = "pttrobo"),
                          start_year,
                          transpose = FALSE) {

  y <- yaml::read_yaml(file)
  for(i_file in seq_along(y)) {
    filename <- file.path(xlsx_path, paste0(names(y[i_file]), ".xlsx"))
    d <- koosta_tiedoston_datat(y[[i_file]], start_year = start_year)
    openxlsx::write.xlsx(d, filename, overwrite = TRUE, keepNA = TRUE)
    if (transpose) {
      filename <- c(filename, gsub("\\.xlsx","_trans.xlsx", filename))
      d_t <- purrr::map(d, ~tibble::rownames_to_column(as.data.frame(t(tibble::column_to_rownames(select(.x, -id, -Muunnos), "Aikasarja"))), "time"))
      openxlsx::write.xlsx(d_t, filename[2], overwrite = TRUE, keepNA = FALSE)
      }

    cli_alert_success("Wrote {filename}")
  }
  invisible(filename)
}

koosta_tiedoston_datat <- function(x, start_year) {
  purrr::imap(x, ~{ purrr::map(.x, muodosta_sarjat, name = .y, start_year = start_year) |>
      dplyr::bind_rows() })
}


#' @importFrom rlang %||%
#' @importFrom robonomistClient data_get
#' @import dplyr
muodosta_sarjat <- function(x, name = NULL, start_year) {


  ## Hae ja suodata
  message(name)

  ## Hae ja suodata
  d <-
    if (stringr::str_starts(x$id, "tulli/")|stringr::str_starts(x$id, "ecb/")) {
     data_get(x$id, dl_filter = x$tiedot, tidy_time = TRUE) |>
        tidyr::replace_na(list(value = 0))

    } else if (!is.null(x$tiedot)) {
      data_get(x$id, tidy_time = TRUE) |>
        filter(
          !!!unname(purrr::imap(x$tiedot, ~expr(!!sym(.y) %in% !!.x)))
        )
    } else {
      data_get(x$id, tidy_time = TRUE)
    }

  if(stringr::str_starts(x$id, "tulli/")) {
    x$tiedot$Indikaattorit <- unique(pull(d, Indikaattorit))
    }


  if (!is.null(x$poista_muut_tiedot)&& x$poista_muut_tiedot){
    d <- select(d, names(x$tiedot), time, value)
  }

  d <- filter(d, lubridate::year(time) >= start_year)


  x$muunnos <- x$muunnos %||% "alkuperäinen"

  frequ <- unlist(attr(d, "frequency"))
  if (is.null(frequ)) frequ <- x$frequency

  freq <- switch(frequ[1],
                 "Annual" = 1,
                 "Quarterly" = 4,
                 "Monthly" = 12)

  ## Aggregoi
  if (x$muunnos == "alkuperäinen" && freq == 1) {

    d <-
      mutate(d, time = lubridate::year(time)) |>
      drop_na() |>
      rename(Vuosi = time)

  } else if (x$muunnos == "alkuperäinen" && freq != 1) {
    ## Aika-akseli "Vuosi" jätetään tyyppiin Date
    d <-
      rename(d, Vuosi = time)

  } else {
    fun <- switch(x$muunnos,
                  "vuosisumma" = sum,
                  "vuosikeskiarvo" = mean,
                  "viimeinen" = dplyr::last)


    if (!is.null(x$ajanjakso) && x$ajanjakso == "satovuosi") {
      d <-
        d |>
        drop_na() |>
        mutate(Vuosi = lubridate::year(time - months(6)),
               Ajanjakso = "Satovuosi") |>
        group_by(across(c(-time, -value))) |>
        add_tally() %>%
        #filter(n == freq) %>%
        summarize(value = fun(value), .groups = "drop")

    } else {

      d <-d |>
        mutate(Vuosi = lubridate::year(time)) |>
        tidyr::drop_na(value) |>
        group_by(across(c(-time, -value))) |>
        add_tally() %>%
        filter(n == freq) %>%
        summarize(value = fun(value), .groups = "drop")
    }
  }

  ## Määritä taulukon järjestys
  # Muuttujat <- setdiff(names(d), "value")
  Muuttujat <- c(names(x$tiedot), intersect(names(d), c("Vuosi", "time")))

  jarjestys <-
    purrr::map(Muuttujat, ~{
      if (.x == "Vuosi") {
        if (lubridate::is.Date(d$Vuosi)) {
          seq(lubridate::make_date(start_year,1,1), max(lubridate::make_date(start_year + 11,1,1), max(d$Vuosi)),
              by = dplyr::case_when(freq == 4 ~ "quarter",
                                    freq == 12 ~ "months"))
        } else {
          as.double(seq(as.numeric(start_year), max(start_year + 10 ,max(d$Vuosi))))
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
      tidyr::expand_grid(!!!jarjestys),
      d,
      by = intersect(c(names(x$tiedot), "Vuosi"), names(d))
    )


  ## Pivotoi
  d |>
    tidyr::unite("Aikasarja", -Vuosi, -value, sep = "; ") |>
    tidyr::pivot_wider(names_from = Vuosi) |>
    mutate(id = x$id, Muunnos = x$muunnos) |>
    relocate(id, Muunnos, Aikasarja)


}

#' @export
data_to_yaml <- function(d, file = NULL, xlsx_tiedosto = "file1",
                         sheet = "sheet1",
                         muunnos = c("alkuperäinen", "vuosisumma", "vuosikeskiarvo"),
                         append = FALSE) {
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
      ), sheet)
    ), xlsx_tiedosto)

  handlers <- list(
    character = function(x) enc2utf8(x),
    list = function(x) {
      if (!is.null(names(x))) names(x) <- enc2utf8(names(x))
      x
    }
  )

  if (!is.null(file)) {
    con <- file(file, open = if (append) "a" else "w", encoding = "UTF-8")
    yaml::write_yaml(y, con, handlers = handlers)
    close(con)
    cli_alert_success("Write in {file}")
  }
  out <- yaml::as.yaml(y, handlers = handlers)
  cat(out)
  cat(out, file = "clipboard-128")
}
