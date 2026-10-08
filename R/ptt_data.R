#' Get Tosi data in PTT format
#'
#' Retrieves an observation table with native time columns, cleans column names,
#' and converts character columns to factors. Supplies a plotting frequency when
#' available and a canonical `tosi_id` attribute for YAML output. Notifies the
#' optional `vakka.tosi.collect` schema collector before conversion.
#'
#' @param ... Arguments to [tosi::tosi_data()], including `source_filter`.
#'   Legacy `dl_filter`, `hash` and `tidy_time` arguments are not supported.
#' @param labels Whether to retrieve labels. FALSE requests `lang = "codes"`;
#'   `col_mode` controls column names, not values. TRUE uses Tosi's language
#'   preference unless `lang` is supplied.
#'
#' @export
#' @examples
#' \dontrun{
#' ptt_data_robo("statfin/asvu/11x4.px") |> head()
#' }
ptt_data_robo <- function(..., labels = TRUE) {
  ptt_tosi_data(..., labels = labels) |> ptt_tosi_clean()
}

#' Retrieve Tosi data in PTT column conventions
#'
#' Retains canonical dimensions and schema-derived replacements, cleans column
#' names, converts character columns to factors, and supplies a plotting frequency
#' when available. Notifies the optional `vakka.tosi.collect` callback once with
#' the original schema before conversion. By default Tosi uses the session
#' language preference and identifier column names.
#'
#' @param table_id Tosi table identifier.
#' @param lang Language passed to Tosi; NULL uses its session preference.
#' @param col_mode Tosi column naming mode, defaulting to identifiers.
#' @param source_filter Source selection passed to Tosi.
#' @param aggregation Aggregation passed to Tosi.
#' @return A tibble in PTT column conventions, with a `frequency` attribute when
#'   one supported frequency is available and a canonical `tosi_id` attribute
#'   (`connector_id/object_id`) for YAML output. The identity attribute is additive
#'   metadata; it does not change columns or values.
#' @export
ptt_data_tosi <- function(table_id, lang = NULL, col_mode = "ids",
                          source_filter = NULL, aggregation = NULL) {
  ptt_tosi_data(
    table_id,
    lang = lang, col_mode = col_mode,
    source_filter = source_filter, aggregation = aggregation, format = "tbl"
  ) |> ptt_tosi_clean()
}

# Shared PTT cleaning; raw-name YAML retrievals do not pass through this step.
ptt_tosi_clean <- function(x) {
  x |>
    statfitools::clean_names() |>
    dplyr::mutate(dplyr::across(where(is.character), forcats::as_factor)) |>
    droplevels()
}

# YAML consumers retain source column names; PTT helpers clean them afterwards.
ptt_tosi_data <- function(..., labels = TRUE, lang = NULL, col_mode = "labels") {
  ptt_tosi_table(tosi::tosi_data(
    ...,
    lang = if (labels) lang else "codes", col_mode = col_mode
  ))
}

# Notify using the original schema, before typed metadata is removed.
ptt_tosi_table <- function(x) {
  schema <- attr(x, "schema")
  collect <- getOption("vakka.tosi.collect")
  if (!is.null(collect)) collect(schema)
  column_names <- schema$column_names(attr(x, "col_mode"))
  frequency <- schema$frequency
  freq_column <- if ("freq" %in% names(column_names)) column_names[["freq"]]
  if (is.null(frequency) && !is.null(freq_column) && freq_column %in% names(x)) {
    frequency <- unique(stats::na.omit(as.character(x[[freq_column]])))
  }
  plot_frequencies <- c(
    A = "Annual", Q = "Quarterly", M = "Monthly", W = "Weekly", D = "Daily"
  )
  x <- tibble::as_tibble(x, drop_replaced = TRUE)
  if (length(frequency) == 1L && as.character(frequency) %in% names(plot_frequencies)) {
    attr(x, "frequency") <- unname(plot_frequencies[as.character(frequency)])
  }
  attr(x, "tosi_id") <- paste(schema$connector_id, schema$object_id, sep = "/")
  x
}


#' @describeIn ptt_data_robo With labels TRUE.
#' @export
#'
ptt_data_robo_l <- function(..., labels = TRUE) {
  ptt_data_robo(..., labels = labels)
}


#' @describeIn ptt_data_robo Ordinary retrieval alias; there is no legacy cache to bypass.
#' @export
#'
ptt_data_robo_h <- function(...) {
  ptt_data_robo(...)
}

#' @describeIn ptt_data_robo With labels FALSE.
#' @export
#'
ptt_data_robo_c <- function(..., labels = FALSE) {
  ptt_data_robo(..., labels = labels)
}

#' @describeIn ptt_data_robo Both labels and codes.
#' @export
#'
ptt_data_robo_b <- function(...) {
  y <- bind_cols(
    ptt_data_robo_l(...),
    rename_with(ptt_data_robo_c(...), ~ paste0(.x, "_code"))
  ) |>
    select(-value_code) |>
    relocate(value, .after = last_col())

  y$time_code <- NULL
  y
}

utils::globalVariables("where")
