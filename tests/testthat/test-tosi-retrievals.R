tosi_fixture <- function(object_id = "khi_11xl_px", frequency = "M") {
  schema <- tosi::TosiSchema$new(
    "statfin", object_id, as.POSIXct("2026-10-06", tz = "UTC"),
    components = list(
      tosi::TosiSchemaDimension$new("vuosi", label = "Vuosi"),
      tosi::TosiSchemaTime$new(replaces_id = "vuosi"),
      tosi::TosiSchemaDimension$new("tiedot", label = "Tiedot"),
      tosi::TosiSchemaDimension$new("indeksisarja", label = "Indeksisarja"),
      tosi::TosiSchemaDimension$new("tuotteet_toimialoittain_cpa_2015_mig"),
      tosi::TosiSchemaValue$new()
    ), frequency = tosi::frequency_code(frequency), lang = "fi"
  )
  tosi::new_tosi_table(tibble::tibble(
    Vuosi = "2015", time = as.Date(c("2015-01-01", if (frequency == "A") "2016-01-01" else "2015-02-01")),
    Tiedot = if (object_id == "thi_118g_px") "Pisteluku (2015=100)" else "Pisteluku",
    Indeksisarja = "Tuontihintaindeksi",
    tuotteet_toimialoittain_cpa_2015_mig = "Liha", value = c(100, 200)
  ), schema, col_mode = "labels")
}

test_that("Tosi wrapper preserves identifier defaults and forwards selection", {
  x <- tosi_fixture()
  names(x) <- unname(attr(x, "schema")$column_names("ids"))
  attr(x, "col_mode") <- "ids"
  calls <- list()
  schemas <- list()
  withr::local_options(vakka.tosi.collect = function(schema) {
    schemas[[length(schemas) + 1L]] <<- schema
  })
  local_mocked_bindings(tosi_data = function(...) {
    calls[[length(calls) + 1L]] <<- list(...)
    x
  }, .package = "tosi")
  expect_identical(as.list(formals(ptt_data_tosi)), alist(
    table_id = , lang = NULL, col_mode = "ids",
    source_filter = NULL, aggregation = NULL
  ))
  out <- ptt_data_tosi("statfin/khi/11xl.px")
  expect_identical(calls[[1]], list(
    "statfin/khi/11xl.px",
    source_filter = NULL, aggregation = NULL,
    format = "tbl", lang = NULL, col_mode = "ids"
  ))
  expect_named(out, c(
    "time", "tiedot", "indeksisarja",
    "tuotteet_toimialoittain_cpa_2015_mig", "value"
  ))
  expect_identical(out$time, as.Date(c("2015-01-01", "2015-02-01")))
  expect_identical(out$tiedot, factor(rep("Pisteluku", 2)))
  expect_identical(out$value, c(100, 200))
  expect_identical(attr(out, "frequency"), "Monthly")
  expect_identical(attr(out, "tosi_id"), "statfin/khi_11xl_px")
  expect_false(inherits(out, "tosi_table"))
  expect_length(schemas, 1L)
  expect_identical(schemas[[1]], attr(x, "schema"))

  x <- tosi_fixture()
  ptt_data_tosi("table",
    lang = "codes", col_mode = "labels",
    source_filter = list(tiedot = "x"), aggregation = "sum"
  )
  expect_identical(calls[[2]], list(
    "table",
    source_filter = list(tiedot = "x"), aggregation = "sum",
    format = "tbl", lang = "codes", col_mode = "labels"
  ))
  expect_length(schemas, 2L)
  legacy <- ptt_data_robo_l("table")
  expect_null(calls[[3]]$lang)
  expect_identical(calls[[3]]$col_mode, "labels")
  expect_identical(legacy, ptt_data_tosi("table", col_mode = "labels"))
})

test_that("Tosi wrapper derives plotting frequency from typed frequency columns", {
  schema <- tosi::TosiSchema$new(
    "statfin", "frequency_fixture", as.POSIXct("2026-10-07", tz = "UTC"),
    components = list(
      tosi::TosiSchemaTime$new(), tosi::TosiSchemaFrequency$new(),
      tosi::TosiSchemaDimension$new("group"), tosi::TosiSchemaValue$new()
    ), lang = "fi"
  )
  x <- tosi::new_tosi_table(tibble::tibble(
    time = as.Date(c("2015-01-01", "2015-02-01")),
    freq = c("M", "M"), group = factor(c("A", "A"), levels = c("A", "B")),
    value = c(100, 200)
  ), schema, col_mode = "ids")
  local_mocked_bindings(tosi_data = function(...) x, .package = "tosi")
  out <- ptt_data_tosi("table")
  expect_identical(attr(out, "frequency"), "Monthly")
  expect_identical(levels(out$group), "A")
  x$freq <- c("M", "Q")
  expect_null(attr(ptt_data_tosi("table"), "frequency"))
})

test_that("typed retrieval is converted before factors and notifies the collector", {
  x <- tosi_fixture()
  calls <- list()
  schemas <- list()
  withr::local_options(vakka.tosi.collect = function(schema) {
    schemas[[length(schemas) + 1L]] <<- schema
  })
  local_mocked_bindings(tosi_data = function(
    path, source_filter = NULL,
    aggregation = NULL, lang = NULL, col_mode = "labels", format = "tbl"
  ) {
    calls[[length(calls) + 1L]] <<- list(
      path = path, source_filter = source_filter,
      lang = lang, col_mode = col_mode
    )
    x
  }, .package = "tosi")
  out <- ptt_data_robo("statfin/khi/11xl.px", source_filter = list(tiedot = "x"))
  expect_s3_class(out, "tbl_df")
  expect_false(inherits(out, "tosi_table"))
  expect_false("vuosi" %in% names(out))
  expect_true(is.factor(out$tiedot))
  expect_identical(attr(out, "frequency"), "Monthly")
  expect_identical(attr(out, "tosi_id"), "statfin/khi_11xl_px")
  expect_identical(schemas[[1]], attr(x, "schema"))
  expect_identical(calls[[1]]$source_filter, list(tiedot = "x"))
  ptt_data_robo_c("statfin/khi/11xl.px", col_mode = "ids")
  expect_identical(calls[[2]]$lang, "codes")
  expect_identical(calls[[2]]$col_mode, "ids")
  ptt_data_robo_h("statfin/khi/11xl.px")
  expect_false("hash" %in% names(calls[[3]]))
  expect_error(ptt_data_robo("x", hash = "old"), "unused argument")
  both <- ptt_data_robo_b("statfin/khi/11xl.px")
  expect_true(all(c("tiedot", "tiedot_code", "value") %in% names(both)))
})

test_that("hidden deflators retrieve native paths and capture both schemas", {
  paths <- character()
  schemas <- list()
  withr::local_options(vakka.tosi.collect = function(schema) {
    schemas[[length(schemas) + 1L]] <<- schema
  })
  local_mocked_bindings(tosi_data = function(path, ...) {
    paths <<- c(paths, path)
    tosi_fixture(if (grepl("118g", path)) "thi_118g_px" else "khi_11xl_px")
  }, .package = "tosi")
  time <- as.Date(c("2015-01-01", "2015-02-01"))
  expect_equal(deflate(c(100, 200), time), c(1, 1))
  expect_equal(deflate(c(100, 200), time,
    deflator = "thi",
    index = "Tuontihintaindeksi", class = "Liha"
  ), c(1, 1))
  expect_identical(paths, c("statfin/khi/11xl.px", "statfin/thi/118g.px"))
  expect_identical(
    vapply(schemas, function(s) s$object_id, character(1)),
    c("khi_11xl_px", "thi_118g_px")
  )
})

test_that("YAML retrieval branches retain raw names and native filters", {
  calls <- list()
  local_mocked_bindings(tosi_data = function(...) {
    calls[[length(calls) + 1L]] <<- list(...)
    tosi_fixture(frequency = "A")
  }, .package = "tosi")
  for (spec in list(
    list(id = "ecb/FM", tiedot = list(Tiedot = "Pisteluku")),
    list(id = "statfin/khi/11xl.px", tiedot = list(Tiedot = "Pisteluku")),
    list(id = "statfin/khi/11xl.px")
  )) {
    expect_no_error(pttrobo:::muodosta_sarjat(spec, start_year = 2015))
  }
  expect_identical(calls[[1]]$source_filter, list(Tiedot = "Pisteluku"))
  expect_null(calls[[2]]$source_filter)
  expect_null(calls[[3]]$source_filter)
  file <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(list(plot = list(sarjat = list(list(
    nimi = "Index", robonomist_data = list(
      id = "statfin/khi/11xl.px",
      tiedot = list(Tiedot = "Pisteluku")
    )
  )))), file)
  out <- yaml_to_plotly_data(file, "plot")
  expect_equal(nrow(out$datas$data_1$data), 2L)
  expect_true("Tiedot" %in% names(out$datas$data_1$data))
  expect_length(calls, 4L)
})

test_that("filter printers accept dataframes and URL code uses retrieved identity", {
  x <- tosi_fixture()
  local_mocked_bindings(tosi_url = function(...) x, .package = "tosi")
  expect_match(pttrobo_print_filter(data.frame(group = "A"),
    conc = FALSE,
    print = FALSE
  ), "group")
  expect_match(pttrobo_print_filter_recode(tibble::tibble(group = "A"),
    conc = FALSE, print = FALSE
  ), "group")
  expect_output(
    pttrobo_print_code("https://example.test/table", conc = FALSE),
    "statfin/khi_11xl_px"
  )
})
