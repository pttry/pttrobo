#' Writes an html element for embedding, and optionally png files.
#'
#' @param p A plotly object.
#' @param title The filename of the html element (without file format). The function will clean the name up, or try to extract it from param p if missing.
#' @param path The path of the saved file. When knitting and .Rmd file, the a folder is created matching the file name of the currently knit document and the path is set there.
#' @param render Logical. Is the plot rendered to viewer after saving the widget (default true). Returns the plot object nonetheless.
#' @param self_contained Logical. Will the html artefact have self-contained dependencies, increasing size. Default false.
#' @param png_artefacts Optional character vector of s(mall), n(arrow), and/or w(ide) corresponding to the expected .png sizes.
#' @param png_folder A folder to save png-files.
#' @param iframe_heigh A height of the iframe.
#' @examples
#' p |> ptt_plot_create_widget()
#' @return The plotly object p.
#' @export
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish
#' @importFrom htmlwidgets saveWidget
#' @importFrom googleCloudStorageR gcs_get_global_bucket
#' @importFrom fs path
ptt_plot_create_widget <- function(p, title, filepath,
                                   render = T, self_contained = F,
                                   png_artefacts, png_folder,
                                   iframe_height = "550px") {
  tofilename <- function(str) {
    str_extract_all(str, "[a-zåäö,A-ZÅÄÖ,\\s,_,\\.,0-9]", simplify = T) |>
      str_c(collapse = "") |>
      str_squish() |>
      tolower() |>
      str_replace_all(c("ä" = "a", "å" = "o", "ö" = "o", " |\\." = "_"))
  }
  if (missing(title)) {
    title <- (p$x$layoutAttrs |> unlist())[grep("title.text", names((p$x$layoutAttrs |> unlist())))] |>
      str_extract_all("(?<=\\>)[^\\<\\>]{2,}(?=\\<)") |> unlist() |> first() |> str_c(collapse = "_") |> tofilename()
    message(str_c("Using \"",title,"\" for htmlwidget filename.."))
  } else {
    title <- tofilename(title)
  }

  filepath <- if(missing(filepath)) {
    if(isTRUE(getOption('knitr.in.progress'))) {
      cur_input <- knitr::current_input() |> str_remove("\\.Rmd$") |> str_replace_all("/","_") |> str_c("_artefacts")
      #dir.create(cur_input,showWarnings = F)
      ###
      cat(str_c('\n<iframe src="https://storage.googleapis.com/pttry/ennustekuvat/',
                cur_input,"/",title,
                '.html" width="100%" scrolling="no" marginheight="0" frameborder="0" height="',
                iframe_height, '"></iframe>\n'))
      # }

      tempdir()
    } else {
      tempdir()
      # getwd()
      }
  } else  { filepath }

  # print(path(filepath,title,ext = "html"))

  p |>
    saveWidget(path(filepath,title,ext = "html"), selfcontained = self_contained, libdir = "plot_dependencies")


  if(!missing(png_artefacts)) {
    if(missing(png_folder)) png_folder <- filepath
    p %>% ptt_plot_automate_png(png_artefacts, dl_path = png_folder)

  }

  if(render == T) {
    p
  } else { invisible(p) }
}

#' Uses a headless browser to render the png files.
#'
#' @param p A plotly object.
#' @param artefacts A character vector of s(mall), n(arrow), and/or w(ide) corresponding to the expected .png sizes.
#' @param dl_path The path where the .png files will be downloaded to. Default is current working directory.
#' @return The plotly object p.
#' @importFrom htmlwidgets onRender
#' @importFrom chromote ChromoteSession
#' @importFrom knitr combine_words
#' @importFrom stringr str_replace_all str_c
#' @importFrom lubridate now as_datetime seconds
#' @importFrom fs path
ptt_plot_automate_png <- function(p, artefacts, dl_path = getwd()) {

  if(!any(artefacts %in% c("html","s","small","w","wide","n","narrow"))) {
    stop("\"png_artefacts\" must consist of one or more of s(mall), n(arrow) or w(ide), corresponding to desired .png size(s).", call. = F)
  }
  artefacts <- as.list(str_replace_all(artefacts, c("^s(|mall)$" = "pieni", "^w(|ide)$" = "leveä", "^n(|arrow)$" = "kapea")))

  p %>% onRender(jsCode = str_c("function(gd,params,data) {
            if(data.includes('leveä')) {
              dlBtn = $(gd).find('[data-title=\"Lataa kuva (leveä)\"]')[0];
              dlBtn.click();
            };
            if(data.includes('kapea')) {
              dlBtn = $(gd).find('[data-title=\"Lataa kuva (kapea)\"]')[0];
              dlBtn.click();
            };
            if(data.includes('pieni')) {
              console.log('pieni')
              dlBtn = $(gd).find('[data-title=\"Lataa kuva (pieni)\"]')[0];
              dlBtn.click();
            };
    }"),  data = artefacts) %>%
    ptt_plot_create_widget(title = "pngdl", filepath = tempdir(), self_contained = T, render = F)


  b <- ChromoteSession$new()
  b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(dl_path))
  b$Page$navigate(str_c("file://",path(tempdir(),"pngdl.html")))
  Sys.sleep(2)
  b$close()

  invisible(file.remove(path(tempdir(),"pngdl", ext = "html")))

  recent_files <- list.files(dl_path) %>% map(~ {
    if (!is.na(file.info(.x)$ctime) && file.info(.x)$ctime %>% as_datetime(tz = "UTC") >= now(tzone = "UTC") - seconds(5)) { .x }
  }) %>%
    purrr::compact()

  recent_length <- length(recent_files)
  if(recent_length > 0) {
    message(str_c("\nThe file",ifelse(recent_length > 1, "s",""),"\n", combine_words(recent_files,sep = ",\n", and = ", and\n"),"\n",
                  ifelse(recent_length > 1, "are","is")," in ",dl_path,"."))
  }

}

#' Uploads the html elements and dependencies to cloud storage.
#'
#' The cloud storage authentication file have to in working folder or in Tiedosto/Documents folder.
#'
#' @param files_path The folder where the artefacts to be uploaded are located.
#' @param upload_path The gcs folder where the artefacts will be uploaded to.
#' @param release_time A release time in same format as \code{\link[base]{Sys.time}}.
#'        Can be set also \code{FALSE}. If past the Sys.time() of code{TRUE} prevents upload.
#'
#' @export
#' @importFrom knitr current_input
#' @importFrom stringr str_remove str_replace_all str_c str_detect
#' @importFrom dplyr case_when
#' @importFrom googleCloudStorageR gcs_metadata_object gcs_upload gcs_get_global_bucket gcs_auth gcs_global_bucket gcs_list_objects
#' @importFrom fs path
#' @importFrom purrr walk
ptt_plot_upload_widgets <- function(files_path, upload_path, overwrite = FALSE, release_time = TRUE) {

  if ((is.logical(release_time) && release_time) || (release_time  < Sys.time() && !is.logical(release_time))) stop("Upload is past the release time. Set new relese_time or set it FALSE")

  if (length(Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))) != 0){
    aut_file <- Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
  } else {
    aut_file <- Sys.glob(file.path("~" |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
  }

  tryCatch(gcs_auth(aut_file), error = function(e) {
    str <- paste0("Do you have the proper authorisation file in the directory?\n")
    stop(str, call. = F)
  })
  suppressMessages(gcs_global_bucket("pttry"))

  is_knitting <- isTRUE(getOption('knitr.in.progress'))
  is_missing_upload_path <- missing(upload_path)

  if(missing(files_path)) {
    if(is_knitting == T) {
      cur_input <- knitr::current_input()
      files_path <- tempdir()#cur_input |> str_remove("\\.Rmd$") |> str_replace_all("/","_") |> str_c("_artefacts")
    } else {
      files_path <- tempdir()
      # stop("Give the path to the files you wish to upload. Careful! This will upload every .html, .css, .map, .scss, .txt and .js file in the given path!", call. = F)
    }
  } else {
    upl_files <-  list.files(path = files_path, recursive = T, full.names = T) |> str_subset("\\.(css|js|map|scss|html|txt)$") |> str_c(collapse = ", ")
    message(str_c("Give the path to the files you wish to upload. Careful! This will upload all of ",upl_files,"!\nType \"upload\" to continue:"))
    ans <- readline(" ")
    if (ans != "upload") { stop("Canceled", call. = F) }
  }

  if (is_missing_upload_path & !is_knitting) {
    cur_input <- basename(rstudioapi::documentPath())
    # stop("Give the path to the folder in the upload bucket where you wish to upload the files to.", call. = F)
  }

  artefact_files <- list.files(files_path, recursive = T) |> str_subset("\\.(css|js|map|scss|html|txt)$")
  if(overwrite == FALSE) { message("Overwrite is set to false, set overwrite = T in ptt_plot_upload_widgets if you want to overwrite existing uploads.") }
  walk(artefact_files, function(artefact_file) {
    upload_file <- if(is_missing_upload_path) {
      prefix <- cur_input %>% str_remove("\\.Rmd$") |> str_replace_all("/","_") |> str_c("_artefacts")
      path("ennustekuvat",prefix,artefact_file)
    } else {
      path(upload_path,artefact_file)
    }
    obj.existence <- suppressMessages(gcs_list_objects(prefix = upload_file) %>% nrow() %>% as.logical())
    # print(artefact_file)
    # print(upload_file)
    if(obj.existence == TRUE & overwrite == FALSE) {
      message(str_c("The file ",upload_file, " already exists!"))
    } else {
      if(obj.existence == TRUE) {
        message(str_c("Overwriting previous upload of ",upload_file))
      } else {
        message(str_c("Uploading ",upload_file))
      }
      upload_type <- dplyr::case_when(str_detect(upload_file, "css$") ~ "text/css",
                                      str_detect(upload_file, "js$") ~ "text/javascript",
                                      str_detect(upload_file, "txt$") ~ "text/plain",
                                      str_detect(upload_file, "map$") ~ "application/json",
                                      TRUE ~ as.character(NA))
      if(is.na(upload_type)) { upload_type <- NULL}
      meta <- gcs_metadata_object(artefact_file, cacheControl = "public, max-age=600")
      meta[["name"]] <- str_replace_all(upload_file, c("\\%C3\\%B6" = "ö", "\\%C3\\%A4" = "ä", "\\%2F" = "/"))
      upload_meta <- gcs_upload(path(files_path,artefact_file), gcs_get_global_bucket(), name = upload_file, type = upload_type, object_metadata = meta, predefinedAcl="bucketLevel")
      # print(upload_meta)
    }

  })
}
