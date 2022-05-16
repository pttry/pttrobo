.onLoad <- function(...) {
  if (nzchar(Sys.getenv("VAKKA_FAILSAFE"))) {
    vakka_failsafe()
  }
}

## Set knitr options to catch and print errors when building on cloud. Rendering of Rmd files won't stop on errors. These need to be run inside the Rmd while rmarkdown::render will overwrite them. These options are triggered when pttrobo is loaded and VAKKA_FAILSAFE environment variable is set this is done to minimize boilerplate code in the Rmds.
vakka_failsafe <- function() {
  message("Set Vakka failsafe mode")
  knitr::opts_chunk$set(error = TRUE)
  knitr::knit_hooks$set(error = function(x, options) {
    message("Error in chunk!")
    paste(c('\n\n:::{style="color:Crimson; background-color: SeaShell;"}',
            gsub('^## Error', '**Error**', x),
            ':::'), collapse = '\n')
  })
}
