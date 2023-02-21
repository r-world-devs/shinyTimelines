#' @description
#' sihnyTimelines module
#' @importFrom magrittr %>%
#' @importFrom dplyr sym
"_PACKAGE"

app_sys <- function(...) {
  system.file(..., package = "shinyTimelines")
}

"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

.onLoad <- function(...) {
  shiny::addResourcePath('shinyTimelines', system.file("www", package = "shinyTimelines"))
}

