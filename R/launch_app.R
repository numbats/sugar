
#' Shiny Unit Grade and Attendance Reviewer
#'
#'
#' Launches the shiny web application that allows students to see their grade and attendance of a unit.
#'
#'
#' @export
launch_app <- function() {
  appDir <- (here::here("app","app.R"))
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `sugar`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


