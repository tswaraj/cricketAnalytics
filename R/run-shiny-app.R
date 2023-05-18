#' @export
#' @import utils shiny
#'
run_app = function() {
  # Name of app folder inside of inst/
  # Do not include `inst/`
  app_dir = system.file("myapp", package = "cricketAnalytics")

  shiny::runApp(app_dir, display.mode = "normal")
}
