#' Run the tissuePPT Shiny web application locally.
#' @export
tissuePPT_app <- function() {
  shiny::runApp(system.file('tissuePPTapp', package='tissuePPT'),
                host=getOption("0.0.0.0"), port =getOption("8989"))
}
