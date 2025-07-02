#' Lancer l'application Shiny
#'
#' Cette fonction lance l'application Shiny.
#'
#' @export
shiny_app <- function() {
   appDir <- system.file("shiny_app", package = "xyIphone")
   if (appDir == "") {
      stop("Impossible de trouver l'application shiny. Veuillez réinstaller le package.", call. = FALSE)
   }
   shiny::runApp(appDir, display.mode = "normal")
}
