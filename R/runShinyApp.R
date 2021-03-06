#' @export

runShinyApp <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny", package = "WILD3810"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny", example, package = "WILD3810")
  shiny::runApp(appDir, display.mode = "normal")
}
