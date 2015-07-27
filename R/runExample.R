#' Run a Shiny Example of EEAR
#' 
#' Launches a Shiny app UI for loading, displaying, and discovering
#' usage patterns within a customer's consumption behavior.
#' 
#' @param example Name of an available Shiny example
#' @export
runExample <- function(example) {
    # locate all the shiny app examples that exist
    validExamples <- list.files(system.file("shiny-examples", package = "mypackage"))
    
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
    appDir <- system.file("shiny-examples", example, package = "mypackage")
    shiny::runApp(appDir, display.mode = "normal")
}