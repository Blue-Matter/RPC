#' Reference Point Calculator (RPC)
#'
#' Launches the RPC Shiny App.
#'
#' @param ... Arguments to \link[shiny]{runApp}.
#' @export
RPC <- function(...) shiny::runApp(system.file("shiny_apps/RPC", package = "RPC"), ...)
