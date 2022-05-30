#' Reference Point Calculator (RPC)
#'
#' Launches the RPC Shiny App.
#'
#' @param ... Arguments to \link[shiny]{runApp}.
#' @details \code{RPC} opens up the App in the user browser.
#'
#' @examples
#' RPC(launch.browser = TRUE)
#'
#' @export
RPC <- function(...) shiny::runApp(system.file("shiny_apps/RPC", package = "RPC"), ...)

#' @rdname RPC
#' @param filename Name of file of saved RPC session
#' @details \code{load_session} returns a named list of openMSE objects saved in an RPC session. Objects include:
#' \itemize{
#' \item \code{\linkS4class{OM}} the original operating model uploaded to the App
#' \item \code{\linkS4class{Hist}} containing the historical reconstruction of the stock
#' \item \code{\linkS4class{MSE}} containing the projection results from closed-loop simulation of management procedures
#' \item \code{MPs} A list of management procedure functions that return catch advice from data.
#' \item \code{PMs} A list of performance measure functions that evaluates management procedures in the MSE object.
#' }
#' These objects can be used to re-create figures, obtain LRP estimates, etc. in the R console,
#' for example, see \link{plot_hist} and \link{plot_LRP}.
#' @export
load_session <- function(filename) {
  prev_session <- readRDS(file = filename)
  stopifnot(inherits(prev_session, "list"))

  # OM, Hist, MSE
  out <- prev_session[c("OM", "MSEhist", "MSEproj")] %>% structure(names = c("OM", "Hist", "MSE"))

  # MP_list
  MPs_to_add <- ls(envir = prev_session$MPs_env)
  if(length(MPs_to_add)) {
    out$MPs <- lapply(MPs_to_add, get, envir = prev_session$MPs_env)
  } else {
    out$MPs <- list()
  }

  # Performance measures
  PM <- ls(envir = prev_session$PMenv)
  if(length(PM)) {
    out$PMs <- lapply(PM, function(x) prev_session$PMenv[[x]])
  } else {
    out$PMs <- list()
  }

  return(out)
}
