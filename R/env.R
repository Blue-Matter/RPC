
#' @title RPC environments
#' @description Environments that the shiny app will use to store information.
RPCenv <- new.env()

#' @describeIn RPCenv Stores performance metrics.
#' @export
PMenv <- new.env() # Performance metrics

#' @describeIn RPCenv Stores descriptions of selected MPs.
#' @export
MPdesc <- new.env() # Description of MPs

#' @describeIn RPCenv Stores MP intervals.
#' @export
MPinterval <- new.env() # MP frequency

