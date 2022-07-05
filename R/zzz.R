

#' @importFrom dplyr %>%
#' @importFrom graphics abline layout legend lines matlines matplot matpoints mtext par points polygon text title
#' @importFrom stats lm median optimize predict quantile
#' @import methods utils ggplot2 grDevices shiny MSEtool SAMtool MSEextra
NULL

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "AM", "MP", "PM", "Probability", "Quantile", "R", "SSB", "Type", "Year", "b", "value", "variable", "y"))
}

