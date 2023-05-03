
library(RPC)
library(openMSE)

testthat::test_file("tests/manual/test-LRP-hist.R", package = "RPC")

testthat::test_file("tests/manual/test-MP.R", package = "RPC")

testthat::test_file("tests/manual/test-MSE.R", package = "RPC")
