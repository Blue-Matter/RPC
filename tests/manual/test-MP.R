
# Test MP
OMs <- avail("OM", package = "RPC")

for(i in 1:length(OMs)) {
#for(i in 1:2) {
  OM_test <- get(OMs[i])
  x <- OM_test %>% SubCpars(1:3) %>% runMSE(Hist = TRUE, silent = TRUE)

  testthat::expect_s4_class(x, "Hist")

  testthat::expect_no_error(CurF_plot(x))
  testthat::expect_no_error(CurF_plot(x, 3))
  testthat::expect_no_error(CurF_plot(x, 0))

  testthat::expect_no_error(CurC_plot(x))
  testthat::expect_no_error(CurC_plot(x, 3))
  testthat::expect_no_error(CurC_plot(x, 0))

  testthat::expect_s3_class(make_FixC_MP(1), "MP")
  testthat::expect_s3_class(make_FixC_MP(0), "MP")
  testthat::expect_s3_class(myCMP <- make_FixC_MP(3), "MP")
  testthat::expect_s4_class(myCMP(x = 1, SimulatedData), "Rec")

  testthat::expect_s3_class(make_FixF_MP(1), "MP")
  testthat::expect_s3_class(make_FixF_MP(0), "MP")
  testthat::expect_s3_class(myFMP <- make_FixF_MP(3), "MP")
  testthat::expect_s4_class(myFMP(x = 1, SimulatedData), "Rec")
}

