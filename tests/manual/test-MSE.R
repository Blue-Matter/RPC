
# Test MP
OMs <- avail("OM", package = "RPC")

for(i in 1:length(OMs)) {
#for(i in 1:2) {
  OM_test <- get(OMs[i])
  MSEhist <- OM_test %>% SubCpars(1:3) %>% runMSE(Hist = TRUE, silent = TRUE)
  x <- Project(MSEhist, MPs = c("NFref", "AvC"), silent = TRUE, extended = TRUE)
  x@Hist <- MSEhist

  testthat::expect_s4_class(x, "MSE")

  testthat::expect_no_error(proj_plot(x, MSEhist, type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch")))
  testthat::expect_no_error(proj_plot(x, MSEhist, type = c("SSBMSY")))
  testthat::expect_no_error(proj_plot(x, MSEhist, type = c("SP")))
  testthat::expect_no_error(proj_plot(x, MSEhist, type = c("F")))
  testthat::expect_no_error(proj_plot(x, MSEhist, type = c("SPR")))
  testthat::expect_no_error(proj_plot(x, MSEhist, type = c("Catch")))

  testthat::expect_s4_class(PM1 <- make_PMobj(x,
                                              year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                              SSBhist_yr = x@OM$CurrentYr[1],
                                              label = "SSB_PM"),
                            "PMobj")
  testthat::expect_s4_class(make_PMobj(x,
                                       type = "SSB0",
                                       year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                       SSB0_type = "Dynamic",
                                       label = "mylabel"),
                            "PMobj")
  testthat::expect_s4_class(make_PMobj(x,
                                       type = "SSBMSY",
                                       year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                       label = "mylabel"),
                            "PMobj")
  testthat::expect_s4_class(make_PMobj(x,
                                       type = "F",
                                       year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                       label = "mylabel"),
                            "PMobj")
  testthat::expect_s4_class(make_PMobj(x,
                                       type = "SPR",
                                       year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                       label = "mylabel"),
                            "PMobj")
  testthat::expect_s4_class(PM2 <- make_PMobj(x,
                                       type = "Catch",
                                       year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                       label = "Catch_PM",
                                       Chist_yr = x@OM$CurrentYr[1]),
                            "PMobj")
  testthat::expect_s4_class(make_PMobj(x,
                                       type = "SSB50%Rmax",
                                       year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                       label = "mylabel"),
                            "PMobj")
  testthat::expect_s4_class(make_PMobj(x,
                                       type = "SSB90%R/S",
                                       year_range = x@OM$CurrentYr[1] + c(1, x@proyears),
                                       label = "mylabel"),
                            "PMobj")


  testthat::expect_no_error(prob_plot(x, PM_list = list(PM1), xlim = NULL, ylim = NULL, figure = TRUE)) # Only works with length-one list


  testthat::expect_no_error(stoch_plot(
    x,
    MPstoch = x@MPs,
    qval = 0.9,
    type = c("SSB0")
  ))
  testthat::expect_no_error(stoch_plot(
    x,
    MPstoch = x@MPs,
    qval = 0.9,
    type = c("SSBMSY")
  ))
  testthat::expect_no_error(stoch_plot(
    x,
    MPstoch = x@MPs,
    qval = 0.9,
    type = c("SP")
  ))
  testthat::expect_no_error(stoch_plot(
    x,
    MPstoch = x@MPs,
    qval = 0.9,
    type = c("F")
  ))
  testthat::expect_no_error(stoch_plot(
    x,
    MPstoch = x@MPs,
    qval = 0.9,
    type = c("SPR")
  ))
  testthat::expect_no_error(stoch_plot(
    x,
    MPstoch = x@MPs,
    qval = 0.9,
    type = c("Catch")
  ))

  testthat::expect_no_error(
    hist_sim(
      x,
      MSEhist = x@Hist,
      MP = "NFref",
      sims = 1:2,
      type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch")
    ))

  testthat::expect_no_error(
    hist_sim(
      x,
      MSEhist = x@Hist,
      MP = "NFref",
      sims = 1:2,
      type = c("SSBMSY")
    ))

  testthat::expect_no_error(
    hist_sim(
      x,
      MSEhist = x@Hist,
      MP = "NFref",
      sims = 1:2,
      type = c("SP")
    ))

  testthat::expect_no_error(
    hist_sim(
      x,
      MSEhist = x@Hist,
      MP = "NFref",
      sims = 1:2,
      type = c("F")
    ))

  testthat::expect_no_error(
    hist_sim(
      x,
      MSEhist = x@Hist,
      MP = "NFref",
      sims = 1:2,
      type = c("SPR")
    ))

  testthat::expect_no_error(
    hist_sim(
      x,
      MSEhist = x@Hist,
      MP = "NFref",
      sims = 1:2,
      type = c("Catch")
    ))

  testthat::expect_no_error(
    lollipop_plot(x = x, PM_list = list(PNOF = PM1, MC = PM2)) # Needs a named list
  )

  testthat::expect_no_error(tradeoff_plot(x, PM1, PM2, xlab = parse(text = PM1@Caption), ylab = parse(text = PM2@Caption)))
  testthat::expect_no_error(radar_plot(x, PM_list = list(PNOF = PM1, MC = PM2))) # Needs named list of 3+ PMs

}

