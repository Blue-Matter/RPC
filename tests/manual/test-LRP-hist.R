


OMs <- avail("OM", package = "RPC")

for(i in 1:length(OMs)) {
#for(i in 1:2) {

  OM_test <- get(OMs[i])
  x <- OM_test %>% SubCpars(1:3) %>% runMSE(Hist = TRUE, silent = TRUE)

  testthat::expect_s4_class(x, "Hist")

  testthat::expect_no_error(hist_bio(x, figure = TRUE))

  testthat::expect_no_error(hist_future_recruit(x, figure = TRUE))

  testthat::expect_no_error(hist_bio_schedule(
    x,
    var = c("Len_age"),
    n_age_plot = 3,
    yr_plot = x@OM@CurrentYr[1],
    sim = 1,
    figure = TRUE
  ))
  testthat::expect_no_error(hist_bio_schedule(
    x,
    var = c("Wt_age"),
    n_age_plot = 3,
    yr_plot = x@OM@CurrentYr[1],
    sim = 1,
    figure = TRUE
  ))
  testthat::expect_no_error(hist_bio_schedule(
    x,
    var = c("Mat_age"),
    n_age_plot = 3,
    yr_plot = x@OM@CurrentYr[1],
    sim = 1,
    figure = TRUE
  ))
  testthat::expect_no_error(hist_bio_schedule(
    x,
    var = c("M_ageArray"),
    n_age_plot = 3,
    yr_plot = x@OM@CurrentYr[1],
    sim = 1,
    figure = TRUE
  ))

  testthat::expect_no_error(hist_growth_I(x))

  testthat::expect_no_error(hist_growth_II(x))

  testthat::expect_no_error(hist_spatial(x, type = c("par")))
  testthat::expect_no_error(hist_spatial(x, type = c("matrix")))
  testthat::expect_no_error(hist_spatial(x, type = c("all")))

  testthat::expect_no_error(hist_sel(x, yr = x@OM@CurrentYr + c(-4, 0), maturity = TRUE, figure = TRUE))

  testthat::expect_no_error(hist_YieldCurve(x, yr_bio = x@OM@CurrentYr, yr_sel = x@OM@CurrentYr,
                                            F_range = seq(0, 1, 0.1), figure = TRUE))

  testthat::expect_no_error(hist_resample_recruitment(
    x,
    dist = c("Lognormal"),
    mu = 1,
    LnSD = 0.7,
    LnAC = 0,
    Pshape = 1.1,
    figure = TRUE,
    nsim_plot = 5
  ))
  testthat::expect_no_error(hist_resample_recruitment(
    x,
    dist = c("Pareto"),
    mu = 1,
    LnSD = 0.7,
    LnAC = 0,
    Pshape = 1.1,
    figure = TRUE,
    nsim_plot = 5
  ))

  testthat::expect_no_error(hist_SRR_change(x, SR_new = 1, h_mult = 2, y_fit = x@OM@CurrentYr - c(x@OM@nyears, 1) + 1, figure = TRUE))
  testthat::expect_no_error(hist_SRR_change(x, SR_new = 2, h_mult = 2, y_fit = x@OM@CurrentYr - c(x@OM@nyears, 1) + 1, figure = TRUE))

  testthat::expect_no_error(hist_phi0(x, figure = TRUE))

  testthat::expect_no_error(hist_per_recruit(x, yr_bio = x@OM@CurrentYr,
                                             yr_sel = x@OM@CurrentYr,
                                             F_range = seq(0, 1, 0.1), figure = TRUE))



  testthat::expect_no_error(LRP_SSBhist(
    x,
    figure = c("ts"),
    SSB_y = x@OM@CurrentYr,
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))
  testthat::expect_no_error(LRP_SSBhist(
    x,
    figure = c("prob"),
    SSB_y = x@OM@CurrentYr,
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))

  testthat::expect_no_error(LRP_SSBMSY(
    x,
    figure = c("ts"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))
  testthat::expect_no_error(LRP_SSBMSY(
    x,
    figure = c("prob"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))


  testthat::expect_no_error(LRP_SSB0(
    x,
    figure = c("ts", "prob", "none"),
    type = c("equilibrium", "initial", "dynamic"),
    prob_ratio = 0.4,
    prob_ylim = c(0, 1)
  ))


  testthat::expect_no_error(LRP_SP(x, figure = c("ts"), Bunit = c("B", "VB", "SSB")))
  testthat::expect_no_error(LRP_SP(x, figure = c("phase"), Bunit = c("B", "VB", "SSB")))

  testthat::expect_no_error(LRP_R(
    x,
    figure = c("ts"),
    #SR_xlim,
    #SR_ylim,
    #SR_y_RPS0,
    SR_include = 1:3
  ))

  testthat::expect_no_error(LRP_R(
    x,
    figure = c("SR"),
    #SR_xlim,
    #SR_ylim,
    #SR_y_RPS0,
    SR_include = 1:3
  ))

  testthat::expect_no_error(LRP_RPS(x, figure = c("ts", "none")))

  testthat::expect_no_error(LRP_SPR(
    x,
    figure = c("ts"),
    prob_ratio = 0.4,
    prob_ylim = c(0, 1)
  ))
  testthat::expect_no_error(LRP_SPR(
    x,
    figure = c("prob"),
    prob_ratio = 0.4,
    prob_ylim = c(0, 1)
  ))

  testthat::expect_no_error(LRP_FMSY(
    x,
    figure = c("ts", "prob", "none"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))
  testthat::expect_no_error(LRP_FMSY(
    x,
    figure = c("prob"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))

  ## Not currently used in RPC app
  testthat::expect_no_error(LRP_Fmed(
    x,
    figure = c("ts", "prob", "none"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))

  testthat::expect_no_error(LRP_50Rmax(
    x,
    figure = c("ts", "prob", "none"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))
  testthat::expect_no_error(LRP_50Rmax(
    x,
    figure = c("prob"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))

  testthat::expect_no_error(LRP_RPS90(
    x,
    figure = c("ts", "prob", "none"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))

  testthat::expect_no_error(LRP_RPS90(
    x,
    figure = c("prob"),
    prob_ratio = 1,
    prob_ylim = c(0, 1)
  ))

}

