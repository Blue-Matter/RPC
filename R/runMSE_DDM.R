
#' Run a management strategy evaluation with density-dependent natural mortality
#'
#' **Experimental** A wrapper function that converts an \linkS4class{OM} to \linkS4class{MOM}, adds a density-dependent M relationship
#' (as a function of biomass) via \link{makeRel}, runs \link{multiMSE}, and converts the output back to an \linkS4class{MSE} object.
#'
#' @param OM An operating model.
#' @param M0 Natural mortality as the stock biomass reaches unfished levels. Vector of length \code{OM@nsim}.
#' @param M1 Natural mortality as the stock biomass reaches zero. Vector of length \code{OM@nsim}.
#' @param MPs A character vector of MPs.
#' @param HistRel Logical, whether the MICE relationship is used for the historical population. Generally `FALSE` if conditioned from
#' an outside model.
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical. Should the MSE be run using parallel processing? See Details for more information.
#' @param extended Logical. Return extended projection results?
#' @param checkMPs Logical. Check if the specified MPs exist and can be run on SimulatedData?
#' @return A \linkS4class{MSE} object.
#' @details Assumes that fecundity-at-age, weight-at-age, and maturity-at-age are constant in the projection.
#'
#' Natural mortality (M) is a linear function of stock depletion in terms to total biomass (B) in year y
#' (Forrest et al. 2018):
#' \deqn{M_y = M_0 + (M_1 + M_0) (1 - B_y/B_0)}{M_y = M_0 + (M_1 + M_0) (1 - B_y/B_0)}
#' with a constraint that \eqn{M_y = M_0} if \eqn{B_y > B_0}
#'
#' @references
#' Forrest, R., Holt, K., and Kronlund, A. 2018. Performance of alternative harvest control rules for two
#' Pacific groundfish stocks with uncertain natural mortality: Bias, robustness and trade-offs.
#' Fisheries Research 206: 259–286. \doi{10.1016/j.fishres.2018.04.007}
#' @export
runMSE_DDM <- function(OM, M0, M1, MPs = "NFref", HistRel = FALSE,
                       silent = FALSE, parallel = FALSE, extended = FALSE, checkMPs = FALSE) {

  if (length(M0) == 1) M0 <- rep(M0, OM@nsim)
  if (length(M1) == 1) M1 <- rep(M1, OM@nsim)

  if (!silent) message("Initial operating model sampling to generate density-dependent M relationship...")
  Hist <- local({
    OM@cpars$D <- rep(0.5, OM@nsim)
    runMSE(OM, Hist = TRUE, silent = TRUE)
  })
  m <- Hist@SampPars

  # Fix maturity, growth, and fecundity to last historical year
  update_LH <- function(x, StockPars, nyears, proyears) {
    mm <- StockPars[[x]]
    mm[, , OM@nyears + 1:proyears] <- replicate(OM@proyears, mm[, , OM@nyears])
    mm
  }
  OM@cpars[c("Mat_age", "Wt_age", "Fec_age")] <- lapply(c("Mat_age", "Wt_age", "Fec_Age"),
                                                        update_LH,
                                                        StockPars = m$Stock,
                                                        nyears = OM@nyears,
                                                        proyears = OM@proyears)

  # Get density-dependent MSY/unfished reference points
  refpt <- lapply(1:OM@nsim, function(x) {
    #B0 <- Hist@Ref$ReferencePoints$SSB0[x] Not the appropriate B0 to use
    mat <- m$Stock$Mat_age[x, , OM@nyears]
    weight <- m$Stock$Wt_age[x, , OM@nyears]
    fec <- m$Stock$Fec_Age[x, , OM@nyears]

    Arec <- SRalphaconv(h = m$Stock$hs[x], phi0 = m$Stock$SSBpR[x, 1], SR = m$Stock$SRrel[x])
    Brec <- SRbetaconv(h = m$Stock$hs[x], R0 = m$Stock$R0[x], phi0 = m$Stock$SSBpR[x, 1], SR = m$Stock$SRrel[x])

    n_age <- OM@maxage + 1
    surv_M <- rep(exp(-M0[x]), n_age)

    DDM_NPR0 <- calc_NPR(surv = exp(-M0[x]) %>% rep(n_age), n_age = n_age, plusgroup = m$Stock$plusgroup)
    DDM_phi0 <- sum(DDM_NPR0 * weight * mat)

    if(m$Stock$SRrel[x] == 1) {
      DDM_R0 <- (Arec * DDM_phi0 - 1)/Brec/DDM_phi0
    } else {
      DDM_R0 <- log(Arec * DDM_phi0)/Brec/DDM_phi0
    }
    DDM_BPR0 <- sum(DDM_NPR0 * weight)
    DDM_B0 <- DDM_BPR0 * DDM_R0

    # MSY values
    #vul <- m$Fleet$V_real[x, , OM@nyears]
    #opt2 <- optimize(yield_fn_wrapper, interval = c(1e-4, 4), M = NA,
    #                 fec = fec, weight = weight, vul = vul,
    #                 SR = ifelse(m$Stock$SRrel[x] == 1, "BH", "Ricker"),
    #                 Arec = Arec, Brec = Brec, catch_eq = "Baranov", B0 = DDM_B0, tv_M = "DD",
    #                 M_bounds = c(M0, M1), plusgroup = m$Stock$plusgroup)
    #opt3 <- yield_fn_wrapper(opt2$minimum, M = NA, fec = fec, weight = weight, vul = vul,
    #                         SR = ifelse(m$Stock$SRrel[x] == 1, "BH", "Ricker"),
    #                         Arec = Arec, Brec = Brec, opt = FALSE, catch_eq = "Baranov", B0 = DDM_B0, tv_M = "DD",
    #                         M_bounds = c(M0, M1), plusgroup = m$Stock$plusgroup)
    #opt3["F"] <- opt2$minimum # FMSY

    # Unfished values - assumes plusgroup = TRUE
    unfished <- yield_fn_wrapper(0, M = NA, fec = fec, weight = weight, vul = rep(1, OM@maxage + 1),
                                 SR = ifelse(m$Stock$SRrel[x] == 1, "BH", "Ricker"),
                                 Arec = Arec, Brec = Brec, opt = FALSE, catch_eq = "Baranov",
                                 B0 = DDM_B0, tv_M = "DD",
                                 M_bounds = c(M0, M1),
                                 plusgroup = m$Stock$plusgroup)
    #list(MSY = opt3, unfished = unfished)
    return(unfished)
  })
  B0_proj <- vapply(refpt, getElement, numeric(1), "B")

  MOM <- makeMOM(OM)
  MOM@cpars$control$HistRel <- HistRel # If FALSE, ignore density-dependent M for historical reconstruction
  Rel <- makeRel(M0 = M0, M1 = M1, B0 = B0_proj)
  MOM@Rel <- list(Rel)

  multiHist <- SimulateMOM(MOM, parallel = parallel, silent = silent)

  # Update annual reference points FMSY
  MMSE <- ProjectMOM(multiHist, MPs = list(list(MPs)), silent = silent, parallel = parallel, checkMPs = checkMPs, dropHist = TRUE)

  Histout <- multiHist[[1]][[1]]
  Histout@Misc <- Hist@Misc
  Histout@OM <- Hist@OM
  MSEout <- new("MSE",
                Name = OM@Name,
                nyears = OM@nyears, proyears = OM@proyears, nMPs = length(MPs),
                MPs = MPs, nsim = OM@nsim,
                OM = Hist@Data@OM, # Update D and Depletion below
                Obs = Hist@Data@Obs,
                SB_SBMSY = MMSE@SB_SBMSY[, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                F_FMSY = MMSE@F_FMSY[, 1, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                N = MMSE@N[, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                B = MMSE@B[, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                SSB = MMSE@SSB[, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                V = MMSE@VB[, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                FM = MMSE@FM[, 1, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                SPR = lapply(MMSE@SPR, function(x) x[, 1, , ]),
                Catch = MMSE@Catch[, 1, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                Removals = MMSE@Removals[, 1, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                Effort = MMSE@Effort[, 1, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                TAC = MMSE@TAC[, 1, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                TAE = MMSE@TAE[, 1, 1, , ] %>% array(c(OM@nsim, length(MPs), OM@proyears)),
                BioEco = list(LatEffort = array(NA, c(OM@nsim, length(MPs), OM@proyears)),
                              Revenue = array(NA, c(OM@nsim, length(MPs), OM@proyears)),
                              Cost = array(NA, c(OM@nsim, length(MPs), OM@proyears))
                ),
                RefPoint=list(#MSY=MSY_y,
                  #FMSY=FMSY_y,
                  #SSBMSY=SSBMSY_y,
                  #F_SPR=F_SPR_y,
                  Dynamic_Unfished = MMSE@RefPoint$Dynamic_Unfished,
                  ByYear = MMSE@RefPoint$ByYear
                ),
                CB_hist = apply(multiHist[[1]][[1]]@TSdata$Landings, 1:2, sum),
                FM_hist = multiHist[[1]][[1]]@SampPars$Fleet$qs * multiHist[[1]][[1]]@SampPars$Fleet$Find,
                SSB_hist = apply(multiHist[[1]][[1]]@TSdata$SBiomass, 1:2, sum),
                Hist = new("Hist"), #$ if(extended) Histout, see below
                PPD = MMSE@PPD[[1]][[1]],
                Misc = list()
  )
  MSEout@OM$D <- multiHist[[1]][[1]]@Data@OM$D
  MSEout@OM$Depletion <- multiHist[[1]][[1]]@Data@OM$Depletion
  if (extended) MSEout@Hist <- Histout # also add Misc

  # Store MSE info
  attr(MSEout, "version") <- packageVersion("MSEtool")
  attr(MSEout, "date") <- date()
  attr(MSEout, "R.version") <- R.version

  MSEout
}


# Taken from SAMtool:::calc_NPR
calc_NPR <- function(surv, n_age, plusgroup = TRUE) {
  NPR <- numeric(n_age)
  NPR[1] <- 1
  for(a in 2:n_age) NPR[a] <- NPR[a-1] * surv[a-1]
  if (plusgroup) NPR[n_age] <- NPR[n_age]/(1 - surv[n_age])
  return(NPR)
}

#Taken from SAMtool:::Baranov
Baranov <- function(sel = 1, apicalF, M, N) {
  FF <- sel * apicalF
  Z <- FF + M
  CAA <- FF / Z * (1 - exp(-Z)) * N
  return(CAA)
}

# Taken from SAMtool:::yield_fn_SCA - replace maturity with fecundity and add plusgroup argument
yield_fn_wrapper <- function(x, M, fec, weight, vul,
                             SR = c("BH", "Ricker"), Arec, Brec,
                             catch_eq = c("Baranov", "Pope"), opt = TRUE, x_transform = FALSE, B0 = 1,
                             tv_M = c("none", "walk", "DD"), M_bounds = NULL,
                             plusgroup = TRUE) {
  if (is.null(tv_M)) tv_M <- "none"
  tv_M <- match.arg(tv_M)

  if (tv_M != "DD") {
    yield_fn(x, M, fec, weight, vul, SR, Arec, Brec, catch_eq, opt, x_transform, plusgroup)
  } else {

    dep <- M_DD <- numeric(21)
    dep[1] <- 0.4
    for(i in 1:20) {
      M_DD[i] <- ifelse(dep[i] >= 1, M_bounds[1],
                        ifelse(dep[i] <= 0, M_bounds[2], M_bounds[1] + (M_bounds[2] - M_bounds[1]) * (1 - dep[i])))
      out <- yield_fn(x, M = rep(M_DD[i], length(mat)), fec, weight, vul, SR, Arec, Brec, catch_eq,
                      opt = FALSE, x_transform = x_transform, plusgroup = plusgroup)
      if (abs(out["B"]/B0 - dep[i]) <= 1e-4) break
      dep[i+1] <- out["B"]/B0
    }

    if (opt) {
      return(-1 * out["Yield"])
    } else {
      return(out)
    }
  }
}


# Taken from SAMtool:::yield_fn_SCA_int
yield_fn <- function(x, M, fec, weight, vul, SR = c("BH", "Ricker"), Arec, Brec,
                     catch_eq = c("Baranov", "Pope"), opt = TRUE, x_transform = FALSE, plusgroup = TRUE) {
  SR <- match.arg(SR)
  catch_eq <- match.arg(catch_eq)
  if (catch_eq == "Baranov") {
    FMort <- ifelse(x_transform, exp(x), x)
    surv <- exp(-vul * FMort - M)
  } else {
    U <- ifelse(x_transform, ilogit(x), x)
    surv <- exp(-M) * (1 - vul * U)
  }
  n_age <- length(M)
  NPR <- calc_NPR(surv, n_age, plusgroup)
  EPR <- sum(NPR * fec)
  if (SR == "BH") {
    Req <- (Arec * EPR - 1)/(Brec * EPR)
  } else if (SR == "Ricker") {
    Req <- log(Arec * EPR)/(Brec * EPR)
  }

  if (catch_eq == "Baranov") {
    CPR <- Baranov(vul, FMort, M, NPR)
  } else {
    CPR <- vul * U * NPR * exp(-0.5 * M)
  }
  YPR <- sum(CPR * weight)
  Yield <- YPR * Req
  if (opt) {
    return(-1 * Yield)
  } else {

    B <- Req * sum(NPR * weight)
    E <- Req * EPR
    VB <- Req * sum(NPR * vul * weight)

    return(c(EPR = EPR, Yield = Yield, YPR = YPR, B = B, E = E, VB = VB, R = Req))
  }
}
