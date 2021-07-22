
MSYCalcs2 <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, Fec_at_Age, V_at_Age,
                      maxage, R0x, SRrelx, hx, opt=1, plusgroup=0, SSBpR0) {
  # Box 3.1 Walters & Martell 2004
  n_age <- maxage + 1
  FF <- exp(logF)
  lx <- l0 <- rep(1, n_age)
  F_at_Age <- FF * V_at_Age
  Z_at_Age <- F_at_Age + M_at_Age
  surv0 <- exp(-M_at_Age)
  surv <- exp(-Z_at_Age)
  for (a in 2:n_age) {
    l0[a] <- l0[a-1] * surv0[a-1]
    lx[a] <- lx[a-1] * surv[a-1] # fished survival
  }
  if (plusgroup == 1) {
    l0[length(l0)] <- l0[length(l0)]/(1 - surv0[length(l0)])
    lx[length(lx)] <- lx[length(lx)]/(1 - surv[length(lx)])
  }

  Egg0 <- sum(l0 * Fec_at_Age) # unfished egg-per-recruit (assuming fecundity proportional to weight)
  EggF <- sum(lx * Fec_at_Age) # fished egg-per-recruit (assuming fecundity proportional to weight)

  vB0 <- sum(l0 * Wt_at_Age * V_at_Age) # unfished and fished vuln. biomass per-recruit
  vBF <- sum(lx * Wt_at_Age * V_at_Age)

  SB0 <- sum(l0 * Fec_at_Age) # spawning biomas per-recruit - same as eggs atm
  SBF <- sum(lx * Fec_at_Age)

  B0 <- sum(l0 * Wt_at_Age) # biomass-per-recruit
  BF <- sum(lx * Wt_at_Age)

  hx[hx>0.999] <- 0.999
  SPR <- EggF/Egg0

  # Calculate equilibrium recruitment at this SPR
  if (SRrelx ==1) { # BH SRR
    Arec <- 4*hx/(1-hx)/SSBpR0
    Brec <- (5*hx-1)/(1-hx)/(R0x*SSBpR0)
    RelRec <- (Arec * EggF-1)/(Brec * EggF)
    new_R0 <- (Arec * Egg0-1)/(Brec * Egg0)
  }
  if (SRrelx ==2) { # Ricker
    Brec <- 1.25 * log(5*h) / (R0x * SSBpR0)
    Arec <- ((5*hx)^1.25)/SSBpR0
    RelRec <- log(Arec * EggF)/Brec/EggF
    new_R0 <- log(Arec * Egg0)/Brec/Egg0
  }

  RelRec[RelRec<0] <- 0

  YPR <- sum(lx * Wt_at_Age * F_at_Age * (1 - exp(-Z_at_Age))/Z_at_Age)
  Yield <- YPR * RelRec

  if (opt == 1)  return(-Yield)
  if (opt == 2) {
    out <- c(Yield=Yield,
             F= FF,
             SB = SBF * RelRec,
             SB_SB0 = (SBF * RelRec)/(SB0 * new_R0),
             B_B0 = (BF * RelRec)/(B0 * new_R0),
             B = BF * RelRec,
             VB = vBF * RelRec,
             VB_VB0 = (vBF * RelRec)/(vB0 * new_R0),
             RelRec=RelRec,
             SB0 = SB0 * new_R0,
             B0=B0 * new_R0)
    return(out)
  }
}
