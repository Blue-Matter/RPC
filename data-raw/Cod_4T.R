


OM <- readRDS("data-raw/4TcodOM.rds")
Hist = runMSE(OM, Hist = TRUE)

SSB <- apply(Hist@TSdata$SBiomass, 1:2, sum)
R <- apply(Hist@AtAge$Number[, 3, , ], 1:2, sum)

#plot(SSB[1, 1:45], R[1, 1:45], xlim = c(0, 4e5), ylim = c(0, 6e5))
#Yr <- OM10@CurrentYr - OM@nyears:1 + 1
#text(SSB[1, 1:45], R[1, 1:45], Yr[1:45], pos = 3)

N_age <- apply(Hist@AtAge$Number[1, , , ], 1:2, sum)
Wt_age <- OM@cpars$Wt_age[1,,1:OM@nyears]
Mat_age <- OM@cpars$Mat_age[1,,1:OM@nyears]
M_age <- OM@cpars$M_ageArray[1,,1:OM@nyears]
F_age <- Hist@AtAge$F.Mortality[1, , , 1]



######################## Preparation from SCA output
nsim <- 250
Yr <- 1971:2018
nyears <- length(Yr)
maxage <- 12
n_age <- maxage + 1

#### Maturity
Mat <- Mat_age %>% array(c(n_age, nyears, nsim)) %>% aperm(c(3, 1, 2))

#### F
FM <- F_age %>% array(c(n_age, nyears, nsim)) %>% aperm(c(3, 1, 2))

M <- M_age %>% array(c(n_age, nyears, nsim)) %>% aperm(c(3, 1, 2))

#### SCA abundance 1978-2018
N <- N_age %>% array(c(n_age, nyears, nsim)) %>% aperm(c(3, 1, 2))

#### Derive weight at age
Wt <- Wt_age %>% array(c(n_age, nyears, nsim)) %>% aperm(c(3, 1, 2))

#### Length at age # To be ignored
a <- 1
b <- 1
Laa <-  (Wt/a)^(1/b)

# Stock-recruitment
SSB <- apply(N * Wt * Mat, c(1, 3), sum, na.rm = TRUE)[1, ]
R <- N[1, 1, ]
#plot(Yr, R, typ = 'o')
#plot(Yr, SSB, typ = 'o')
#plot(SSB, R, xlim = c(0, 2.5e5), ylim = c(0, 1.5e5))

phi0 <- local({
  NPR <- numeric(n_age)
  NPR[1] <- 1
  for(a in 2:n_age) NPR[a] <- NPR[a-1] * exp(-M[1, a-1, 1])
  NPR[n_age] <- NPR[n_age]/(1 - exp(-M[1, n_age, 1]))
  sum(NPR * Mat[1, , 1] * Wt[1, , 1])
})

# Let's profile steepness by fitting a stock-recruit relationship
# to SSB and age-2 abundance estimates
SRrel <- 2# 1 = Beverton-Holt, Ricker = 2
h_profile <- seq(0.3, ifelse(SRrel == 1, 0.99, 1.2), 0.01)


################# Predict SR-relationship from low regime (1992 - 2017)
opt_profile <- sapply(h_profile, function(x) {
  optimize(SAMtool:::get_SR, interval = log(c(0.5, 2) * mean(R, na.rm = TRUE)),
           E = SSB[Yr %in% 2005:2015], R = R[Yr %in% 2005:2015],
           EPR0 = phi0, fix_h = TRUE, h = x, type = ifelse(SRrel == 1, "BH", "Ricker"))$objective
})
plot(h_profile, opt_profile, xlab = "Steepness", ylab = "Negative log-likelihood"); data.frame(h_profile, opt_profile)

h <- 0.99

# Predict recruitment (over all years)
SR_fit <- local({
  opt <- optimize(SAMtool:::get_SR, interval = log(c(0.5, 2) * mean(R, na.rm = TRUE)),
                  E = SSB[Yr %in% 2005:2015], R = R[Yr %in% 2005:2015], EPR0 = phi0, fix_h = TRUE, h = h,
                  type = ifelse(SRrel == 1, "BH", "Ricker"))

  SAMtool:::get_SR(opt$minimum, opt = FALSE, figure = TRUE,
                   E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = h,
                   type = ifelse(SRrel == 1, "BH", "Ricker"))
})

# Stock-recruit alpha, beta
SR_fit$Arec # 1.45
SR_fit$Brec # 3.76e-6


Perr <- R[!is.na(R)]/SR_fit$Rpred # Recruitment deviations
log_dev <- log(Perr)
sigmaR <- sd(log_dev) # SD of log-recruitment deviation
AC_lag1 <- acf(log_dev, plot = FALSE)$acf[2, 1, 1] # autocorrelation
R0 <- SR_fit$R0




out <- VPA2OM(Name = "Southern Gulf of St. Lawrence Atlantic Cod", CurrentYr = max(Yr),
             h = h, Obs = Precise_Unbiased,
             naa = N,
             faa = FM,
             waa = Wt,
             Mataa = Mat,
             Maa = M,
             laa = Laa,
             nyr_par_mu = 5,
             LowerTri = 3,
             #report = TRUE,
             SRrel = SRrel,
             R0 = R0, phi0 = phi0, AC = AC_lag1, Perr = sigmaR)
out@Name <- "Gulf of St. Lawrence Atlantic Cod (SCA assessment, low Ricker regime 2005-2015)"
out@Agency <- "DFO"
out@Region <- "NAFO 4T (Gulf of St. Lawrence)"
out@Common_Name <- "Atlantic Cod"
out@Species <- "Gadus morhua"
out@Source <- c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_038-eng.html",
               SAR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2019/2019_021-eng.html")

out@a <- a
out@b <- b
out@cpars$Linf <- rep(50, nsim)
out@cpars$K <- rep(0.2, nsim)
out@cpars$t0 <- rep(0, nsim)

DFO_GSL_Atlantic_Cod_2018 <- out

usethis::use_data(DFO_GSL_Atlantic_Cod_2018, overwrite = TRUE)
