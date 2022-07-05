
# === Build script for Spring Spawning Atlantic Herring in NAFO 4T (sGSL) =======================
library(MSEtool)
setwd("G:\\Shared drives\\BM shared\\1. Projects\\RPC\\Operating models\\GSL_Herring\\SS")

######################## Preparation from SCA output
nsim <- 250
Yr <- 1978:2019
nyears <- length(Yr)
maxage <- 11
n_age <- maxage + 1

#### Maturity
Mat <- ifelse(0:maxage >= 4, 1, 0) %>% array(c(n_age, nsim, nyears)) %>% aperm(c(2, 1, 3))

#### SCA F 1978-2017
FM <- local({
  F_SCA <- read.csv("data/SS_F_Table_21.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  F_age <- matrix(NA_real_, nyears, n_age)
  F_age[, 3:12] <- F_SCA[, -c(1, 12)]

  F_age[, 1:2] <- 0
  F_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})

M <- local({
  N_age <- matrix(NA_real_, nyears, n_age)
  N_SCA <- read.csv("data/SS_N_Table_20.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  N_age[, 3:12] <- N_SCA[, -c(1, 12)]
  F_age <- FM[1, , ] %>% t()

  M_age <- matrix(NA_real_, nyears, n_age)
  for(y in 2:nyears - 1) {
    for(a in 3:n_age - 1) {
      M_age[y,a] <- -log(N_age[y+1,a+1]/N_age[y,a]/exp(-F_age[y,a]))
    }
    M_age[y,n_age] <- -log(N_age[y+1,n_age]/(N_age[y,n_age] * exp(-F_age[y,n_age]) + N_age[y,n_age-1] * exp(-F_age[y,n_age-1])))
  }
  M_age[, n_age - 1] <- M_age[, n_age]
  M_age[nyears, ] <- M_age[nyears - 1, ]
  M_age[, 1:2] <- M_age[, 3]
  M_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})

#### SCA abundance 1978-2018
N <- local({
  N_age <- matrix(NA_real_, nyears, n_age)
  N_SCA <- read.csv("data/SS_N_Table_20.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  N_age[, 3:12] <- N_SCA[, -c(1, 12)]

  # Back calculate to age - 0 with Z = M
  ages <- 2:1
  for(a in ages) N_age[2:nyears - 1, a] <- N_age[2:nyears, a+1] * exp(M[1, a, 2:nyears - 1])
  N_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})

#### SCA biomass 1978-2018
#### Derive weight at age
Wt <- local({
  B_SCA <- read.csv("data/SS_B_Table_19.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  N_SCA <- read.csv("data/SS_N_Table_20.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)

  Wt_age <- matrix(NA_real_, nyears, n_age)
  Wt_age[, 3:12] <- B_SCA[, -c(1, 12)]/N_SCA[, -c(1, 12)]
  Wt_age[, 1] <- 0.03
  Wt_age[, 2] <- 0.07 # Table 11
  Wt_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})

#### Length at age (parameters from Wigley et al. 2003)
a <- 7.5e-6
b <- 3.0314
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
SRrel <- 2 # 1 = Beverton-Holt, Ricker = 2
h_profile <- seq(0.3, ifelse(SRrel == 1, 0.99, 1.2), 0.01)


################# Predict SR-relationship from low regime (1992 - 2017)
opt_profile <- sapply(h_profile, function(x) {
  optimize(SAMtool:::get_SR, interval = log(c(0.5, 2) * mean(R, na.rm = TRUE)),
           E = SSB[Yr >= 1992 & !is.na(R)], R = R[Yr >= 1992 & !is.na(R)],
           EPR0 = phi0, fix_h = TRUE, h = x, type = ifelse(SRrel == 1, "BH", "Ricker"))$objective
})
plot(h_profile, opt_profile, xlab = "Steepness", ylab = "Negative log-likelihood"); data.frame(h_profile, opt_profile)

h <- h_profile[which.min(opt_profile)] # Ricker h = 0.46

# Predict recruitment (over all years)
SR_fit <- local({
  opt <- optimize(SAMtool:::get_SR, interval = log(c(0.5, 2) * mean(R, na.rm = TRUE)),
                  E = SSB[Yr >= 1992 & !is.na(R)], R = R[Yr >= 1992 & !is.na(R)], EPR0 = phi0, fix_h = TRUE, h = h,
                  type = ifelse(SRrel == 1, "BH", "Ricker"))

  SAMtool:::get_SR(opt$minimum, opt = FALSE, figure = TRUE,
                   E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = h,
                   type = ifelse(SRrel == 1, "BH", "Ricker"))
})

# Stock-recruit alpha, beta
SR_fit$Arec # 8.48 (vs. 8.43, Turcotte 2022, p. 8)
SR_fit$Brec # 9.7e-6 (vs. 1.4e-5, Turcotte 2022, p. 8)


Perr <- R[!is.na(R)]/SR_fit$Rpred # Recruitment deviations
log_dev <- log(Perr)
sigmaR <- sd(log_dev) # SD of log-recruitment deviation
AC_lag1 <- acf(log_dev, plot = FALSE)$acf[2, 1, 1] # autocorrelation
R0 <- SR_fit$R0




OM <- VPA2OM(Name = "Gulf of St. Lawrence Spring Spawning Herring", CurrentYr = max(Yr),
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
OM@Name <- "Gulf of St. Lawrence Spring Spawning Herring (SCA assessment, low Ricker regime 1992-2017)"
OM@Agency <- "DFO"
OM@Region <- "NAFO 4T (Gulf of St. Lawrence)"
OM@Common_Name <- "Atlantic Herring"
OM@Species <- "Clupea harengus"
OM@Source <- c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2021/2021_030-eng.html",
               SAR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2020/2020_029-eng.html")

OM@a <- a
OM@b <- b
OM@cpars$Linf <- rep(29.225, nsim)
OM@cpars$K <- rep(0.315, nsim)
OM@cpars$t0 <- rep(-2.58, nsim)

DFO_GSL_SS_Herring_2019 <- OM

usethis::use_data(DFO_GSL_SS_Herring_2019, overwrite = TRUE)
