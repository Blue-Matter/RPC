
# === Build script for Spring Spawning Atlantic Herring in NAFO 4T (sGSL) =======================
library(MSEtool)
setwd("G:\\Shared drives\\BM shared\\1. Projects\\RPC\\Operating models\\GSL_Herring\\SS")

######################## Preparation from VPA output
nsim <- 250
Yr <- 1978:2019
nyears <- length(Yr)
maxage <- 11
n_age <- maxage + 1

#### Maturity
Mat <- ifelse(0:maxage >= 4, 1, 0) %>% array(c(n_age, nsim, nyears)) %>% aperm(c(2, 1, 3))

#### VPA F 1978-2017
FM <- local({
  F_VPA <- read.csv("data/SS_F_Table_21.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  F_age <- matrix(NA_real_, nyears, n_age)
  F_age[, 3:12] <- F_VPA[, -c(1, 12)]

  F_age[, 1:2] <- 0
  F_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})

M <- local({
  N_age <- matrix(NA_real_, nyears, n_age)
  N_VPA <- read.csv("data/SS_N_Table_20.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  N_age[, 3:12] <- N_VPA[, -c(1, 12)]
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

#### VPA abundance 1978-2018
N <- local({
  N_age <- matrix(NA_real_, nyears, n_age)
  N_VPA <- read.csv("data/SS_N_Table_20.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  N_age[, 3:12] <- N_VPA[, -c(1, 12)]

  # Back calculate to age - 0 with Z = M
  ages <- 2:1
  for(a in ages) N_age[2:nyears - 1, a] <- N_age[2:nyears, a+1] * exp(M[1, a, 2:nyears - 1])
  N_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})

#### VPA biomass 1978-2018
#### Derive weight at age
Wt <- local({
  B_VPA <- read.csv("data/SS_B_Table_19.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)
  N_VPA <- read.csv("data/SS_N_Table_20.csv", header = FALSE) %>% as.matrix() %>% matrix(nyears, 12, byrow = TRUE)

  Wt_age <- matrix(NA_real_, nyears, n_age)
  Wt_age[, 3:12] <- B_VPA[, -c(1, 12)]/N_VPA[, -c(1, 12)]
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
h <- 0.9 # Profile is very flat

SR_fit <- local({
  opt <- optimize(SAMtool:::get_SR, interval = log(c(0.5, 2) * mean(R, na.rm = TRUE)),
                  E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = h)

  # Flat profile
  #h <- seq(0.3, 0.99, 0.01)
  #opt <- sapply(h, function(x) optimize(SAMtool:::get_SR, interval = log(c(0.5, 2) * mean(R, na.rm = TRUE)),
  #                                      E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = x)$objective)
  #plot(h, opt); data.frame(h, opt)

  SAMtool:::get_SR(opt$minimum, opt = FALSE, figure = TRUE,
                   E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = h)
})

Perr <- R[!is.na(R)]/SR_fit$Rpred
log_dev <- log(Perr)
sigmaR <- sd(log_dev)
AC_lag1 <- acf(log_dev, plot = FALSE)$acf[2, 1, 1]
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
             R0 = R0, phi0 = phi0, AC = AC_lag1, Perr = sigmaR)
OM@Name <- "Gulf of St. Lawrence Spring Spawning Herring (SCA assessment, steepness = 0.9)"
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
