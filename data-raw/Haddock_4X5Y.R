
# === Build script for Atlantic Haddock in NAFO 4X5Y =======================

library(MSEtool)
library(dplyr)

######################## Preparation from VPA output
nsim <- 250
Yr <- 1985:2015
nyears <- length(Yr)
maxage <- 11
n_age <- maxage + 1

#### Natural mortality
M <- array(0.2, c(nsim, n_age, nyears))
M[, 11:12, match(2000:2004, Yr)] <- 0.3
M[, 11:12, match(2005:2009, Yr)] <- 0.6
M[, 11:12, match(2010:2015, Yr)] <- 0.9

#### Maturity
Mat <- ifelse(0:maxage >= 4, 1, 0) %>% array(c(n_age, nsim, nyears)) %>% aperm(c(2, 1, 3))

#### VPA abundance 1985-2015
N <- local({
  N_age <- matrix(NA_real_, nyears + 1, n_age)
  N_VPA <- readxl::read_excel("docs/4X5Y Haddock Data_DLM tools case study.xlsx", sheet = "Stock-Recruit Predictions VPA", skip = 3)
  N_age[, -1] <- N_VPA[, c(2, 4, 6, 8, 10, 11, 13, 15, 17, 19, 20)] %>% as.matrix()

  # Back calculate to age - 0 with Z = M
  N_age[1:nyears, 1] <- N_age[2:(nyears+1), 2] * exp(0.2)
  N_age %>% array(c(nyears+1, n_age, nsim)) %>% aperm(3:1)
})

#### Weight at age
Wt <- local({
  Wt_VPA <- readxl::read_excel("docs/4X5Y Haddock Data_DLM tools case study.xlsx", sheet = "Weight at age", skip = 2)

  Wt_age <- matrix(NA_real_, nyears, n_age)
  Wt_age[] <- Wt_VPA[match(Yr, Wt_VPA$Year), 2:13] %>% as.matrix()
  Wt_age[1:9, 1] <- 0.007
  Wt_age[11, 11:12] <- Wt_age[12, 11:12]
  Wt_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})

#### Length at age
Laa <- local({
  L_VPA <- readxl::read_excel("docs/4X5Y Haddock Data_DLM tools case study.xlsx", sheet = "Length at age", skip = 2)

  Len_age <- matrix(NA_real_, nyears, n_age)
  Len_age[] <- L_VPA[match(Yr, L_VPA$Year), 2:13] %>% as.matrix()
  Len_age[1:3, 1] <- 6.5
  Len_age[7:9, 1] <- 8.5
  Len_age[11, 11:12] <- Len_age[12, 11:12]
  Len_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})


#### VPA F 1978-2017
FM <- local({
  N_age <- N[1, , ] %>% t()
  M_age <- M[1, , ] %>% t()
  F_age <- matrix(NA_real_, nyears, n_age)
  for(y in 1:nyears) {
    for(a in 2:n_age - 1) {
      F_age[y, a] <- -log(N_age[y+1, a+1] * exp(M_age[y, a]) / N_age[y, a])
    }
    F_age[y, n_age] <- -log(N_age[y+1, n_age]/(N_age[y, n_age] * exp(-M_age[y, n_age]) + N_age[y, n_age - 1] * exp(-M_age[y, n_age-1])))
  }
  F_age[, 1:2] <- 0
  F_age[, n_age - 1] <- F_age[, n_age]
  F_age %>% array(c(nyears, n_age, nsim)) %>% aperm(3:1)
})


# Stock-recruitment
SSB <- apply(N[, , 1:nyears] * Wt * Mat, c(1, 3), sum, na.rm = TRUE)[1, ]
R <- N[1, 1, 1:nyears]
#plot(Yr, R, typ = 'o')
#plot(Yr, SSB, typ = 'o')
#plot(SSB, R, xlim = c(0, 7e4), ylim = c(0, 5e5))

phi0 <- local({
  NPR <- numeric(n_age)
  NPR[1] <- 1
  for(a in 2:n_age) NPR[a] <- NPR[a-1] * exp(-M[1, a-1, 1])
  NPR[n_age] <- NPR[n_age]/(1 - exp(-M[1, n_age, 1]))
  sum(NPR * Mat[1, , 1] * Wt[1, , 1])
})
h <- 0.95 # Profile shows little S-R relationship (h -> 0.99)

SR_fit <- local({
  opt <- optimize(SAMtool:::get_SR, interval = log(c(0.5, 4) * mean(R, na.rm = TRUE)),
                  E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = h)

  # Profile shows little S-R relationship (h -> 0.99)
  #h <- seq(0.3, 0.99, 0.01)
  #opt <- sapply(h, function(x) optimize(SAMtool:::get_SR, interval = log(c(0.5, 4) * mean(R, na.rm = TRUE)),
  #              E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = x)$objective)
  #plot(h, opt); data.frame(h, opt)

  SAMtool:::get_SR(opt$minimum, opt = FALSE, figure = TRUE,
                   E = SSB[!is.na(R)], R = R[!is.na(R)], EPR0 = phi0, fix_h = TRUE, h = h)
})

Perr <- R[!is.na(R)]/SR_fit$Rpred
log_dev <- log(Perr)
sigmaR <- sd(log_dev)
AC_lag1 <- acf(log_dev, plot = FALSE)$acf[2, 1, 1]
R0 <- SR_fit$R0


OM <- VPA2OM(Name = "4X5Y Haddock", CurrentYr = max(Yr),
             h = h, Obs = Precise_Unbiased,
             naa = N[, , 1:nyears],
             faa = FM,
             waa = Wt,
             Mataa = Mat,
             Maa = M,
             laa = Laa,
             nyr_par_mu = 3,
             LowerTri = 1,
             #report = TRUE,
             R0 = R0, phi0 = phi0, AC = AC_lag1, Perr = sigmaR)
OM@Name <- "4X5Y Haddock, 2015 VPA assessment (steepness = 0.95)"
OM@Agency <- "DFO"
OM@Region <- "NAFO 4X5Y (W. Scotian Shelf & Bay of Fundy)"
OM@Source <- c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_041-eng.html",
               SAR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2017/2017_006-eng.html")

OM@Species <- "Melanogrammus aeglefinus"
OM@Common_Name <- "Haddock"
OM@Latitude <- 42.8
OM@Longitude <- -66

OM@a <- 8e-6
OM@b <- 3.04
OM@cpars$Linf <- rep(46, nsim)
OM@cpars$K <- rep(0.383, nsim)
OM@cpars$t0 <- rep(-0.55, nsim)


DFO_4X5Y_Haddock_2015 <- OM

usethis::use_data(DFO_4X5Y_Haddock_2015)

