

# Custom MPs

# DFO

SCA_6020<-make_MP(SCA_Pope,HCR60_20)

CurF <- function(x, Data, reps = 1, val = 1) {
  rec <- new("Rec")
  rec@Effort <- rep(val, reps)
  rec
}
class(CurF) <- "MP"

CurC <- function(x, Data, reps = 1, val = 1) {
  yrlast <- match(Data@LHYear[1], Data@Year)
  C_dat <- Data@Cat[x, yrlast] * val
  Rec <- new("Rec")
  Rec@TAC <- rep(C_dat, reps)
  Rec
}
class(CurC) <- "MP"

No_Fishing <- NFref
