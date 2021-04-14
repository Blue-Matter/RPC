

# Custom MPs

# DFO

SCA_6020<-make_MP(SCA_Pope,HCR60_20)

# CurF ===========================================================================

CurF_200<-function (x, Data, reps, plot = FALSE){
  rec <- new("Rec")
  rec@Effort <- 2
  rec
}

CurF_150<-function (x, Data, reps, plot = FALSE){
  rec <- new("Rec")
  rec@Effort <- 1.5
  rec
}

CurF_100<-function (x, Data, reps, plot = FALSE){
  rec <- new("Rec")
  rec@Effort <- 1
  rec
}

CurF_050<-function (x, Data, reps, plot = FALSE){
  rec <- new("Rec")
  rec@Effort <- 0.5
  rec
}

CurF_000<-function (x, Data, reps, plot = FALSE){
  rec <- new("Rec")
  rec@Effort <- 0
  rec
}

class(CurF_200)<-"MP"
class(CurF_150)<-"MP"
class(CurF_100)<-"MP"
class(CurF_050)<-"MP"
class(CurF_000)<-"MP"


# CurC ============================================================================

CurC_200<-function (x, Data, reps = 100, plot = FALSE){
  yrlast <- match(Data@LHYear[1], Data@Year)
  C_dat <- Data@Cat[x, yrlast]*2
  Rec <- new("Rec")
  Rec@TAC <-C_dat
  Rec
}

CurC_150<-function (x, Data, reps = 100, plot = FALSE){
  yrlast <- match(Data@LHYear[1], Data@Year)
  C_dat <- Data@Cat[x, yrlast]*1.5
  Rec <- new("Rec")
  Rec@TAC <-C_dat
  Rec
}

CurC_100<-function (x, Data, reps = 100, plot = FALSE){
  yrlast <- match(Data@LHYear[1], Data@Year)
  C_dat <- Data@Cat[x, yrlast]
  Rec <- new("Rec")
  Rec@TAC <-C_dat
  Rec
}

CurC_050<-function (x, Data, reps = 100, plot = FALSE){
  yrlast <- match(Data@LHYear[1], Data@Year)
  C_dat <- Data@Cat[x, yrlast]*0.5
  Rec <- new("Rec")
  Rec@TAC <-C_dat
  Rec
}

CurC_000<-function (x, Data, reps = 100, plot = FALSE){
  yrlast <- match(Data@LHYear[1], Data@Year)
  C_dat <-0
  Rec <- new("Rec")
  Rec@TAC <-C_dat
  Rec
}

class(CurC_200)<-"MP"
class(CurC_150)<-"MP"
class(CurC_100)<-"MP"
class(CurC_050)<-"MP"
class(CurC_000)<-"MP"
