

# Custom MPs

# DFO

#SCA_6020<-make_MP(SCA_Pope,HCR60_20)


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

#' @name make-MP
#' @title Functions for making management procedures
#'
#' @description Various functions that generate MPs
NULL

#' @rdname make-MP
#' @details \code{make_FixC_MP} generates a fixed catch MP.
#' @param ratio Numeric, the ratio of projected F or catch relative to that in the last historical year.
#' @export
make_FixC_MP <- function(ratio = 1) {
  MP_out <- CurC
  formals(MP_out)$val <- ratio
  structure(MP_out, class = "MP", RPC = TRUE)
}


#' @rdname make-MP
#' @details \code{make_FixF_MP} generates a fixed F MP. Ensure that \code{OM@@qinc <- OM@@qcv <- c(0, 0)}.
#' @export
make_FixF_MP <- function(ratio) {
  MP_out <- CurF
  formals(MP_out)$val <- ratio
  structure(MP_out, class = "MP", RPC = TRUE)
}

#' @rdname make-MP
#' @details \code{make_RPC_MP} generates MP using a harvest control rule in the App.
#' @param input The input list from a shiny server session.
#' @param verbose Whether to return information on the MP in the server log.
#' @export
make_RPC_MP <- function(input, verbose = TRUE) {
  Assess <- input$MS_Origin

  OCP_type <- input$MS_IVar
  SPR_OCP <- ifelse(OCP_type == "F_FSPR", input$SPR_OCP, NA_real_)

  Ftarget_type <- input$MS_DVar
  SPR_targ <- ifelse(Ftarget_type == "FSPR", input$SPR_targ, NA_real_)

  if(input$MS_control == 1) {
    relF_min <- input$CP_yint
    relF_max <- input$CP_yint
    LOCP <- TOCP <- 0
  } else {
    relF_min <- input$CP_1_x
    relF_max <- input$CP_2_x
    LOCP <- input$CP_1_y
    TOCP <- input$CP_2_y
  }

  MP <- make_MP(Assess, HCR_segment, OCP_type = OCP_type, Ftarget_type = Ftarget_type,
                OCP = c(LOCP, TOCP), relF = c(relF_min, relF_max),
                SPR_OCP = SPR_OCP, SPR_targ = SPR_targ)
  attr(MP, "RPC") <- TRUE

  if(verbose) {
    AM(paste0("Management Procedure '", input$MS_HCR_Label, "' constructed as:\n",
              paste0("  make_MP(Assess = ", Assess, ", HCR = HCR_ramp, OCP_type = \"", OCP_type, "\", Ftarget_type = \"", Ftarget_type,
                     "\", LOCP = ", LOCP, ", TOCP = ", TOCP, ", relF_min = ", relF_min, ", refF_max = ", relF_max,
                     ", SPR_OCP = ", SPR_OCP, ", SPR_targ = ", SPR_targ, ")")))
  }

  return(MP)
}

#' @rdname make-MP
#' @details \code{make_HCR_name} returns a default name or description of the harvest control rule.
#' @param type Whether a label or full description is returned.
#' @export
make_HCR_name <- function(input, type = c("label", "description")) {
  type <- match.arg(type)

  output_name <- input$MS_DVar # FMSY, F01, Fmax, FSPR

  if(type == "label") {
    mod_name <- switch(input$MS_Origin, "Perfect" = "Perf", "SCA_Pope" = "Assess", "Shortcut2" = "Short")

    if(input$MS_control == 1) {
      output_val <- input$CP_yint * 100

      MS_name <- paste0(mod_name, "_", output_val, input$MS_DVar)
    } else {
      output_val <- paste0(100 * input$CP_2_y, "/", 100 * input$CP_1_y)
      OCP_name <- input$MS_IVar %>% strsplit("_") %>% getElement(1) %>% getElement(2)
      OCP_val <- paste0(100 * input$CP_2_x, "/", 100 * input$CP_1_x)

      MS_name <- paste0(mod_name, "_", output_val, input$MS_DVar, "_", OCP_val, OCP_name)
    }
  } else {
    mod_name <- paste0(", values obtained from ",
                       switch(input$MS_Origin,
                              "Perfect" = "operating model",
                              "SCA_Pope" = "fitted assessment model",
                              "Shortcut2" = "assessment emulator"
                       ),
                       " and update interval of ", input$MS_interval, " years")

    if(input$MS_control == 1) {
      output_val <- paste0(input$CP_yint * 100, "% ", output_name)

      MS_name <- paste0("HCR to set catch at ", output_val, mod_name)
    } else {
      output_val <- paste0(100 * input$CP_2_y, ", ", 100 * input$CP_1_y, "%")
      OCP_val <- paste0(100 * input$CP_2_x, ", ", 100 * input$CP_1_x, "% ")
      OCP_name <- input$MS_IVar %>% strsplit("_") %>% getElement(1) %>% getElement(2)

      MS_name <- paste0("HCR with control points at ", OCP_val, OCP_name,
                        "with corresponding catch set at ", output_val, "%", output_name, mod_name)
    }
  }
  return(MS_name)
}
