
#' @name plot-MSE
#' @title Plot output from MSE
#' @description Figures of results of the closed-loop simulation by MP
#' @param x An object of class \linkS4class{MSE}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEproj} which is
#' the MSE object.
#' @param MSEhist If \code{x} is an MSE object, then a \linkS4class{Hist} object may also be needed.
#' @param type Character describing the type of output.
#' @return Various plots using base graphics or ggplot2.
#' @examples
#' Hist <- MSEtool::runMSE(Hist = TRUE)
#' hist_bio(Hist)
#' @author Q. Huynh

#' @rdname plot-MSE
#' @details \code{proj_plot} generates median trajectories (historical and projected) with reference points if applicable.
#' @importFrom reshape2 melt
#' @importFrom scales label_parse
#' @export
proj_plot<-function(x, MSEhist, type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch")) {
  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
    MSEhist <- x$MSEhist
  } else {
    MSEproj <- x
  }

  type <- match.arg(type)
  nMP<-MSEproj@nMPs
  MPcols <- rainbow(nMP,start=0.2,end=1)

  # Median plot (top)
  CurrentYr <- MSEproj@OM$CurrentYr[1]

  nyh<-MSEproj@nyears
  nyp<-MSEproj@proyears
  hy<-CurrentYr - nyh:1 + 1
  py<-CurrentYr + 1:nyp
  ay<-c(hy, py)

  hist <- switch(type,
                 "SSB0" = MSEproj@SSB_hist,
                 "SSBMSY" = MSEproj@SSB_hist,
                 "SP" = local({
                   B <- apply(MSEproj@Misc$extended$B, c(1, 3, 4), sum)
                   Removals <- apply(MSEproj@Misc$extended$Removals, c(1, 3, 4), sum)
                   B[, 1, 2:(nyh+1)] - B[, 1, 1:nyh] + Removals[, 1, 1:nyh]
                 }),
                 "F" = MSEproj@FM_hist,
                 "SPR" = MSEhist@TSdata$SPR$Equilibrium,
                 "Catch" = MSEproj@CB_hist) %>% apply(2, median)
  proj_med <- switch(type,
                     "SSB0" = MSEproj@SSB,
                     "SSBMSY" = MSEproj@SSB,
                     "SP" = MSEproj@B[, , 2:nyp - 1, drop = FALSE] - MSEproj@B[, , 2:nyp, drop = FALSE] +
                       MSEproj@Removals[, , 2:nyp, drop = FALSE],
                     "F" = MSEproj@FM,
                     "SPR" = MSEproj@SPR$Equilibrium,
                     "Catch" = MSEproj@Catch) %>% apply(c(2, 3), median)
  max_y <- max(hist, proj_med)

  ylab <- switch(type,
                 "SSB0" = "Spawning biomass (SSB)",
                 "SSBMSY" = "Spawning biomass (SSB)",
                 "SP" = "Surplus production",
                 "F" = "Fishing mortality",
                 "SPR" = "Equilibrium SPR",
                 "Catch" = "Removals")

  title_txt <- switch(type,
                      "SSB0" = parse(text = "Median~SSB~and~SSB[0]~reference~points"),
                      "SSBMSY" = parse(text = "Median~SSB~and~SSB[MSY]~reference~points"),
                      "SP" = "Median surplus production",
                      "F" = parse(text = "Median~F~and~F[MSY]~reference~points"),
                      "SPR" = "Median equilibrium SPR",
                      "Catch" = "Median catch")

  if(type == "SP") {
    dat_MP <- cbind(rep(hist[nyh], nMP), proj_med) %>%
      structure(dimnames = list(MP = MSEproj@MPs, Year = c(CurrentYr, py[-length(py)]))) %>%
      reshape2::melt()

    g <- ggplot(dat_MP, aes(Year, value)) + geom_line(size = 1, aes(colour = MP)) +
      theme_bw() +
      coord_cartesian(xlim = range(ay)) +
      geom_vline(xintercept = CurrentYr, linetype = 4) +
      geom_hline(yintercept = 0, linetype = 2) +
      scale_colour_manual(values = MPcols %>% structure(names = MSEproj@MPs)) +
      labs(y = ylab) + ggtitle(title_txt)
  } else {
    dat_MP <- cbind(rep(hist[nyh], nMP), proj_med) %>%
      structure(dimnames = list(MP = MSEproj@MPs, Year = c(CurrentYr, py))) %>%
      reshape2::melt()

    g <- ggplot(dat_MP, aes(Year, value)) + geom_line(size = 1, aes(colour = MP)) +
      theme_bw() +
      coord_cartesian(xlim = range(ay), ylim = c(0, 1.2 * max_y)) +
      geom_vline(xintercept = CurrentYr, linetype = 4) +
      scale_colour_manual(values = MPcols %>% structure(names = MSEproj@MPs)) +
      labs(y = ylab) + ggtitle(title_txt)
  }

  if(type == "SSB0") {
    SSB0d <- data.frame(value = apply(MSEproj@RefPoint$Dynamic_Unfished$SSB0, 2, median),
                        Type = "Dynamic~SSB[0]",
                        Year = ay)
    SSB0a <- data.frame(value = apply(MSEproj@RefPoint$ByYear$SSB0, 2, median),
                        Type = "Equilibrium~SSB[0]",
                        Year = ay)
    SSB0i <- data.frame(value = rep(hist[1], length(ay)),
                        Type = "Initial~SSB[0]",
                        Year = ay)
    SSBhist <- data.frame(value = hist,
                          Type = "Historical~SSB",
                          Year = hy)
    SSB_out <- rbind(SSB0d, SSB0a, SSB0i, SSBhist)
    SSB_out$Type <- factor(SSB_out$Type,
                           levels = c("Historical~SSB", "Equilibrium~SSB[0]", "Initial~SSB[0]", "Dynamic~SSB[0]"))

    g <- g + geom_line(data = SSB_out, aes(linetype = Type, size = Type)) +
      geom_point(data = SSB_out, aes(shape = Type)) +
      scale_linetype_manual(name = "SSB Type", values = c(1, 2, 1, 3), labels = scales::label_parse()) +
      scale_shape_manual(name = "SSB Type", values = c(NA_integer_, 1, 16, 4), labels = scales::label_parse()) +
      scale_size_manual(name = "SSB Type", values = c(2, 0.5, 0.5, 0.5), labels = scales::label_parse())


  } else if(type == "SSBMSY") {
    SSBhist <- data.frame(value = hist,
                          Type = "Historical~SSB",
                          Year = hy)
    SSBMSY <- data.frame(value = apply(MSEproj@RefPoint$ByYear$SSBMSY, 2, median),
                         Type = "SSB[MSY]",
                         Year = ay)
    SSB_out <- rbind(SSBhist, SSBMSY)

    g <- g + geom_line(data = SSB_out, aes(linetype = Type, size = Type)) +
      geom_point(data = SSB_out, aes(shape = Type)) +
      scale_linetype_manual(name = "SSB Type", values = c(1, 3), labels = scales::label_parse()) +
      scale_shape_manual(name = "SSB Type", values = c(NA_integer_, 4), labels = scales::label_parse()) +
      scale_size_manual(name = "SSB Type", values = c(2, 0.5), labels = scales::label_parse())

  } else if(type == "SP") {
    SP <- data.frame(value = hist, Year = hy)
    g <- g + geom_line(data = SP, size = 2)
  } else if(type == "F") {
    FMSY <- data.frame(value = apply(MSEproj@RefPoint$ByYear$FMSY, 2, median),
                       Type = "F[MSY]",
                       Year = ay)
    Fhist <- data.frame(value = hist,
                        Type = "Historical~F",
                        Year = hy)
    F_out <- rbind(Fhist, FMSY)
    F_out$Type <- factor(F_out$Type, levels = c("Historical~F", "F[MSY]"))

    g <- g + geom_line(data = F_out, aes(linetype = Type, size = Type)) +
      geom_point(data = F_out, aes(shape = Type)) +
      scale_linetype_manual(name = "F Type", values = c(1, 3), labels = scales::label_parse()) +
      scale_shape_manual(name = "F Type", values = c(NA_integer_, 4), labels = scales::label_parse()) +
      scale_size_manual(name = "F Type", values = c(2, 0.5), labels = scales::label_parse())
  } else if(type == "SPR") {
    SPR <- data.frame(value = hist, Year = hy)
    g <- g + geom_line(data = SPR, size = 2)
  } else {
    Catch <- data.frame(value = hist, Year = hy)
    g <- g + geom_line(data = Catch, size = 2)
  }

  return(g)
}

#' Make a performance metric
#'
#' Function that defines and returns performance measures.
#' @param x An object of class \linkS4class{MSE}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEproj} which is
#' the MSE object.
#' @param type The response variable for the performance metric.
#' @param frac The reference value for calculating probability of exceeding this value.
#' @param year_range The length two vector for the range of years (relative to OM@@CurrentYr)
#' during which to calculate the performance measure.
#' @param label Optional character that describes the performance measure.
#' @param ... Additional arguments depending on \code{type}. See details.
#'
#' @details
#' \code{type = "SSB"} calculates SSB relative to historical SSB. Provide the reference year with argument \code{SSBhist_yr}.
#'
#' \code{type = "SSB0"} calculates SSB relative to SSB0 (either "Dynamic", "Initial", or "Equilibrium").
#' Provide the type with argument \code{SSB0_type}.
#'
#' \code{type = "SSBMSY"} calculates SSB relative to SSBMSY.
#'
#' \code{type = "F"} calculates F relative to FMSY.
#'
#' \code{type = "SPR"} calculates SPR relative to frac.
#'
#' \code{type = "SSBMSY"} calculates SSB relative to historical SSB. Provide the reference year with argument \code{Chist_yr}.
#'
#' @examples
#' \donttest{
#' library(MSEtool)
#' MSE <- MSEtool::runMSE(OM = MSEtool::testOM, MPs = c("NFref", "FMSYref"))
#' PM <- make_PMobj(MSE, type = "SSB", frac = 0.75, year_range = 51:100, SSBhist_yr = 50)
#' }
#' @return A \linkS4class{PMobj} object.
#' @export
make_PMobj <- function(x, type = c("SSB", "SSB0", "SSBMSY", "F", "SPR", "Catch"),
                       frac = 0.4, year_range, label, ...) {
  type <- match.arg(type)
  dots <- list(...)

  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
  } else {
    MSEproj <- x
  }

  CurrentYr <- MSEproj@OM$CurrentYr[1]

  nyp<-MSEproj@proyears
  nyh<-MSEproj@nyears
  pyind<-nyh+(1:nyp)
  py<-CurrentYr + 1:nyp

  yind <- match(year_range[1]:year_range[2], py)

  if(type == "SSB") {
    xout <- MSEproj@SSB
    ref <- MSEproj@SSB_hist[, match(dots$SSBhist_yr, seq(CurrentYr - nyh + 1, CurrentYr))]
    caption <- paste0("Probability~SSB/SSB[", dots$SSBhist_yr, "]>", frac)
  } else if(type == "SSB0") {
    xout <- MSEproj@SSB

    # Identical regardless of MP, but make same shape as SSB
    ref <- switch(dots$SSB0_type,
                  "Equilibrium" = array(MSEproj@RefPoint$ByYear$SSB0[, pyind], dim(xout)[c(1, 3 ,2)]) %>%
                    aperm(c(1, 3, 2)),
                  "Initial" = MSEproj@SSB_hist[, 1],
                  "Dynamic" = array(MSEproj@RefPoint$Dynamic_Unfished$SSB0[, pyind], dim(xout)[c(1, 3, 2)]) %>%
                    aperm(c(1, 3, 2))
                  )

    caption <- paste0("Probability~'SSB'~'/'~'", dots$SSB0_type, "'~SSB[0]>", frac)

    #SSB0d <- array(MSEproj@RefPoint$Dynamic_Unfished$SSB0[, pyind], dim(SSB)[c(1, 3, 2)]) %>% aperm(c(1, 3, 2))
    #SSB0a <- array(MSEproj@RefPoint$ByYear$SSB0[, pyind], dim(SSB)[c(1, 3 ,2)]) %>% aperm(c(1, 3, 2))
    #SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[, nyh], dim(SSB0a))
    #SSB0i <- array(MSEproj@SSB_hist[, 1], dim(SSB0a))
  } else if(type == "SSBMSY") {
    xout <- MSEproj@SSB
    ref <- MSEproj@RefPoint$SSBMSY[, , pyind, drop = FALSE]
    caption <- paste0("Probability~SSB/SSB[MSY]>", frac)
  } else if(type == "F") {
    xout <- MSEproj@RefPoint$FMSY[, , pyind, drop = FALSE]/MSEproj@FM
    ref <- 1
    caption <- paste0("Probability~F/F[MSY]<", frac)
  } else if(type == "SPR") {
    xout <- MSEproj@SPR$Equilibrium
    ref <- 1
    caption <- paste0("Probability~SPR>", frac)
  } else {
    xout <- MSEproj@Catch
    ref <- MSEproj@CB_hist[, match(dots$Chist_yr, seq(CurrentYr - nyh + 1, CurrentYr))]
    caption <- paste0("Probability~C/C[", dots$Chist_yr, "]>", frac)
  }

  if(missing(label)) label <- caption %>% strsplit("~") %>% getElement(1) %>% paste(collapse = " ")

  PM <- new("PMobj")
  PM@Name <- label
  PM@Caption <- caption
  PM@Stat <- xout/ref
  PM@Ref <- frac
  PM@Prob <- apply(PM@Stat[, , yind, drop = FALSE] > PM@Ref, c(1, 2), mean)
  PM@Mean <- apply(PM@Stat[, , yind, drop = FALSE] > PM@Ref, 2, mean)
  PM@MPs <- MSEproj@MPs

  return(PM)
}


#' @rdname plot-MSE
#' @details \code{prob_plot} generates either a figure or a table of performance metrics.
#' @param PM_list A list of performance metrics (PM), preferably generated by \link{PM_obj}. If \code{figure = TRUE},
#' only the first PM in the list will be plotted.
#' @param xlim The year range over which to plot annual probabilities of the performance measure if \code{figure = TRUE}.
#' @param ylim The y-axis range over which to plot annual probabilities of the performance measure if \code{figure = TRUE}.
#' @param figure Logical, whether to plot the performance measure (TRUE) or return a matrix (FALSE).
#' @export
prob_plot <- function(x, PM_list = list(), xlim = NULL, ylim = NULL, figure = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
  } else {
    MSEproj <- x
  }

  nMP<-MSEproj@nMPs
  CurrentYr <- MSEproj@OM$CurrentYr[1]
  MPcols <- rainbow(nMP,start=0.2,end=1)

  # Probability plots
  nyp<-MSEproj@proyears
  py<-CurrentYr + 1:nyp
  if(figure) {

    if(missing(xlim) || is.null(xlim) || all(!xlim)) xlim <- range(py)
    if(missing(ylim) || is.null(ylim) || all(!ylim)) ylim <- c(0, 1.05)

    # PM_list is either length one or two
    g <- lapply(PM_list, function(x) {
      dat <- apply(x@Stat > x@Ref, c(2, 3), mean) %>%
        structure(dimnames = list(MP = MSEproj@MPs, Year = py)) %>%
        reshape2::melt(value.name = "Probability")

      ggplot(dat, aes(Year, Probability, colour = MP)) + geom_line(size = 1) +
        theme_bw() +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        scale_colour_manual(values = MPcols %>% structure(names = MSEproj@MPs)) +
        ggtitle(parse(text = x@Caption))
    })
    if(length(PM_list) == 1) return(g[[1]])

  } else {
    prob <- sapply(PM_list, function(x) x@Mean) %>% matrix(nrow = nMP) %>%
      structure(dimnames = list(MSEproj@MPs, rep("Probability", length(PM_list))))
    return(prob)
  }
}

#' @rdname plot-MSE
#' @details \code{stoch_plot} generates median trajectories with confidence intervals.
#' @param MPstoch A character vector of MPs to plot.
#' @param qval The quantile of the confidence interval to plot.
#' @importFrom ggpubr ggarrange
#' @export
stoch_plot <- function(x, MPstoch, qval = 0.9, type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch")) {
  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
  } else {
    MSEproj <- x
  }
  type <- match.arg(type)

  qval <- max(0.01, qval)
  qval <- min(0.99, qval)
  type <- match.arg(type)

  nMP<-MSEproj@nMPs
  CurrentYr <- MSEproj@OM$CurrentYr[1]
  MPcols <- rainbow(nMP,start=0.2,end=1,alpha = 0.3)
  MPlabcols <- rainbow(nMP,start=0.2,end=1)

  MPind<-match(MPstoch,MSEproj@MPs)

  # Probability plots
  nyp<-MSEproj@proyears
  nyh<-MSEproj@nyears
  pyind<-nyh+(1:nyp)
  py<-CurrentYr + 1:nyp

  if(type == "SSB0") {
    SSB<-MSEproj@SSB
    # Identical regardless of MP, but make same shape as SSB
    SSB0d <- array(MSEproj@RefPoint$Dynamic_Unfished$SSB0[, pyind], dim(SSB)[c(1, 3, 2)]) %>% aperm(c(1, 3, 2))
    SSB0a <- array(MSEproj@RefPoint$ByYear$SSB0[, pyind], dim(SSB)[c(1, 3 ,2)]) %>% aperm(c(1, 3, 2))

    SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[, nyh], dim(SSB0a))
    SSB0i <- array(MSEproj@SSB_hist[, 1], dim(SSB0a))

    refs <- list(SSB0i,SSB0an,SSB0a,SSB0d)
    nrr<-length(refs)
    ylabs <- paste0("SSB/", c("Initial", paste0("Equilibrium~(", CurrentYr, ")"), "Equilibrium", "Dynamic"), "~SSB[0]")

    g <- lapply(1:length(refs), function(rr) {
      Stoch_plot_int(x=SSB,ref=refs[[rr]],ylab=ylabs[rr],py=py,MPcols=MPcols,
                     MPlabcols=MPlabcols,MPind=MPind,qval=qval,MPs = MPstoch)
    })
    g$ncol <- g$nrow <- 2
    g$legend <- "right"
    g$common.legend <- TRUE
    do.call(ggpubr::ggarrange, g)

    ## Plot SSB as well?

  } else if(type == "SSBMSY") {

    SSB<-MSEproj@SSB
    refs <- list(1, MSEproj@RefPoint$SSBMSY[, , pyind, drop = FALSE])
    ylabs <- c("SSB", "SSB/SSB[MSY]")

    g <- lapply(1:length(refs), function(rr) {
      Stoch_plot_int(x=SSB,ref=refs[[rr]],ylab=ylabs[rr],py=py,MPcols=MPcols,
                     MPlabcols=MPlabcols,MPind=MPind,qval=qval,MPs = MPstoch)
    })
    g$ncol <- 2
    g$nrow <- 1
    g$legend <- "right"
    g$common.legend <- TRUE
    do.call(ggpubr::ggarrange, g)

  } else if(type == "SP") {
    B <- apply(MSEproj@Misc$extended$B, c(1, 3, 4), sum)[, MPind, , drop = FALSE]
    Removals <- apply(MSEproj@Misc$extended$Removals, c(1, 3, 4), sum)[, MPind, , drop = FALSE]
    SP <- B[, , nyh + 2:nyp, drop = FALSE] - B[, , nyh + 2:nyp - 1, drop = FALSE] + Removals[, , nyh + 2:nyp - 1, drop = FALSE]

    Stoch_plot_int(x = SP, ref = 1, ylab = "Surplus~production", py = py[-length(py)], MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval, MPs = MPstoch, ylim = expression(range(qs))) +
      geom_hline(yintercept = 0, linetype = 2)

  } else if(type == "F") {

    FM <- MSEproj@FM
    refs <- list(1, MSEproj@RefPoint$FMSY[, , pyind, drop = FALSE])
    ylabs <- c("Fishing~mortality", "F/F[MSY]")

    g <- lapply(1:length(refs), function(rr) {
      Stoch_plot_int(x=FM,ref=refs[[rr]],ylab=ylabs[rr],py=py,MPcols=MPcols,
                     MPlabcols=MPlabcols,MPind=MPind,qval=qval,MPs = MPstoch)
    })
    g$ncol <- 2
    g$nrow <- 1
    g$legend <- "right"
    g$common.legend <- TRUE
    do.call(ggpubr::ggarrange, g)

  } else if(type == "SPR") {
    Stoch_plot_int(x = MSEproj@SPR$Equilibrium, ref = 1, ylab = "Equilibrium~SPR", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval, MPs = MPstoch)
  } else {
    Stoch_plot_int(x = MSEproj@Catch, ref = 1, ylab = "Catch", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval, MPs = MPstoch)
  }
}


Stoch_plot_int <- function(x, ref = 1, ylab, py, MPcols, MPlabcols, MPind, qval, MPs, ylim = expression(c(0, 1.1 * max(qs)))) {
  q <- c(0.5 * (1 - qval), 0.5, qval + 0.5 * (1 - qval))
  qs <- apply(x/ref, 2:3, quantile, q)

  meds <- structure(qs[2, MPind, , drop = FALSE], dimnames = list(Type = "Median", MP = MPs, Year = py)) %>%
    reshape2::melt()
  lower <- structure(qs[1, MPind, , drop = FALSE], dimnames = list(Type = "Lower", MP = MPs, Year = py)) %>% reshape2::melt()
  upper <- structure(qs[3, MPind, length(py):1, drop = FALSE], dimnames = list(Type = "Upper", MP = MPs, Year = rev(py))) %>% reshape2::melt()

  ggplot(rbind(lower, upper), aes(Year, value)) +
    geom_line(size = 2, data = meds, aes(colour = MP)) +
    geom_polygon(aes(fill = MP, group = MP)) +
    theme_bw() +
    scale_fill_manual(values = MPcols[MPind]) +
    scale_colour_manual(values = MPlabcols[MPind]) +
    coord_cartesian(xlim = range(py), ylim = eval(ylim)) +
    labs(y = parse(text = ylab))
}

#' @rdname plot-MSE
#' @details \code{hist_sim} plots individual simulations (up to three).
#' @param MP Character, name of MP to plot.
#' @param sims Numeric vector (up to length three) indicating the simulations to plot.
#' @export
hist_sim <- function(x, MSEhist, MP, sims, type = c("SSB0", "SSBMSY", "SP", "F", "SPR", "Catch")) {
  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
    MSEhist <- x$MSEhist
  } else {
    MSEproj <- x
  }
  type <- match.arg(type)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  if(missing(MP)) MP <- MSEproj@MPs[1]
  if(missing(sims)) sims <- 1:max(MSEproj@nsim, 3)

  proyears<-MSEproj@proyears
  nyears<-MSEproj@nyears
  cols<-c('red','blue','black')
  CurrentYr <- MSEproj@OM$CurrentYr[1]

  yrs<-CurrentYr+(-nyears:(proyears-1))
  MPind<-match(MP,MSEproj@MPs)

  if(type == "SSB0") {

    SSB<-cbind(MSEproj@SSB_hist[sims, , drop = FALSE], MSEproj@SSB[, MPind, ][sims, , drop = FALSE])

    SSB0d<-MSEproj@RefPoint$Dynamic_Unfished$SSB0[sims, , drop = FALSE]
    SSB0a<-MSEproj@RefPoint$ByYear$SSB0[sims, , drop = FALSE]
    SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[sims, nyears], dim(SSB0a))
    SSB0i<-SSB[,1]

    par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
    layout(matrix(c(1,1,2,3,4,4,5,6,6),nrow=3,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1,1))

    matplot(yrs,t(SSB0d),col=cols,lty=3,type='o',pch=4,yaxs='i',ylim=c(0,max(SSB0d)),ylab="Spawning stock biomass (SSB)")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
    #matlines(yrs,t(SSB0a),col=cols,lty=2, pch = 1, typ = 'o')
    matlines(yrs,t(SSB),col=makeTransparent(cols,80),type='l',lty=1,lwd=5)
    matlines(yrs, matrix(SSB0i, length(yrs), length(sims), byrow = TRUE), lty = 1, col=cols)
  } else if(type == "SSBMSY") {

    SSB <- cbind(MSEproj@SSB_hist[sims, , drop = FALSE], MSEproj@SSB[, MPind, ][sims, , drop = FALSE])
    SSBMSY <- MSEproj@RefPoint$ByYear$SSBMSY[sims, , drop = FALSE]

    par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
    layout(matrix(c(1,1,2,3,3,4),nrow=2,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1.5))

    matplot(yrs,t(SSBMSY),col=cols,lty=3,type='o',pch=4,yaxs='i',ylim=c(0,max(SSB, SSBMSY)),ylab="Spawning stock biomass (SSB)")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
    matlines(yrs,t(SSB),col=makeTransparent(cols,80),type='l',lty=1,lwd=5)
  } else if(type == "SP") {

    layout(matrix(1:2, nrow = 1), widths = c(0.8, 0.2))

    B <- apply(MSEproj@Misc$extended$B, c(1, 3, 4), sum)[, MPind, ][sims, , drop = FALSE]
    Removals <- apply(MSEproj@Misc$extended$Removals, c(1, 3, 4), sum)[, MPind, ][sims, , drop = FALSE]

    matplot(yrs[-length(yrs)], t(B[, 2:length(yrs)] - B[, 2:length(yrs) - 1] + Removals[, 2:length(yrs) - 1]),
            col = makeTransparent(cols, 80), lty = 1, type = 'l', lwd = 5, pch = 4, yaxs = "i",
            xlab = "Year", ylab = "Surplus production")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
    abline(h = 0, lty = 2)

  } else if(type == "F") {

    FM <- cbind(MSEproj@FM_hist[sims, , drop = FALSE], MSEproj@FM[, MPind, ][sims, , drop = FALSE])
    FMSY <- MSEproj@RefPoint$ByYear$FMSY[sims, , drop = FALSE]

    par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
    layout(matrix(c(1,1,2,3,3,4),nrow=2,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1.5))

    matplot(yrs,t(FMSY),col=cols,lty=3,type='o',pch=4,yaxs='i',ylim=c(0, max(FM, FMSY)),
            ylab="Fishing mortality")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
    matlines(yrs,t(FM),col=makeTransparent(cols,80),type='l',lty=1,lwd=5)
  } else if(type == "SPR") {

    SPR <- cbind(MSEhist@TSdata$SPR$Equilibrium[sims, , drop = FALSE],
                 MSEproj@SPR$Equilibrium[, MPind, ][sims, , drop = FALSE])

    layout(matrix(1:2, nrow = 1), widths = c(0.8, 0.2))

    matplot(yrs,t(SPR),col=makeTransparent(cols,80),lty=1,type='l',lwd=5,yaxs='i',ylim=c(0,1),
            ylab="Equilibrium SPR", xlab = "Year")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  } else {

    Catch <- cbind(MSEproj@CB_hist[sims, , drop = FALSE],
                   MSEproj@Catch[, MPind, ][sims, , drop = FALSE])

    layout(matrix(1:2, nrow = 1), widths = c(0.8, 0.2))

    matplot(yrs,t(Catch),col=makeTransparent(cols,80),lty=1,type='l',lwd=5,yaxs='i',ylim=c(0, max(Catch)),
            ylab="Catch", xlab = "Year")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  }

  plot(1,1,typ='n',axes=F,xlab="",ylab="")

  if(type == "SSB0") {
    legend('topleft',legend=c(expression(SSB), expression(Dynamic~SSB[0]), expression(Initial~SSB[0])),
           lty=c(1,3,1,1),lwd=c(3,1,1,1),pch=c(NA,4,NA),col=c("#99999995",'black','black','black'),bty='n')
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')

    matplot(yrs,t(SSB/SSB0i),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSB0i)),
            ylab=expression(SSB/Initial~SSB[0]))
    abline(h=pretty(c(0,1.1 * max(SSB/SSB0i)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

    matplot(yrs,t(SSB/SSB0d),col=cols,lty=2,type='l',yaxs='i',ylim=c(0, 1.1),ylab=expression(SSB/Dynamic~SSB[0]))
    abline(h=seq(0,1,length.out=6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

    matplot(yrs,t(SSB/SSB0a),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSB0a)),
            ylab=expression(SSB/Equilibrium~SSB[0]))
    abline(h=pretty(c(0,1.1 * max(SSB/SSB0a)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

    matplot(yrs,t(SSB/SSB0an),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSB0an)),
            ylab=parse(text = paste0("SSB/Equilibrium~(", CurrentYr, ")~SSB[0]")))
    abline(h=pretty(c(0,1.1 * max(SSB/SSB0an)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

  } else if(type == "SSBMSY") {

    legend('topleft',legend=c(expression(SSB), expression(SSB[MSY])),
           lty=c(1,3),lwd=c(3,1),pch=c(NA,4),col=c("#99999995",'black'),bty='n')
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')

    matplot(yrs,t(SSB/SSBMSY),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSBMSY)),
            ylab=expression(SSB/SSB[MSY]))
    abline(h=pretty(c(0,1.1 * max(SSB/SSBMSY)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')
  } else if(type == "F") {

    legend('topleft',legend=c(expression(F), expression(F[MSY])),
           lty=c(1,3),lwd=c(3,1),pch=c(NA,4),col=c("#99999995",'black'),bty='n')
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')

    matplot(yrs,t(FM/FMSY),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(FM/FMSY)),
            ylab=expression(F/F[MSY]))
    abline(h=pretty(c(0,1.1 * max(F/FMSY)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')
  } else {
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')
  }

}

#' @rdname plot-MSE
#' @details \code{lollipop_plot} plots all performance measures for all MPs.
#' @param PM_list A list of \linkS4class{PMobj} objects, preferably, generated by \link{make_PMobj}.
#' @importFrom dplyr bind_rows
#' @export
lollipop_plot <- function(x, PM_list) {
  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
  } else {
    MSEproj <- x
  }
  probs <- Map(function(x, y) {
    out <- data.frame(MP = x@MPs)
    out[["Probability"]] <- x@Mean
    out[["PM"]] <- y
    return(out)
  }, x = PM_list, y = names(PM_list)) %>% bind_rows()
  probs$MP <- factor(probs$MP, levels = MSEproj@MPs)

  MPcols <- rainbow(MSEproj@nMPs, start = 0.2, end = 1) %>%
    structure(names = MSEproj@MPs)

  ggplot(probs, aes(PM, Probability)) +
    geom_linerange(position = position_dodge(width = 0.6), aes(colour = MP, ymin = 0, ymax = Probability)) +
    geom_point(position = position_dodge(width = 0.6), size = 2, shape = 21, aes(fill = MP)) +
    theme_bw() +
    scale_colour_manual(values = MPcols) +
    scale_fill_manual(values = MPcols) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Performance metric")
}

#' @rdname plot-MSE
#' @details \code{tradeoff_plot} plots a comparison of two performance measures for all MPs.
#' @param PMx A \linkS4class{PMobj} object to show on the x-axis.
#' @param PMy A \linkS4class{PMobj} object to show on the y-axis.
#' @param xlab Name of the performance metric on the x-axis.
#' @param ylab Name of the performance metric on the y-axis.
#' @importFrom ggrepel geom_text_repel
#' @export
tradeoff_plot <- function(x, PMx, PMy, xlab, ylab) {
  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
    MSEhist <- x$MSEhist
  } else {
    MSEproj <- x
  }
  nMP<-MSEproj@nMPs
  MPcols <- rainbow(nMP,start=0.2,end=1, alpha = 0.5)

  out <- data.frame(x = PMx@Mean, y = PMy@Mean, MP = factor(MSEproj@MPs, levels = MSEproj@MPs))

  ggplot(out, aes(x, y)) +
    geom_abline(intercept = 0, slope = 1, linetype = 3) +
    geom_point(size = 4, shape = 21, aes(fill = MP)) +
    ggrepel::geom_text_repel(aes(label = MP)) +
    theme_bw() +
    labs(x = xlab, y = ylab) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_fill_manual(values = structure(MPcols, names = MSEproj@MPs))
}


#' @rdname plot-MSE
#' @details \code{radar_plot} plots all performance measures for all MPs, where each metric is plotted along the vertex of a polygon.
#' @param ... Additional arguments to \link[ggspider]{spider_web}.
#' @param PM_list A list of \linkS4class{PMobj} objects, preferably, generated by \link{make_PMobj}.
#' @importFrom ggspider spider_web
#' @export
radar_plot <- function(x, PM_list, ...) {
  if(inherits(x, "reactivevalues")) {
    MSEproj <- x$MSEproj
  } else {
    MSEproj <- x
  }
  pm_df <- Map(function(x, y) {
    out <- data.frame(MP = x@MPs)
    out[[y]] <- x@Mean
    return(out)
  }, x = PM_list, y = names(PM_list)) %>% Reduce(dplyr::left_join, .)
  pm_df$MP <- factor(pm_df$MP, levels = MSEproj@MPs)
  custom_pal <- rainbow(MSEproj@nMPs, start = 0.2, end = 1) %>% structure(names = MSEproj@MPs)

  x <- reshape2::melt(pm_df, id.vars = "MP", value.name = "prob",
                      variable.name = "pm")
  suppressMessages({
    g <- ggspider::spider_web(x, "MP", "pm", "prob", leg_main_title = "MP",
                              leg_lty_title = "MP type", palette = "Set2", ...) +
      scale_color_manual(name = "MP", values = custom_pal) +
      guides(linetype = "none")
  })
  return(g)
}

