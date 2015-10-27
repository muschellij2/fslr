#' @title Orthographic Display, added options
#' @description Copy of \code{oro.nifti}'s \code{\link{orthographic}} function 
#' with some tweaks such as adding L/R designations for left and right
#' @return NULL
#' @seealso \link{orthographic}
#' @param x is an object of class nifti or similar.
#' @param y is an object of class nifti or similar for the overlay.
#' @param xyz is the coordinate for the center of the crosshairs.
#' @param w is the time point to be displayed (4D arrays only).
#' @param col is grayscale (by default).
#' @param col.y is hotmetal (by default).
#' @param zlim is the minimum and maximum `z' values passed into image.
#' @param zlim.y is the minimum and maximum `z' values passed into image 
#' for the overlay.
#' @param crosshairs is a logical value for the presence of crosshairs 
#' in all three orthogonal planes (default = TRUE).
#' @param NA.x Set any values of 0 in \code{x} to \code{NA}
#' @param NA.y Set any values of 0 in \code{y} to \code{NA}
#' @param col.crosshairs is the color of the crosshairs (default = red).
#' @param xlab is set to "" since all margins are set to zero.
#' @param ylab is set to "" since all margins are set to zero.
#' @param axes is set to FALSE since all margins are set to zero.
#' @param oma is the size of the outer margins in the par function.
#' @param mar is the number of lines of margin in the par function.
#' @param bg is the background color in the par function.
#' @param text allows the user to specify text to appear in 
#' the fourth (unused) pane.
#' @param text.color is the color of the user-specified text 
#' (default = ``white").
#' @param text.cex is the size of the user-specified text (default = 2).
#' @param text.x x coordinate for text 
#' @param text.y y coordinate for text
#' @param add.orient (logical) Add left/right, A/P, etc. orientation
#' @param mfrow (numeric) layout of the 3 slices
#' @param breaks (numeric) breaks for x to passed to 
#' \code{\link[graphics]{image}}
#' @param ybreaks (numeric) breaks for y to passed to 
#' \code{\link[graphics]{image}}
#' @param addlegend (logical) add legend?
#' @param leg.x (numeric) x coord for legend
#' @param leg.y (numeric) y coord for legend
#' @param legend (character) legend text
#' @param leg.col (character) Colors for legend 
#' @param leg.title (character) title for legend 
#' @param leg.cex (numeric) \code{cex} for \code{\link{legend}}
#' @param window (vector) Length-2 vector to limit image to certain range
#' @param ycolorbar (logical) Should a colorbar for \code{y} be plotted
#' @param clabels Label for colorbar (see \code{\link{colorbar}})
#' @param add Should the y-plot be added or its own plot?  Used
#' in \code{double_ortho}
#' @param ... other arguments to the image function may be provided here.
#' @import scales
#' @export
ortho2 = function (x, y = NULL, xyz = NULL, w = 1, col = gray(0:64/64), 
                   col.y = hotmetal(), zlim = NULL, zlim.y = NULL, 
                   NA.x = FALSE,
                   NA.y = TRUE,
                   crosshairs = TRUE, 
                   col.crosshairs = "red", xlab = "", ylab = "", axes = FALSE, 
                   oma = c(0, 0, 0, ifelse(ycolorbar, 5, 0)), 
                   mar = rep(0, 4), bg = "black", text = NULL, 
                   text.color = "white", text.cex = 2, 
                   text.x=32,
                   text.y=32,
                   add.orient=TRUE,
                   mfrow=c(2,2), ybreaks = NULL, breaks=NULL,
                   addlegend = FALSE,
                   leg.x=32,
                   leg.y=32,
                   legend,
                   leg.col,
                   leg.title = NULL,
                   leg.cex,
                   window=NULL,
                   ycolorbar = FALSE,
                   clabels = TRUE,
                   add = TRUE,
                   ...) 
{
  if (!is.null(y)) {
    if (!all(dim(x)[1:3] == dim(y)[1:3])) {
      stop("dimensions of \"x\" and \"y\" must be equal")
    }
  }
  if (inherits(x, "nifti")){
    if (!is.null(window)) {
      x = window_img(x, window=window, replace="window")
#       x@cal_min = window[1]
#       x@cal_max = window[2]
#       x[ x < window[1] ] = window[1]
#       x[ x >= window[2] ] = window[2]
    }
  }
  X <- nrow(x)
  Y <- ncol(x)
  Z <- nsli(x)
  W <- ntim(x)
  mXY = max(X, Y)
  lr.shift = 4
  ud.shift = 6
  if (!is.null(y)){
    if (NA.y){
      y[ y == 0 ] = NA
      y = cal_img(y)
    }
  }
  if (NA.x){
    x[ x == 0 ] = NA
    x = cal_img(x)
  }
  if (is.null(xyz)) {
    xyz <- ceiling(c(X, Y, Z)/2)
  }
  if (X == 0 || Y == 0 || Z == 0) {
    stop("size of NIfTI volume is zero, nothing to plot")
  }  
  if (is.null(zlim)) {
    zlim <- c(x@cal_min, x@cal_max)
    if (any(!is.finite(zlim)) || diff(zlim) == 0) {
      zlim <- c(x@glmin, x@glmax)
    }
    if (any(!is.finite(zlim)) || diff(zlim) == 0) {
      zlim <- range(x, na.rm = TRUE)
    }
  }
  if (is.null(breaks)){
    breaks <- c(min(x, zlim, na.rm = TRUE), 
                seq(min(zlim, na.rm = TRUE),
                    max(zlim, na.rm = TRUE), 
                    length = length(col) - 1),
                max(x, zlim, na.rm = TRUE))
  }

  if (!is.null(y) && is.null(zlim.y)) {
    zlim.y <- c(y@cal_min, y@cal_max)
    if (max(zlim.y) == 0) {
      zlim.y <- c(x@glmin, x@glmax)
    }
  }
  oldpar <- par(no.readonly = TRUE)
  par(mfrow = mfrow, oma = oma, mar = mar, bg = bg)
  
  pdim = x@pixdim
  if (!is.na(W)) {
    if (w < 1 || w > W) {
      stop("volume \"w\" out of range")
    }
    x = x[, , , w]
  }
  graphics::image(1:X, 1:Z, x[, xyz[2], ], col = col, zlim = zlim, 
                  breaks = breaks, asp = pdim[4]/pdim[2], xlab = ylab, 
                  ylab = xlab, axes = axes, ...)
  if (!add & crosshairs) {
    abline(h = xyz[3], v = xyz[1], col = col.crosshairs)
  }
  if (!is.null(y)) {
    if (inherits(y, "nifti") | inherits(y, "anlz")){
      class(y@.Data) == "numeric"
    }
    
    if (is.null(ybreaks)){
      graphics::image(1:X, 1:Z, y[, xyz[2], ], col = col.y, 
                    zlim = zlim.y, add = add,
                    asp = ifelse(add, NA, pdim[4]/pdim[2]),
                    axes = axes
                    )
    } else {
      graphics::image(1:X, 1:Z, y[, xyz[2], ], col = col.y, 
                      zlim = zlim.y, add = add, breaks = ybreaks,
                      asp = ifelse(add, NA, pdim[4]/pdim[2]),
                      axes = axes
                      )
    }
  }
  if (crosshairs) {
    abline(h = xyz[3], v = xyz[1], col = col.crosshairs)
  }
  if (add.orient){
    text("L", x = X + lr.shift, y = Z/2, las = 1, col="white")
    text("R", x = -lr.shift, y = Z/2, las = 1, col="white")
    text("S", x = X/2-.5, y = Z-ud.shift, las = 1, col="white")
    text("I", x = X/2-.5, y = ud.shift, las = 1, col="white")
  }
  graphics::image(1:Y, 1:Z, x[xyz[1], , ], col = col, breaks = breaks, 
                  asp = pdim[4]/pdim[3], xlab = xlab, ylab = ylab, 
                  axes = axes, ...)
  if (!add & crosshairs) {
    abline(h = xyz[3], v = xyz[2], col = col.crosshairs)
  }
  if (!is.null(y)) {
    if (is.null(ybreaks)){
      graphics::image(1:Y, 1:Z, y[xyz[1], , ], col = col.y, 
                    zlim = zlim.y, add = add,
                    asp = ifelse(add, NA, pdim[4]/pdim[3]),
                    axes = axes
                    )
    } else {
      graphics::image(1:Y, 1:Z, y[xyz[1], , ], col = col.y, 
                      zlim = zlim.y, add = add, breaks=ybreaks,
                      asp = ifelse(add, NA, pdim[4]/pdim[3]),
                      axes = axes
                      )
    }
  }
  if (crosshairs) {
    abline(h = xyz[3], v = xyz[2], col = col.crosshairs)
  }
  if (add.orient){
    text("A", x = Y-1, y = Z/2, las = 1, col="white")
    text("P", x = 0+1, y = Z/2, las = 1, col="white")
    text("S", x = Y/2-.5, y = Z-ud.shift, las = 1, col="white")
    text("I", x = Y/2-.5, y = ud.shift, las = 1, col="white")
    #     
    #     mtext("A", side=4, las = 1, outer=FALSE, adj=0)
    #     mtext("P", side=2, las = 1, outer=FALSE, adj= 0, padj=0)
    #     mtext("S", side=3, las = 1, outer=FALSE)
    #     mtext("I", side=1, las = 1, outer=FALSE)
  }    
  graphics::image(1:X, 1:Y, x[, , xyz[3]], col = col, breaks = breaks, 
                  asp = pdim[3]/pdim[2], xlab = xlab, ylab = ylab, 
                  axes = axes, ...)
  if (!add & crosshairs) {
    abline(h = xyz[2], v = xyz[1], col = col.crosshairs)
  }
  if (!is.null(y)) {
    if (is.null(ybreaks)){
      graphics::image(1:X, 1:Y, y[, , xyz[3]], col = col.y, 
                      zlim = zlim.y, add = add,
                      asp = ifelse(add, NA, pdim[3]/pdim[2]),
                      axes = axes
                      )
    } else {
      graphics::image(1:X, 1:Y, y[, , xyz[3]], col = col.y, 
                      zlim = zlim.y, add = add, breaks = ybreaks,
                      asp = ifelse(add, NA, pdim[3]/pdim[2]),
                      axes = axes
                      )
    }
  }
  if (crosshairs) {
    abline(h = xyz[2], v = xyz[1], col = col.crosshairs)
  }
  if (add.orient){
    text("L", x = X + lr.shift, y = Y/2, las = 1, col="white")
    text("R", x = -lr.shift, y = Y/2, las = 1, col="white")
    text("A", x = X/2-.5, y = Y-ud.shift, las = 1, col="white")
    text("P", x = X/2-.5, y = ud.shift, las = 1, col="white")
    
    #     mtext("L", side=4, las = 1, outer=FALSE, adj=0)
    #     mtext("R", side=2, las = 1, outer=FALSE, adj= 0, padj=0)
    #     mtext("A", side=3, las = 1, outer=FALSE)
    #     mtext("P", side=1, las = 1, outer=FALSE)
  }    
  
  if (!is.null(text) | addlegend) {
    suppressWarnings({
      graphics::image(1:64, 1:64, matrix(NA, 64, 64), xlab = "", 
                    ylab = "", axes = FALSE, ...)
    })
    if (addlegend) {
      legend(x = leg.x, y= leg.y, 
           legend=legend, 
           pch=rep(15, length(leg.col)),
           col=leg.col, 
           cex = leg.cex, 
           text.col = "white",
           title = leg.title)    
    }
    if (!is.null(text)){
      text(labels = text, x=text.x, y=text.y, col = 
             text.color, cex = text.cex)
    }    
  }

  par(oldpar)
  if (!is.null(y)) {
    if (is.null(ybreaks)){
      if (ycolorbar){
        warning("colorbar not supported if ybreaks unspecified")
        #         nc <- length(col.y)
        #         if (diff(zlim.y) == 0) {
        #           zlim.y <- ifelse(zlim.y[1L] == 0, c(-1, 1), 
        #                 zlim.y[1L] + c(-0.4, 0.4) * abs(zlim.y[1L]))
        #         }
        #         zi = floor((nc - 1e-05) * y[, , xyz[3]] + 1e-07)
        #         breaks = unique(zi[zi >= 0 & zi < nc])
        #         
        #         colorbar(breaks=ybreaks, col=col.y, text.col="white")
      }
    } else {
      if (ycolorbar){
        colorbar(breaks=ybreaks, col=alpha(col.y, 1), 
                 text.col="white", 
                 labels = clabels)
      }
    }
  }
  invisible()
}


#' @title Add a colorbar to an ortho2 object
#'
#' @description Adds a series of colors mapped to a value
#' @param breaks a set of finite numeric breakpoints for the 
#' colours (see \code{\link{image}}
#' @param col a list of colors (see \code{\link{image}}
#' @param text.col axis and text label color
#' @param labels labels for tick marks - see \code{\link{axis}}
#' @param maxleft Extent the lefthand for colorbar
#' @note Much of this was taken from \code{vertical.image.legend} from
#' the \code{aqfig} package
#' @import graphics
#' @export
#' @return A plot
colorbar <- function (breaks, #the minimum and maximum z values for which 
                      # colors should be plotted (see \code{\link{image}})
                     col, # a list of colors (see \code{\link{image}})
                     text.col = "white", # axis and text label color
                     labels = TRUE,
                     maxleft = 0.95
                     ) {
  # taken from vertical.image.legend from package aqfig
  starting.par.settings <- par(no.readonly = TRUE)
  mai <- par("mai")
  fin <- par("fin")
  rat = mai[4]/fin[1]
  rat = max(rat, 1-maxleft)
  x.legend.fig <- c(1 - rat, 1)
  y.legend.fig <- c(mai[1]/fin[2], 1 - (mai[3]/fin[2]))
  x.legend.plt <- c(x.legend.fig[1] + (0.08 * (x.legend.fig[2] - 
                                                 x.legend.fig[1])), 
                    x.legend.fig[2] - (0.6 * (x.legend.fig[2] - 
                                                x.legend.fig[1])))
  y.legend.plt <- y.legend.fig
  cut.pts <- breaks
  z <- (cut.pts[1:length(col)] + cut.pts[2:(length(col) + 1)])/2
  par(new = TRUE, pty = "m", plt = c(x.legend.plt, y.legend.plt))
  image(x = 1, y = z, z = matrix(z, nrow = 1, ncol = length(col)), 
        col = col, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  if (isTRUE(labels)){
    at = NULL
  } else {
    at = z
  }
  axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis = 0.5, 
       tcl = -0.1, 
       labels = labels,
       at = at,
       col.axis = text.col,
       col = text.col)
  box()
  mfg.settings <- par()$mfg
  par(starting.par.settings)
  par(mfg = mfg.settings, new = FALSE)
}
