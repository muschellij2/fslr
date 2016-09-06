#' @title Create Multi-Image Plot with Overlays
#' @description Creates a multi-row or multi-column plot with image
#' slices and the potential for overlays as well.
#'
#' @param x List of images of class \code{nifti} or character vector of filenames
#' @param y List of images of class \code{nifti} or character vector of filenames.
#' Same length as \code{x}.
#' @param z Slice to display.
#' @param w 3D volume to display if \code{x} has 4-D elements
#' @param mask \code{nifti} image to drop empty image dimensions if wanted. 
#' Passed to \code{\link{dropEmptyImageDimensions}}
#' @param col.x Color to display x images
#' @param col.y Color to display y images
#' @param zlim.x Limits for \code{x} to plot
#' @param zlim.y Limits for \code{y} to plot
#' @param plane the plane of acquisition to be displayed
#' @param xlab Label for x-axis
#' @param ylab Label for y-axis
#' @param axes Should axes be displayed
#' @param direction Should images be a row or column? Ignored if 
#' \code{mfrow} is in \code{par.opts}
#' @param par.opts Options to pass to \code{\link{par}}
#' @param text Text to be displayed 
#' @param text.x Location of text in x-domain
#' @param text.y Location of text in y-domain
#' @param text.cex Multiplier for text font
#' @param text.col Color for \code{text} and \code{main}.
#' @param main Title for each plot
#' @param main.cex Multiplier for text font. Will default to \code{text.cex}
#' @param main.col Color for \code{main}. Will default to \code{text.col}
#' @param NA.x Should \code{0}'s in \code{x} be set to \code{NA}?
#' @param NA.y Should \code{0}'s in \code{y} be set to \code{NA}?
#' @param ... Additional arguments to pass to \code{\link[graphics]{image}}
#'
#' @return NULL
#' @export 
#' @examples \dontrun{
#' 
#'  if (require(brainR)) {
#'    visits = 1:3
#'    y = paste0("Visit_", visits, ".nii.gz")
#'    y = system.file(y, package = "brainR")
#'    y = lapply(y, readnii)
#' 
#'    y = lapply(y, function(r){
#'      pixdim(r) = c(0, rep(1, 3), rep(0, 4))
#'      dropImageDimension(r)
#'    })
#' 
#'    x = system.file("MNI152_T1_1mm_brain.nii.gz", 
#'                  package = "brainR")
#'    x = readnii(x)
#'    mask = x >0
#'    x = lapply(visits, function(tmp){
#'        x
#'    })
#'    alpha = function(col, alpha = 1) {
#'        cols = t(col2rgb(col, alpha = FALSE)/255)
#'        rgb(cols, alpha = alpha)
#'    }
#'    multi_overlay(x, y, 
#'          col.y = alpha(hotmetal(), 0.5),
#'          mask = mask, 
#'          main = paste0("\n", "Visit ", visits),
#'          text = LETTERS[visits],
#'          text.x = 0.9,
#'          text.y = 0.1,
#'          text.cex = 3)
#'  }
#' }
multi_overlay = function(x, 
                         y = NULL, 
                         z = NULL, 
                         w = 1, 
                         mask = NULL,
                         col.x = gray(0:64/64), 
                         col.y = hotmetal(), 
                         zlim.x = NULL, 
                         zlim.y = NULL, 
                         plane = c("axial", "coronal", 
                                   "sagittal"), 
                         xlab = "", 
                         ylab = "", axes = FALSE, 
                         direction = c("horizontal", "vertical"),
                         par.opts = list( 
                           oma = c(0, 0, 0, 0), 
                           mar = rep(0, 4), 
                           bg = "black"),
                         text = NULL,                      
                         text.x = NULL,
                         text.y = NULL,
                         text.cex = 1,
                         text.col = "white",
                         main = NULL,
                         main.col = text.col,
                         main.cex = text.cex,
                         NA.x = TRUE,
                         NA.y = TRUE,
                         ...) {
  
  relist = function(r){
    r = check_nifti(r)
    if ( !"list" %in% typeof(r)){
      r = list(r)
    }
    return(r)
  }
  all.x = relist(x)
  
  
  y_not_null = !is.null(y)
  if (y_not_null){
    all.y = relist(y)
    stopifnot(length(all.y) == length(all.x))
  }
  
  if (!is.null(mask)){
    mask = check_nifti(mask)
    o1 = dropEmptyImageDimensions(mask, other.imgs = all.x)
    all.x = o1$other.imgs
    if (y_not_null){
      o.y = dropEmptyImageDimensions(mask, other.imgs = all.y)
      all.y = o.y$other.imgs
    }
    mask = o1$outimg
  }
  
  direction = match.arg(direction, c("horizontal", "vertical"))
  
  oldpar <- par(no.readonly = TRUE)
  if (!"mfrow" %in% names(par.opts)) {
    if (direction == "horizontal"){
      par.opts$mfrow = c(1, length(all.x))
    }
    if (direction == "vertical"){
      par.opts$mfrow = c(length(all.x), 1)
    }
  }
  
  make_length = function(x){
    if (!is.null(x)){
      x = c(x, rep(x, length = length(all.x) - length(x)))
    }
    return(x)
  }
  text.cex = make_length(text.cex)
  text.x = make_length(text.x)
  text.y = make_length(text.y)
  text.col = make_length(text.col)
  main = make_length(main)
  main.col = make_length(main.col)
  main.cex = make_length(main.cex)
  
  do.call(par, par.opts)  
  for (i in seq_along(all.x)){
    x = all.x[[i]]
    
    switch(plane[1], axial = {
      aspect <- x@pixdim[3]/x@pixdim[2]
    }, coronal = {
      if (length(dim(x)) == 3) {
        x@.Data <- aperm(x, c(1, 3, 2))
      } else {
        x@.Data <- aperm(x, c(1, 3, 2, 4))
      }
      y@.Data <- aperm(y, c(1, 3, 2))
      aspect <- x@pixdim[4]/x@pixdim[2]
    }, sagittal = {
      if (length(dim(x)) == 3) {
        x@.Data <- aperm(x, c(2, 3, 1))
      } else {
        x@.Data <- aperm(x, c(2, 3, 1, 4))
      }
      y@.Data <- aperm(y, c(2, 3, 1))
      aspect <- x@pixdim[4]/x@pixdim[3]
    }, stop(paste("Orthogonal plane", plane[1], "is not valid.")))
    
    
    if (y_not_null) {
      y = all.y[[i]]
      if (!all(dim(x)[1:3] == dim(y)[1:3])) {
        stop("dimensions of \"x\" and \"y\" must be equal")
      }
      if (NA.y){
        y[ y == 0 ] = NA
        y = cal_img(y)
      }      
    }
    
    if (NA.x){
      x[ x == 0 ] = NA
      x = cal_img(x)
    }
    
    X <- nrow(x)
    Y <- ncol(x)
    Z <- nsli(x)
    W <- ntim(x)
    if (X == 0 || Y == 0 || Z == 0) {
      stop("size of NIfTI volume is zero, nothing to plot")
    }
    zlim.x = zlimmer(x, zlim.x)
    breaks.x <- c(min(x, zlim.x, na.rm = TRUE), 
                  seq(min(zlim.x, 
                          na.rm = TRUE), 
                      max(zlim.x, na.rm = TRUE),
                      length = length(col.x) - 
                        1), max(x, zlim.x, na.rm = TRUE))
    if (y_not_null){
      zlim.y = zlimmer(y, zlim.y)  
    }  
    
    if ( is.null(z) ) {
      z = floor( Z / 2 )
    }
    index <- z
    lz <- length(index)
    if (z < 1 || z > Z) {
      stop("slice \"z\" out of range")
    }
    xvals = 1:X
    yvals = 1:Y
    
    # standardize the plot sizes
    xvals = (xvals - 1)/(X-1)
    yvals = (yvals - 1)/(X-1) # std by x so aspect preserved
    if (is.na(W)) {
      for (z in index) {
        graphics::image(xvals, yvals, x[, , z], col = col.x, 
                        breaks = breaks.x, zlim = zlim.x, asp = aspect, 
                        axes = axes, xlab = xlab, ylab = ylab, ...)
        if (y_not_null){
          graphics::image(xvals, yvals, y[, , z], col = col.y, 
                          zlim = zlim.y, add = TRUE)
        }
      }
    } else {
      if (w < 1 || w > W) {
        stop("volume \"w\" out of range")
      }
      for (z in index) {
        graphics::image(xvals, yvals, x[, , z, w], col = col.x, 
                        breaks = breaks.x, zlim = zlim.x, asp = aspect, 
                        axes = axes, xlab = xlab, ylab = ylab, ...)
        if (y_not_null) {
          graphics::image(xvals, yvals, y[, , z], col = col.y, 
                          zlim = zlim.y, add = TRUE)
        }
      }
    }
    if (!is.null(main)) {
      # message("printing title")
      title(main = main[i], outer = FALSE, 
            col.main = main.col[i],
            cex.main = main.cex[i])
    }
    if (!is.null(text)) {
      # message("printing text")
      text(labels = text[i], x = text.x[i], y = text.y[i], 
           cex = text.cex[i], col = text.col[i])
    }
  }
  par(oldpar)    
  invisible(NULL)
}


