
#' @title FSL ROI 
#' @description This function calls \code{fslroi}
#' @param file (character) image to be manipulated
#' @param xmin Minimum index for x-dimension
#' @param xsize Size of ROI in x-dimension
#' @param ymin Minimum index for y-dimension
#' @param ysize Size of ROI in y-dimension
#' @param zmin Minimum index for z-dimension
#' @param zsize Size of ROI in z-dimension
#' @param tmin Minimum index for t-dimension
#' @param tsize Size of ROI in t-dimension
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is \code{TRUE} or 
#' \code{FALSE}.
#' @note Indexing (in both time and space) starts with 0 not 1! 
#' Inputting -1 for a size will set it to the full image extent for that dimension.
#' @export
fslroi = function(
  file,
  xmin = 0,
  xsize = -1,
  ymin = 0,
  ysize = -1,
  zmin = 0,
  zsize = -1,
  tmin = NULL,
  tsize = NULL,
  
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  verbose = TRUE,
  ...){
  
  ##################################
  # Check that if min is set then size must be set
  ##################################
  nuller = function(min, size, name) {
    
    if (is.null(min) != is.null(size)) {
      stop(paste0(name, "min set and ", name, 
                  "size not set, or vice versa! Failing..."))
    }
    if (!is.null(size)) {
      if (size == 0) {
        warning("Size of 0 may have bad side effects/fail")
      }
    }
  }
  nuller(xmin, xsize, "x")
  nuller(ymin, ysize, "y")
  nuller(zmin, zsize, "z")
  nuller(tmin, tsize, "t")
  
  xyz_null = c(
    is.null(xmin),
    is.null(xsize),
    is.null(ymin),
    is.null(ysize),
    is.null(zmin),
    is.null(zsize))  
  
#   xyzt_null = c(xyz_null, 
#                 is.null(tmin), is.null(tsize)
#   )
  ###################
  # If not all xyz set and tmin/tsize set, assume it's using t!
  ###################
  if (!is.null(tmin) & !is.null(tsize)) {
    if (any( xyz_null )) {
      if (!all( xyz_null )) {
        warning(paste0("x/y/z min/size are disregarded because not all set", 
                       " and t only used!"))
      }
      opts = paste(tmin, tsize, sep = " ")
    } else {
      opts = paste(xmin, xsize, ymin, ysize, 
                   zmin, zsize, tmin, tsize, sep = " ")
    }
  } else {
    
    ###################
    # If t is not set (tsize would have failed before)
    ###################  
    if (any(xyz_null)) {
      stop("Not all xyz are set!")
    }
    opts = paste(xmin, xsize, ymin, ysize, zmin, zsize, sep = " ")
  }
  
  
  
  res = fslcmd("fslroi", 
               file = file, 
               outfile = outfile, retimg = retimg,
               reorient = reorient, intern = intern, opts = opts, 
               ... = ..., verbose = verbose, samefile = FALSE,
               opts_after_outfile = TRUE)
  
  return(res)  
}

#' @rdname fslroi
#' @export
fslroi_time = function(
  file,
  tmin = NULL,
  tsize = NULL,
  ...) {
  ret = fslroi(
    file,
    xmin = 0,
    xsize = -1,
    ymin = 0,
    ysize = -1,
    zmin = 0,
    zsize = -1,
    tmin = tmin,
    tsize = tsize,
    ...
  )
  return(ret)
}