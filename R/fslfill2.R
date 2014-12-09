#' @title Fill image holes with dilation then erosion
#' @description This function calls \code{fslmaths} to dilate an image, then calls
#' it again to erode it.   
#' @param file (character) filename of image to be filled
#' @param outfile (character) name of resultant filled file
#' @param kopts (character) Options passed for kernel before erosion/dilation
#' @param remove.ends (logical) Remove top and bottom dilation.  
#' @param refill (logical) Run \code{\link{fslfill}} after dilation/erosion.
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) pass to \code{\link{system}}
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @note This function binarizes the image before running.
#' @export
fslfill2 = function(file, 
                   outfile = NULL, 
                   kopts= "",
                   remove.ends = TRUE,
                   refill = TRUE,
                   retimg = FALSE,
                   reorient = FALSE,
                   intern=TRUE, verbose = TRUE,
                   ...){
  have.outfile = FALSE
  
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
      have.outfile = TRUE
    }
  } else {
    stopifnot(!is.null(outfile))
  }  
  outfile = nii.stub(outfile)
  
  temp.img = tempfile()
  bin = fslbin(file=file, outfile = temp.img, retimg=TRUE)
  dimg = dim(bin)
  ind = which(bin >0, arr.ind=TRUE)
  ind = ind[ ind[, "dim3"] %in% c(1, dimg[3]),]
  nind = nrow(ind)
  
  #### inverting, eroding (equivalent to dilation), then invert back
  opts = paste0("-mul -1 -add 1 ", kopts, " -ero -mul -1 -add 1")
  fslmaths(file = temp.img,
                 outfile = outfile,
                 opts = opts, 
                 retimg=FALSE)
  dil = fslerode(file = outfile,
                   outfile = outfile,
                   kopts = kopts, 
                   retimg=TRUE)
  if (remove.ends) {
    dil@.Data[,,1] = array(0, dim=dimg[1:2])
    dil@.Data[,,dimg[3]] = array(0, dim=dimg[1:2])
    if (nind >0 ){
      dil@.Data[ ind ] = 1  
    }
  }
  dil = cal_img(dil)
  if (refill) {
    dil = fslfill(file = dil, 
                    retimg=TRUE)  
  }
  if (have.outfile){
    gzipped = grepl("gz$", get.imgext())    
    writeNIfTI(dil, filename = outfile, gzipped = gzipped)
  }
  if (retimg){
    return(dil)
  }
  return(outfile)
}
