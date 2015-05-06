#' @title Register using FNIRT
#' @description This function calls \code{fnirt} to register infile to reffile
#' and either saves the image or returns an object of class nifti
#' @param infile (character) input filename
#' @param reffile (character) reference image to be registered to
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to FLIRT
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
fnirt = function(infile, 
                 reffile, 
                 outfile = NULL,                  
                 retimg = TRUE,
                 reorient = FALSE,                 
                 intern=FALSE,
                 opts="", verbose = TRUE, ...){
  cmd <- get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  #   infile = path.expand(infile)
  #   outfile = path.expand(outfile)
  #   reffile = path.expand(reffile)
  infile = checkimg(infile, ...)  
  reffile = checkimg(reffile, ...)  
  outfile = checkimg(outfile, ...)  
  outfile = nii.stub(outfile)
  
  cmd <- paste0(cmd, sprintf(
    'fnirt --in="%s" --ref="%s" --iout="%s" %s', 
    infile, reffile, outfile, opts))
  
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }
  return(res)
}


#' @title FNIRT help
#' @description This function calls \code{fnirt}'s help
#' @return Prints help output and returns output as character vector
#' @export
fnirt.help = function(){
  return(fslhelp("fnirt"))
}


#' @title Register using FNIRT, but doing Affine Registration as well
#' @description This function calls \code{fnirt} to register infile to reffile
#' and either saves the image or returns an object of class nifti, but does
#' the affine registration first
#' @param infile (character) input filename
#' @param reffile (character) reference image to be registered to
#' @param flirt.omat (character) Filename of output affine matrix
#' @param flirt.outfile (character) Filename of output affine-registered image
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param flirt.opts (character) additional options to FLIRT
#' @param opts (character) additional options to FNIRT
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
fnirt_with_affine = function(infile, 
                             reffile, 
                             flirt.omat = NULL,
                             flirt.outfile = NULL,
                             outfile = NULL,                  
                             retimg = TRUE,
                             reorient = FALSE,                 
                             intern=FALSE,
                             flirt.opts = "",
                             opts="", verbose = TRUE, ...){
  cmd <- get.fsl()
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  
  ##################################
  # FLIRT output matrix
  ##################################  
  if (is.null(flirt.omat)) {
    flirt.omat = tempfile()
  }
  flirt.omat = path.expand(flirt.omat)
  
  ##################################
  # FLIRT output file
  ##################################    
  if (is.null(flirt.outfile)) {
    flirt.outfile = tempfile()
  }
  flirt.outfile = path.expand(flirt.outfile)  
  
  flirt.outfile = checkimg(flirt.outfile, ...) 
  flirt.outfile = nii.stub(flirt.outfile)
  
  infile = checkimg(infile, ...)  
  reffile = checkimg(reffile, ...)  
  outfile = checkimg(outfile, ...)  
  outfile = nii.stub(outfile)
  
  affine.file = tempfile()
  res_flirt = flirt(infile = infile, 
                    reffile = reffile, 
                    omat = flirt.omat, 
                    dof = 12,
                    outfile = flirt.outfile,                  
                    ### keep retimg = FALSE
                    retimg = FALSE,
                    intern = intern, 
                    opts=flirt.opts, 
                    verbose = verbose)
  res_fnirt = fnirt(infile = flirt.outfile, 
                    reffile = reffile, 
                    outfile = outfile,                  
                    retimg = retimg,
                    reorient = reorient,                 
                    intern = intern,
                    opts=opts, verbose = verbose, ...)
  
  return(res_fnirt)
}

#' @title Applies FLIRT then FNIRT transformations
#'
#' @description Applies an affine transformation with FLIRT then the warp image with FNIRT
#' @param infile (character) input filename
#' @param reffile (character) reference image to be registered to
#' @param flirt.omat (character) Filename of output affine matrix
#' @param flirt.outfile (character) Filename of output affine-registered image
#' @param fnirt.warpfile (character) Filename of warp image from 
#' \code{\link{fnirt}}
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param flirt.opts (character) additional options to FLIRT
#' @param opts (character) additional options to FNIRT
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
#' @seealso fnirt_with_affine
fnirt_with_affine_apply <- function(infile, 
                                    reffile, 
                                    flirt.omat = NULL,
                                    flirt.outfile = NULL,
                                    fnirt.warpfile = NULL,
                                    outfile = NULL,                  
                                    retimg = TRUE,
                                    reorient = FALSE,                 
                                    intern = FALSE,
                                    flirt.opts = "",
                                    opts="", verbose = TRUE, ...){
  flirt_apply(infile = infile,
              reffile = reffile,
              initmat = flirt.omat,
              outfile = outfile,
              opts = flirt.opts, 
              intern = intern,
              verbose = verbose,
              # keep retimg = FALSE
              retimg = FALSE,
              ...)
  res = fsl_applywarp(infile = outfile,
                      reffile = reffile,          
                      warpfile = fnirt.warpfile,
                      outfile = outfile, 
                      opts = opts, 
                      intern = intern,
                      retimg = retimg,
                      reorient = reorient,
                      verbose = verbose,
                      ...)
  return(res)
}
