#' @name get.fsl
#' @title Create command declaring FSLDIR
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' for location of FSL functions
#' @param add_bin Should \code{bin} be added to the fsl path? 
#' All executables are assumed to be in \code{FSLDIR/bin/}.  If not, and 
#' \code{add_bin = FALSE}, they will be assumed to be in \code{FSLDIR/}.
#' @note This will use \code{Sys.getenv("FSLDIR")} before \code{getOption("fsl.path")}.
#' If the directory is not found for FSL in \code{Sys.getenv("FSLDIR")} and 
#' \code{getOption("fsl.path")}, it will try the default directory \code{/usr/local/fsl}.
#' @return NULL if FSL in path, or bash code for setting up FSL DIR
#' @export
#' @import neurobase
get.fsl = function(add_bin = TRUE){
  cmd = NULL
  fsldir = Sys.getenv("FSLDIR")
  if (fsldir == "") {
    fsldir = getOption("fsl.path")
    ## Will try a default directory (/usr/local/fsl) if nothing else
    if (is.null(fsldir)) {
      #### adding in "/usr/share/fsl/5.0" for NeuroDeb
      def_paths = c("/usr/local/fsl", "/usr/share/fsl/5.0")
      for (def_path in def_paths) {
        if (file.exists(def_path)) {
          warning(paste0("Setting fsl.path to ", def_path))
          options(fsl.path = def_path)
          fsldir = def_path
          break;
        }
      }
    } else {
      if (!file.exists(fsldir)) {
        warning(paste0("fsl.path set but folder doesn't exist! ", 
                       "Likely mis-configured option"))
      }
      
    }
    bin = "bin"
    bin_app = paste0(bin, "/")
    if (!add_bin) {
      bin_app = bin = ""
    }
    
    fslout = get.fsloutput()
    ld_dir = "/usr/lib/fsl/5.0"
    shfile = file.path(fsldir, "etc/fslconf/fsl.sh")
    cmd <- paste0("FSLDIR=", shQuote(fsldir), "; ", 
                  paste0('PATH=${FSLDIR}/', bin, ':${PATH};'),
                  'export PATH FSLDIR; ', 
                  ifelse(file.exists(shfile), 
                         'sh "${FSLDIR}/etc/fslconf/fsl.sh"; ', ""),
                  ifelse(dir.exists(ld_dir),
                      paste0('export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}":',
                             ld_dir, ";"), ""),
                  "FSLOUTPUTTYPE=", fslout, "; export FSLOUTPUTTYPE; ", 
                  paste0("${FSLDIR}/", bin_app)
    )
    fsl_pre = getOption("fsl_pre")
    if (is.null(fsl_pre)) { 
      fsl_pre = "" 
    } else { 
      fsl_pre = as.character(fsl_pre)
    }
    cmd = paste0(cmd, fsl_pre)
  } 
  if (is.null(fsldir)) stop("Can't find FSL")
  if (fsldir %in% "") stop("Can't find FSL")
  return(cmd)
}

#' @rdname get.fsl
#' @aliases get_fsl
#' @export
get_fsl = function(add_bin = TRUE){
  return(get.fsl(add_bin = add_bin))
}


#' @title Get FSL's Directory 
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' for location of FSL functions and returns it
#' @return Character path
#' @aliases fsl_dir
#' @export
fsldir = function(){
  fsldir = Sys.getenv("FSLDIR")
  if (fsldir == "") {
    x = get.fsl()
    fsldir = getOption("fsl.path")
  }
  return(fsldir)
}

#' @rdname fsldir
#' @export
fsl_dir = function(){
  fsldir()
}

#' @title Logical check if FSL is accessible
#' @description Uses \code{get.fsl} to check if FSLDIR is accessible or the option
#' \code{fsl.path} is set and returns logical
#' @param ... options to pass to \code{\link{get.fsl}}
#' @return Logical TRUE is FSL is accessible, FALSE if not
#' @export
#' @examples
#' have.fsl()
have.fsl = function(...){
  x = suppressWarnings(try(get.fsl(...), silent = TRUE))
  return(!inherits(x, "try-error"))
}

#' @rdname have.fsl
#' @aliases have_fsl
#' @export
have_fsl = function(...){
  return(have.fsl(...))
}


#' @name get.fsloutput
#' @title Determine FSL output type
#' @description Finds the FSLOUTPUTTYPE from system environment or 
#' \code{getOption("fsl.outputtype")} for output type (nii.gz, nii, ANALYZE,etc) 
#' @return FSLOUTPUTTYPE, such as \code{NIFTI_GZ}.  If none found, uses NIFTI_GZ as default
#' 
#' @export
get.fsloutput = function(){
  fslout = Sys.getenv("FSLOUTPUTTYPE")
  if (fslout == "") {
    fslout = getOption("fsl.outputtype")
  } 
  if (is.null(fslout)) {
    warning("Can't find FSLOUTPUTTYPE, setting to NIFTI_GZ")
    fslout = "NIFTI_GZ"
    options(fsl.outputtype = "NIFTI_GZ")
  }
  if (fslout == "") {
    warning("Can't find FSLOUTPUTTYPE, setting to NIFTI_GZ")
    fslout = "NIFTI_GZ"
    options(fsl.outputtype = "NIFTI_GZ")
  } 
  return(fslout)
}

#' @title Determine extension of image based on FSLOUTPUTTYPE
#' @description Runs \code{get.fsloutput()} to extract FSLOUTPUTTYPE and then 
#' gets corresponding extension (such as .nii.gz)
#' @return Extension for output type
#' @export
get.imgext = function(){
  fslout = get.fsloutput()
  ext = switch(fslout, 
               NIFTI_PAIR = ".hdr", 
               NIFTI_GZ = ".nii.gz", 
               ANALYZE = ".hdr", 
               ANALYZE_GZ = ".hdr.gz",
               NIFTI = ".nii",
               NIFTI_PAIR_GZ =  ".hdr.gz")
  return(ext)
}


#' @title FSL Maths Help
#' @description This function calls \code{fslmaths}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslmaths.help() 
#' }
fslmaths.help = function(){
  return(fslhelp("fslmaths"))
}



#' @title FSL Stats Help
#' @description This function calls \code{fslstats}'s help
#' @return Prints help output and returns output as character vector
#' @aliases fslrange.help fslmean.help fslentropy.help fslsd.help
#' @export
#' @examples
#' if (have.fsl()){
#'  fslstats.help() 
#' }
fslstats.help = function(){
  return(fslhelp("fslstats"))
}




#' @title FSL Maths 
#' @description This function calls \code{fslmaths}
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslmaths = function(
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  verbose = TRUE,
  ...){
  
  res = fslcmd("fslmaths", 
               file = file, 
               outfile = outfile, retimg = retimg,
               reorient = reorient, intern = intern, opts = opts, 
               ... = ..., verbose = verbose, samefile = FALSE)
  
  return(res)  
}





#' @title FSL Stats 
#' @description This function calls \code{fslstats}
#' @param file (character) filename of image to be checked
#' @param opts (character) operation passed to \code{fslstats}
#' @param verbose (logical) print out command before running
#' @param ts (logical) is the series a timeseries (4D), invoking \code{-t} 
#' option 
#' @param ... options passed to \code{\link{checkimg}}
#' @return Result of fslstats command
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' entropy = fslstats(img, opts='-E')
#' })
#' }  
fslstats <- function(file, opts="", verbose = TRUE, ts = FALSE, ...){
  cmd <- get.fsl()
  file = checkimg(file, ...)
  
  cmd <- paste0(cmd, 
                sprintf('fslstats %s "%s" %s', 
                        ifelse(ts, "-t", ""), 
                        file, 
                        opts))
  if (verbose) {
    message(cmd, "\n")
  }
  x = trimws(system(cmd, intern = TRUE))
  return(x)
}




#' @name fslsmooth
#' @title Gaussian smooth image using FSL
#' @description This function calls \code{fslmaths -s} to smooth an image and either
#' saves the image or returns an object of class nifti
#' @param file (character or nifti) image to be smoothed
#' @param sigma (numeric) sigma (in mm) of Gaussian kernel for smoothing
#' @param mask (character) optional mask given for image
#' @param smooth_mask (logical) Smooth mask?  If TRUE, the masked image 
#' will be divided by the smoothed mask.
#' @param smoothed_mask (character or nifti) If specified and 
#' \code{smooth_mask = TRUE}, then will use this as the smoothed mask for 
#' division.
#' @param outfile (character) resultant smoothed image name (optional)
#' if not give, will be the stub of the filename then _sigma
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.
#' @examples
#' if (have.fsl()){
#' system.time({
#' dims = c(50, 50, 20)
#' x = array(rnorm(prod(dims)), dim = dims)
#' img = nifti(x, dim= dims, 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' s.img = fslsmooth(img, retimg=TRUE)
#' })
#' }
#' @export
fslsmooth <- function(
  file,
  sigma=10, 
  mask=NULL, 
  smooth_mask = TRUE,
  smoothed_mask = NULL,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  verbose = TRUE,
  ...){
  
  leader = cmd = get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, sprintf('fslmaths "%s"', file))
  
  if ( !is.null(mask)) {
    mask = checkimg(mask, ...)
    cmd <- paste(cmd, sprintf(' -mas "%s"', mask))
  }
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, fileext = "")
  outfile = nii.stub(outfile)
  cmd <- paste(cmd, sprintf(' -s %s "%s";', sigma, outfile))
  ext = get.imgext()
  
  ### tempfile for mask.stub
  if ( !is.null(mask) & smooth_mask ) {
    if (is.null(smoothed_mask)) {
      smoothed_mask = tempfile(fileext = ".nii.gz")
      cmd <- paste(
        cmd, 
        paste0(leader, 
               sprintf('fslmaths "%s" -s %s "%s";', 
                       mask, sigma, smoothed_mask)))
    } else {
      smoothed_mask = checkimg(smoothed_mask, ...)
    }
    cmd <- paste(
      cmd, paste0(leader, 
                  sprintf('fslmaths "%s" -div "%s" -mas "%s" "%s";', 
                          outfile, smoothed_mask, mask, outfile)))    
  }
  if (verbose) {
    message(cmd, "\n")
  }  
  res = system(cmd, intern = intern)
  outfile = paste0(outfile, ext)  

  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  } else {
    return(outfile)
  }   
  #   x = file.remove(paste0(mask.blur, ".nii"))
  # return(res)
}

#' @name fslmask
#' @title Mask image using FSL
#' @description This function calls \code{fslmaths -mas} to mask an image from 
#' an image mask and either saves the image or returns an object of class nifti 
#' @param file (character) image to be masked
#' @param mask (character) mask given for image
#' @param outfile (character) resultant masked image name
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) additional options to be passed to fslmask
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.
#' @examples 
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e5), dim = c(100, 100, 10))
#' img = nifti(x, dim= c(100, 100, 10), 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' mask = img > .5
#' masked = fslmask(img, mask = mask, retimg=TRUE)
#' })
#' } 
#' @export
fslmask <- function(file, mask, outfile=NULL, 
                    retimg = TRUE,
                    reorient = FALSE,
                    intern=FALSE, opts="", verbose = TRUE,
                    ...){
  
  cmd = get.fsl()
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, fileext = "")
  outfile = nii.stub(outfile)
  file = checkimg(file, ...)
  mask = checkimg(mask, ...)
  cmd <- paste0(cmd, sprintf('fslmaths "%s" -mas "%s" %s "%s"', 
                             file, mask, opts, outfile))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }  
  return(res)
}

#' @name fslerode
#' @title Erode image using FSL
#' @description This function calls \code{fslmaths -ero} to erode an image with either 
#' the default FSL kernel or the kernel specified in \code{kopts}.  The function
#' either saves the image or returns an object of class nifti.
#' @param file (character) image to be eroded
#' @param outfile (character) resultant eroded image name 
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param kopts (character) options for kernel
#' @param opts (character) additional options to be passed to fslmaths
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.  If 
#' retimg is TRUE, then the image will be returned. 
#' @import oro.nifti
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' dims = c(50, 50, 20)
#' x = array(rnorm(prod(dims)), dim = dims) 
#' img = nifti(x, dim= dims, 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' mask = img > .5
#' eroded = fslerode(mask, kopts = "-kernel boxv 5", retimg=TRUE)
#' })
#' }    
fslerode <- function(file, outfile=NULL,   
                     retimg = TRUE,
                     reorient = FALSE,
                     intern=FALSE, kopts = "", opts="", 
                     verbose = TRUE,
                     ...){
  
  cmd = get.fsl()
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, fileext = "")
  
  outfile = nii.stub(outfile)
  file = checkimg(file, ...)    
  cmd <- paste0(cmd, sprintf('fslmaths "%s" %s -ero %s "%s"', 
                             file, kopts, opts, outfile))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)
  stopifnot(file.exists(outfile))
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  return(res)
}



#' @title Get value from FSL header
#' @description This function calls \code{fslval} to obtain a nifti header 
#' @param file (character) image filename or character of class nifti
#' @param keyword (character) keyword to be taken from fslhd
#' @param verbose (logical) print out command before running 
#' @param ... options passed to \code{\link{checkimg}}
#' @return Character of information from fslhd field specified in keyword
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  fslval(mnifile, keyword = "dim1")
#' }  
fslval <- function(file, keyword = "", verbose = TRUE, ...){
  cmd <- get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, sprintf('fslval "%s" %s', file, keyword))
  if (verbose) {
    message(cmd, "\n")
  }
  return(trimws(system(cmd, intern = TRUE)))
}

#' @title fslval help
#' @description This function calls \code{fslval}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslval.help()
#' } 
fslval.help = function(){
  return(fslhelp("fslval", help.arg = ""))
}


#' @title Get NIfTI header using FSL
#' @description This function calls \code{fslhd} to obtain a nifti header 
#' @param file (character) image filename or character of class nifti
#' @param opts (character) additional options to be passed to fslhd
#' @param verbose (logical) print out command before running 
#' @param ... options passed to \code{\link{checkimg}}
#' @return Character of information from fslhd
#' 
#' @import R.utils
#' @import graphics
#' @import grDevices
#' @import stats
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  fslhd(mnifile)
#' }   
fslhd <- function(file, opts="", verbose = TRUE, ...){
  cmd <- get.fsl()
  file = checkimg(file, ...)
  if (!file.exists(file)) {
    stop(paste0("File ", file, " does not exist!"))
  }  
  cmd <- paste0(cmd, sprintf('fslhd "%s" %s', file, opts))
  if (verbose) {
    message(cmd, "\n")
  }
  hd = system(cmd, intern = TRUE)
  # for FSL > 6.0
  hd = sub("size of header", "sizeof_hdr", hd)
  return(hd)
}

#' @title FSLhd help
#' @description This function calls \code{fslhd}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslhd.help()
#' }   
fslhd.help = function(){
  return(fslhelp("fslhd", help.arg = ""))
}

#' @title Parse FSL Header
#' @description This function takes in a FSL header and parses the components
#' @param hd (character) header from \code{\link{fslhd}}
#' @return data.frame of information from FSL header
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = mni_fname("2")
#'  hd = fslhd(mnifile)
#'  fslhd.parse(hd)
#' }  
fslhd.parse <- function(hd){
  if (length(hd) == 1) {
    if (file.exists(hd)) {
      hd = fslhd(hd)
    }
  }
  hd = sub("size of header", "sizeof_hdr", hd)
  ss <- strsplit(hd, split = " |\t")
  ss <- lapply(ss, function(x) x[!x %in% ""])
  ss <- lapply(ss, function(x){
    if (grepl("_xyz", x[1])) 
      x <- c(x[1], paste(x[2:length(x)], sep = "", collapse = " "))
    if (grepl("form_name", x[1])) 
      x <- c(x[1], paste(x[2:length(x)], sep = "", collapse = "-"))
    if (grepl("descrip", x[1])) 
      x <- c(x[1], paste(x[2:length(x)], sep = "", collapse = " "))  
    return(x)
  })
  ss.len <- sapply(ss, length)
  ss <- ss[ss.len > 0]
  ss.len <- sapply(ss, length)
  stopifnot(all(ss.len %in% c(1,2)))
  ss <- lapply(ss, function(x){
    if (length(x) == 1) x <- c(x, NA)
    x
  })
  ss <- do.call("rbind", ss)
  rownames(ss) <- ss[,1]
  ss <- data.frame(value = ss[,2, drop=FALSE], stringsAsFactors = FALSE)
  return(ss)
}

#' @title Get Q and S Forms of orientation matrix
#' @description This function obtains the s and q forms of an image transformation 
#' matrix
#' @param file (character) filename of image to pass to header
#' @param verbose (logical) passed to \code{\link{fslhd}}
#' @param ... options passed to \code{\link{checkimg}}
#' @return list with elements of sform and qform and their respective codes
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = mni_fname("2")
#'  getForms(mnifile)
#' }   
getForms <- function(file, 
                     verbose = FALSE,
                     ...){
  file = checkimg(file, ...)  
  x <- fslhd(file, verbose = verbose)
  convmat <- function(form){
    ss <- strsplit(form, " |\t")
    ss <- t(sapply(ss, function(x) x[ x != "" ]))
    ss <- ss[, -1]
    class(ss) <- "numeric"
    return(ss)
  }
  sform <- x[grepl("sto_xyz:", x)]
  sform <- convmat(sform)
  qform <- x[grepl("qto_xyz:", x)]
  qform <- convmat(qform)
  
  sor <- x[grepl("sform_(x|y|z)orient", x)]
  qor <- x[grepl("qform_(x|y|z)orient", x)]
  
  short_orient <- function(orient){
    ss <- strsplit(orient, " |\t")
    ss <- sapply(ss, function(x) x[ x != "" ])[2,]
    first <- substr(ss, 1,1)
    ss2 <- strsplit(ss, "-")
    ss2 <- sapply(ss2, function(x) x[length(x)])
    second <- substr(ss2, 1,1)
    paste(first, second, sep = "")
  }
  ssor <- short_orient(sor)
  sqor <- short_orient(qor)
  
  sform_code <- as.numeric(
    gsub("sform_code", "", x[grepl("sform_code", x)])
  )
  
  qform_code <- as.numeric(
    gsub("qform_code", "", x[grepl("qform_code", x)])
  )  
  return(list(qform = qform, sform = sform, sor = sor, qor = qor, 
              ssor = ssor, sqor = sqor, sform_code = sform_code, 
              qform_code = qform_code ))
}

#' @title Determine of Q and S forms are consistent
#' @description This function determines if the determinants of the sform and qform
#' have the same sign
#' @param hd (list) sforms from \code{\link{getForms}}
#' @return logical indicating if sform and qform consistent
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  forms = getForms(mnifile)
#'  checkout(forms)
#' } 
checkout <- function(hd){
  det.equal <- sign(det(hd$sform)) == sign(det(hd$qform))
  lr.equal <- hd$ssor[1] == hd$sqor[1]
  
  if ( (det.equal & !lr.equal) | (!det.equal & lr.equal)) {
    return(FALSE)
  }
  return(TRUE)
}

#' @title Wrapper for getForms with filename
#' @description Checking the q/s-forms for a header
#' @param file (character) filename of image to be checked
#' @param ... options passed to \code{\link{checkimg}}
#' @return result of \code{\link{checkout}}
#' @export
#' @examples
#' library(fslr)
#' if (have.fsl()){
#'  mnifile = mni_fname("2")
#'  check_file(mnifile)
#' } 
check_file <- function(file, ...){
  file = checkimg(file, ...)  
  hd <- getForms(file, ...)
  checkout(hd)
}

check_sform <- function(hd, value=0){
  hd$sform_code == value
}


check_sform_file <- function(file, value=0, ...){
  file = checkimg(file, ...)  
  hd <- getForms(file, ...)
  check_sform(hd, value = value)
}
## if sign(det(res$sform)) == sign(det(res$qform)) and 
## res$ssor[1] == res$sqor[1] then all good


#' @title Get range of an image
#' @description This function calls \code{fslstats -R} to get the range of an image or \code{fslstats -r} to 
#' get the robust range
#' @param file (character) filename of image to be checked
#' @param robust (logical) Should the range be robust (\code{-r})
#' @param verbose (logical) print out command before running
#' @param ts (logical) is the series a timeseries (4D), invoking \code{-t} 
#' option
#' @param ... options passed to \code{\link{checkimg}}
#' @return numeric vector of length 2
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  fslrange(mnifile)
#' }  
fslrange <- function(file, robust = FALSE, verbose = TRUE, ts = FALSE, ...){
  opts = "-R"
  opts = ifelse(robust, tolower(opts), opts)
  
  val = fslstats(file, opts = opts, verbose = verbose, ts = ts)
  val = strsplit(val, " ")
  if (length(val) == 1) {
    val = as.numeric(val[[1]])
  } else {
    val = t(sapply(val, as.numeric))
  }
  val
}

#' @title Fill image holes
#' @description This function calls \code{fslmaths -fillh} to fill in image holes
#' and either saves the image or returns an object of class nifti  
#' @param file (character) filename of image to be filled
#' @param outfile (character) name of resultant filled file
#' @param bin (logical) binarize the image before filling
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) pass to \code{\link{system}}
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return character or logical depending on intern
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' dims = c(50, 50, 20)
#' x = array(rnorm(prod(dims)), dim = dims) 
#' img = nifti(x, dim= dims, 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' mask = img > .5
#' eroded = fslerode(mask, kopts = "-kernel boxv 5", retimg=TRUE)
#' filled = fslfill(eroded, retimg= TRUE)
#' })
#' }  
fslfill = function(file, outfile = NULL, bin=TRUE, 
                   retimg = TRUE,
                   reorient = FALSE,
                   intern=FALSE, verbose = TRUE,
                   ...){
  cmd <- get.fsl()
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, fileext = "")
  
  outfile = nii.stub(outfile)
  
  file = checkimg(file, ...)    
  runbin = ""
  if (bin) runbin = "-bin"
  cmd <- paste0(cmd, sprintf('fslmaths "%s" %s -fillh "%s"', file, 
                             runbin, outfile))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  return(res)
}

#' @title Threshold an image
#' @description This function calls \code{fslmaths -thr -uthr} to threshold an image
#' and either saves the image or returns an object of class nifti   
#' @param file (character) filename of image to be thresholded
#' @param outfile (character) name of resultant thresholded file
#' @param thresh (numeric) threshold (anything below set to 0)
#' @param uthresh (numeric) upper threshold (anything above set to 0)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to be passed to fslmaths 
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return character or logical depending on intern
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' thresh = fslthresh(img, thresh=0, uthresh = 2, retimg=TRUE)
#' })
#' } 
fslthresh = function(file, outfile = NULL, 
                     thresh = 0, 
                     uthresh = NULL,
                     retimg = TRUE,
                     reorient = FALSE,
                     intern=FALSE, 
                     opts = "", verbose = TRUE, ...){
  cmd <- get.fsl()
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, fileext = "")
  
  outfile = nii.stub(outfile)
  
  file = checkimg(file, ...)  
  
  if (!is.null(uthresh)) {
    opts = paste(sprintf("-uthr %f", uthresh), opts)
  }
  cmd <- paste0(cmd, sprintf('fslmaths "%s" -thr %f %s "%s"', 
                             file, thresh, opts, outfile))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  return(res)
}

#' @title Subsample image by factor of 2
#' @description This function calls \code{fslmaths -subsamp2} to subsample an image
#' and either saves the image or returns an object of class nifti   
#' @param file (character) filename of image to be subsampled
#' @param outfile (character) name of resultant subsampled file
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return character or logical depending on intern
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' subsamp = fslsub2(img, retimg=TRUE)
#' print(voxdim(subsamp))
#' })
#' } 
fslsub2 = function(file, 
                   outfile = NULL, 
                   retimg = TRUE,
                   reorient = FALSE,
                   intern=FALSE, verbose = TRUE, ...){
  res = fslmaths(file = file, 
                 outfile = outfile, 
                 retimg = retimg,
                 reorient = reorient,
                 opts = "-subsamp2",
                 intern = intern, verbose = verbose, ... = ...)
  #   cmd <- get.fsl()
  #  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  #   outfile = nii.stub(outfile)
  #   
  #   file = checkimg(file, ...)  
  #   
  #   cmd <- paste0(cmd, sprintf('fslmaths "%s" -subsamp2 "%s"', 
  #                             file, outfile))
  #   res = system(cmd, intern=intern)
  #   ext = get.imgext()
  #   outfile = paste0(outfile, ext)  
  #   if (retimg){
  #     img = readnii(outfile, reorient=reorient, ...)
  #     return(img)
  #   }
  return(res)
}

#' @title Merge images using FSL
#' @description This function calls \code{fslmerge} to merge files on some dimension
#' and either saves the image or returns an object of class nifti   
#' @param infiles (character) input filenames
#' @param direction (character) direction to merge over, x, y, z, 
#' t (time), a (auto)
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) pass to \code{\link{system}}
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return character or logical depending on intern
#' @export
fslmerge = function(infiles, 
                    direction = c("x", "y", "z", "t", "a"), 
                    outfile = NULL, 
                    retimg = TRUE,
                    reorient = FALSE,                   
                    intern=FALSE, verbose = TRUE, ...){
  cmd <- get.fsl()
  direction = direction[1]
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, fileext = ".nii.gz")
  outfile = nii.stub(outfile)  
  infiles = sapply(infiles, checkimg)
  infiles = paste(infiles, sep = "", collapse = " ")
  
  cmd <- paste0(cmd, sprintf('fslmerge -%s "%s" %s', 
                             direction, outfile, infiles))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  return(res)
}


#' @title FSLMerge help
#' @description This function calls \code{fslmerge}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslmerge.help()
#' }  
fslmerge.help = function(){
  return(fslhelp("fslmerge"))
}



#' @title Register using FLIRT
#' @description This function calls \code{flirt} to register infile to reffile
#' and either saves the image or returns an object of class nifti, along with the
#' transformation matrix omat  
#' @param infile (character) input filename
#' @param reffile (character) reference image to be registered to
#' @param omat (character) Output matrix name
#' @param dof (numeric) degrees of freedom (default 6 - rigid body)
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to FLIRT
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return character or logical depending on intern
#' @export
flirt = function(infile, 
                 reffile, omat = NULL,
                 dof = 6,
                 outfile = NULL,                  
                 retimg = TRUE,
                 reorient = FALSE,                 
                 intern=FALSE,
                 opts="", verbose = TRUE, ...){
  cmd <- get.fsl()
  outfile = check_outfile(outfile = outfile, retimg = retimg, fileext = "")
  #   infile = path.expand(infile)
  #   outfile = path.expand(outfile)
  #   reffile = path.expand(reffile)
  infile = checkimg(infile, ...)  
  reffile = checkimg(reffile, ...)  
  outfile = checkimg(outfile, ...)  
  outfile = nii.stub(outfile, ...)
  
  print.omat = FALSE
  if (is.null(omat)) {
    omat = tempfile(fileext = ".mat")
    print.omat = TRUE
  }
  omat = path.expand(omat)
  cmd <- paste0(cmd, sprintf(
    'flirt -in "%s" -ref "%s" -out "%s" -dof %d -omat "%s" %s', 
    infile, reffile, outfile, dof, omat, opts))
  
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  if (verbose & print.omat) {
    message(paste0("Output matrix not specified, but stored ", 
                   "temporarily at ", omat, "\n"))
  }
  return(outfile)
}


#' @title FLIRT help
#' @description This function calls \code{flirt}'s help
#' @return Prints help output and returns output as character vector
#' @aliases flirt_apply.help
#' @export
#' @examples
#' if (have.fsl()){
#'  flirt.help()
#' } 
flirt.help = function(){
  return(fslhelp("flirt", help.arg = "-help"))
}


#' @title Run MELODIC ICA
#' @description This function calls \code{melodic} 
#' @param file (character) image to be run
#' @param outdir (character) output directory. 
#' (Default \code{dirname(file)})
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) options for melodic
#' @param verbose (logical) print out command before running
#' @param ... arguments passed to \code{\link{checkimg}}
#' @return character or logical depending on intern
#' @export
melodic = function(file, 
                   outdir = dirname(file), 
                   intern=FALSE,                   
                   opts ="", verbose = TRUE, ...){
  cmd <- get.fsl()
  file = path.expand(outdir)
  
  outdir = path.expand(outdir)
  stopifnot(file.exists(outdir))
  file = checkimg(file, ...)  
  
  cmd <- paste0(cmd, sprintf('melodic --in "%s" --outdir "%s" %s', 
                             file, outdir, opts))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  return(res)
}

#' @title MELODIC help
#' @description This function calls \code{melodic}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  melodic.help()
#' } 
melodic.help = function(){
  return(fslhelp("melodic"))
}


#' @title Wrapper for getting fsl help
#' @description This function takes in the function and returns the
#' help from FSL for that function
#' @param func_name FSL function name
#' @param help.arg Argument to print help, usually "--help" 
#' @param extra.args Extra arguments to be passed other than 
#' \code{--help}
#' @return Prints help output and returns output as character vector
#' @export
fslhelp = function(func_name, help.arg = "--help", extra.args = ""){
  cmd = get.fsl()
  cmd <- paste0(cmd, sprintf('%s %s %s', func_name, 
                             help.arg, extra.args))
  #     args = paste(help.arg, extra.args, sep=" ", collapse = " ")
  suppressWarnings({res = system(cmd, intern = TRUE)})
  #     res = system2(func_name, args = args, stdout=TRUE, stderr=TRUE)
  message(res, sep = "\n")
  return(invisible(res))
}



#' @title Use FSL's Brain Extraction Tool (BET)
#' @description This function calls \code{bet} to extract a brain 
#' from an image, usually for skull stripping.
#' @param infile (character) input filename
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to \code{bet}
#' @param betcmd (character) Use \code{bet} or \code{bet2} function
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return character or logical depending on intern
#' @export
fslbet = function(infile, 
                  outfile = NULL,                  
                  retimg = TRUE,
                  reorient = FALSE,                 
                  intern = FALSE,
                  opts="", 
                  betcmd = c("bet2", "bet"),
                  verbose = TRUE,
                  ...){
  betcmd = match.arg( betcmd )
  cmd <- get.fsl()
  outfile = check_outfile(outfile = outfile, retimg = retimg, fileext = "")
  infile = checkimg(infile, ...)  
  outfile = checkimg(outfile, ...)  
  outfile = nii.stub(outfile, ...)
  cmd <- paste0(cmd, sprintf('%s "%s" "%s" %s', 
                             betcmd, infile, outfile, opts))
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  }
  return(res)
}

#' @title Help for FSL BET
#' @description This function calls \code{bet}'s help
#' @param betcmd (character) Get help for \code{bet} or \code{bet2} function
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslbet.help()
#'  fslbet.help("bet")
#' }  
fslbet.help = function(betcmd = c("bet2", "bet")){
  betcmd = match.arg( betcmd )
  return(fslhelp(betcmd, help.arg = "-h"))
}


#' @title Image Center of Gravity (FSL)
#' @description Find Center of Gravity of Image from FSL
#' @param img Object of class nifti, or path of file
#' @param mm Logical if the center of gravity (COG) would be in mm (default \code{TRUE})
#' or voxels (\code{FALSE})
#' @param verbose (logical) print out command before running 
#' @param ts (logical) is the series a timeseries (4D), invoking \code{-t} 
#' option  
#' @return Vector of length 3 unless ts option invoked
#' @note FSL uses a 0-based indexing system, which will give you a different 
#' answer compared to \code{cog}, but \code{fslcog(img, mm = FALSE) +1} 
#' should be relatively close to \code{cog(img)}
#' @export
#' @examples
#' if (have.fsl()){
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' fslcog(img)
#' }
fslcog = function(img, mm = TRUE, verbose = TRUE, ts = FALSE){
  opts = ifelse(mm, "-c", "-C")
  
  cog = fslstats(img, opts = opts, verbose = verbose, ts = ts)
  cog = strsplit(cog, " ")
  if (length(cog) == 1) {
    cog = as.numeric(cog[[1]])
  } else {
    cog = t(sapply(cog, as.numeric))
  }
  cog
}



#' @title FSL Orient 
#' @description This function calls \code{fslorient}
#' @param file (character) image to be manipulated
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If \code{retimg}, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslorient}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslorient = function(
  file,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  verbose = TRUE,
  ...){
  
  if (grepl("-get", opts) & retimg) {
    warning(paste0("fslorient option was a -get, ",
                   "image was not changed - output not returned,",
                   " and retimg set to FALSE"))
    if (!intern) {
      warning("intern set to TRUE")
    }
    retimg = FALSE
  }  
  cmd = get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, sprintf('fslorient %s "%s"', opts, file))
  outfile = nii.stub(file)
  ext = get.imgext()  
  outfile = paste0(outfile, ext)
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  if (retimg) {
    img = readnii(outfile, reorient = reorient, ...)
    return(img)
  } 
  return(res)  
}

#' @title fslorient help
#' @description This function calls \code{fslorient}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslorient.help()
#' } 
fslorient.help = function(){
  return(fslhelp("fslorient", help.arg = ""))
}



#' @title FSL Orient to MNI
#' @description This function calls \code{fslreorient2std}
#' @param file (character) image to be manipulated
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If \code{retimg}, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param verbose (logical) print out command before running
#' @param opts additional options to pass to \code{\link{fslreorient2std}}
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslreorient2std = function(
  file,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  verbose = TRUE,
  opts = "",
  ...){
  
  res = fslcmd(func = "fslreorient2std", 
               file = file,
               outfile = NULL,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = opts,
               verbose = verbose,
               ... = ..., 
               samefile = TRUE)
  
  return(res)  
}

#' @param matfile Output file for the matrix for reorientation
#' @export 
#' @rdname fslreorient2std
fslreorient2std_mat = function(
  file,
  matfile = tempfile(fileext = ".mat"),
  verbose = TRUE,
  ...){
  
  if (file.exists(matfile)) {
    file.remove(matfile)
  }
  rr = fslr::fslreorient2std(
    file, 
    no.outfile = TRUE, 
    opts = paste0(" > ", matfile), 
    verbose = verbose,
    ...)
  result = attr(rr, "result")
  if (result != 0 | !file.exists(matfile)) {
    warning("result from fslreorient2std_mat did not seem to work")
  }
  
  return(matfile)  
}

#' @title fslreorient2std help
#' @description This function calls \code{fslreorient2std}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslreorient2std.help()
#' }  
fslreorient2std.help = function(){
  return(fslhelp("fslreorient2std", help.arg = ""))
}



#' @title FSL Swap Dimensions 
#' @description This function calls \code{fslswapdim}
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param a (character) Option for x domain in \code{fslswapdim}
#' @param b (character) Option for y domain in \code{fslswapdim}
#' @param c (character) Option for z domain in \code{fslswapdim}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslswapdim = function(
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  a = "x",
  b = "y",
  c = "z",
  verbose = TRUE,
  ...){
  
  opts = paste(a, b, c)
  
  res = fslcmd(func = "fslswapdim", 
               file = file,
               outfile = outfile,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = opts,
               verbose = verbose,
               ... = ..., 
               samefile = FALSE)
  
  return(res)  
}

#' @title fslswapdim help
#' @description This function calls \code{fslswapdim}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslswapdim.help()
#' }  
fslswapdim.help = function(){
  return(fslhelp("fslswapdim"))
}


#' @title FSL Command Wrapper
#' @description This function calls fsl command passed to \code{func}
#' @param func (character) FSL function
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{func} 
#' @param verbose (logical) print out command before running
#' @param samefile (logical) is the output the same file?
#' @param opts_after_outfile (logical) should \code{opts} come after 
#' the \code{outfile} in the FSL command?
#' @param frontopts (character) options/character to put in before filename
#' @param no.outfile (logical) is there an output file in the arguments of 
#' the FSL function?
#' @param trim_front trim the whitespace from the front of the command.
#' @param run (logical) Should the command just be printed (if \code{FALSE})?
#' @param ... additional arguments passed to \code{\link{readnii}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslcmd = function(
  func,
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  verbose = TRUE,
  samefile = FALSE,
  opts_after_outfile = FALSE,
  frontopts = "",
  no.outfile = FALSE,
  trim_front = FALSE,
  run = TRUE,
  ...){
  
  cmd = get.fsl()
  file = checkimg(file, ...)
  # file = path.expand(file)
  
  ##########################
  # Add frontopts
  ##########################
  frontopts = paste(frontopts, collapse = " ")
  
  s = sprintf('%s %s ', func, frontopts)
  s = gsub("\\s\\s+", " ", s)
  s = sub("[ \t\r\n]+$", "", s, perl = TRUE)
  if (trim_front) {
    s = trimws(s)
    s = paste0(s, sprintf('"%s"', file))
  } else {
    s = paste(s, sprintf('"%s"', file))
  }
  cmd <- paste0(cmd, s)
  # cmd <- paste0(cmd, sprintf('%s "%s"', func, file))
  
  if (no.outfile & samefile) outfile = ""  
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, fileext = "")
  outfile = nii.stub(outfile)
  
  opts = paste(opts, collapse = " ")
  if (no.outfile) {
    cmd <- paste(cmd, sprintf(' %s ;', opts))
  } else {
    if (!opts_after_outfile) {
      cmd <- paste(cmd, sprintf(' %s "%s";', opts, outfile))
    } else {
      cmd <- paste(cmd, sprintf(' "%s" %s;', outfile, opts))
    }
  }
  ext = get.imgext()
  if (verbose) {
    message(cmd, "\n")
  }
  if (!run) {
    return(cmd)
  }
  res = system(cmd, intern = intern)
  outfile = paste0(outfile, ext)  
  if (retimg) {
    if (samefile) outfile = file
    img = readnii(outfile, reorient = reorient, ...)
    attr(img, "result") = res
    return(img)
  } 
  attr(outfile, "result") = res
  
  return(outfile)  
}
