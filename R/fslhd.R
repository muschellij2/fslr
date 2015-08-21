#' @name get.fsl
#' @title Create command declaring FSLDIR
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' for location of FSL fuctions
#' @return NULL if FSL in path, or bash code for setting up FSL DIR
#' @export
get.fsl = function(){
  cmd = NULL
  fsldir = Sys.getenv("FSLDIR")
  if (fsldir == "") {
    fsldir = getOption("fsl.path")
    fslout = get.fsloutput()
    cmd <- paste0("FSLDIR=", shQuote(fsldir), "; ", 
                  'export FSLDIR; sh "${FSLDIR}/etc/fslconf/fsl.sh"; ',
                  "FSLOUTPUTTYPE=", fslout, "; export FSLOUTPUTTYPE; ", 
                  "$FSLDIR/bin/")
  } 
  if (is.null(fsldir)) stop("Can't find FSL")
  if (fsldir %in% "") stop("Can't find FSL")
  return(cmd)
}


#' @title Get FSL's Directory 
#' @description Finds the FSLDIR from system environment or \code{getOption("fsl.path")}
#' for location of FSL fuctions and returns it
#' @return Character path
#' @export
fsldir = function(){
  fsldir = Sys.getenv("FSLDIR")
  if (fsldir == "") {
    fsldir = getOption("fsl.path")
  }
  return(fsldir)
}

#' @title Logical check if FSL is accessible
#' @description Uses \code{get.fsl} to check if FSLDIR is accessible or the option
#' \code{fsl.path} is set and returns logical
#' @return Logical TRUE is FSL is accessible, FALSE if not
#' @export
#' @examples
#' have.fsl()
have.fsl = function(){
  x = suppressWarnings(try(get.fsl(), silent=TRUE))
  return(!inherits(x, "try-error"))
}

#' @name get.fsloutput
#' @title Determine FSL output type
#' @description Finds the FSLOUTPUTTYPE from system environment or 
#' \code{getOption("fsl.outputtype")} for output type (nii.gz, nii, ANALYZE,etc) 
#' @return FSLOUTPUTTYPE, such as NIFTI_GZ.  If none found, uses NIFTI_GZ as default
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
#' @aliases fslrange.help
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
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
                    file=file, 
                    outfile = outfile, retimg= retimg,
                    reorient=reorient, intern=intern, opts=opts, 
                    ... = ..., verbose = verbose, samefile = FALSE)
  
  return(res)  
}


#' @title Binarize Image using FSL 
#' @description This function calls \code{fslmaths -bin}.  The R functions wraps
#' \code{fslmaths}
#' @param file (character) image to be binarized
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslbin = function(
  file,
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  opts = "", 
  ...){
  
  all.opts = paste("-bin ", opts, collapse=" " )
  res = fslmaths(file=file, outfile=outfile, 
           retimg=retimg, reorient=reorient,
           intern=intern, opts = all.opts, ...)
  
  return(res)  
}




#' @title FSL Stats 
#' @description This function calls \code{fslstats}
#' @param file (character) filename of image to be checked
#' @param opts (character) operation passed to \code{fslstats}
#' @param verbose (logical) print out command before running
#' @param ... options passed to \code{\link{checkimg}}
#' @return Result of fslstats command
#' @import stringr
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
fslstats <- function(file, opts="", verbose = TRUE, ...){
  cmd <- get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, sprintf('fslstats "%s" %s', file, opts))
  if (verbose){
    cat(cmd, "\n")
  }
  x = str_trim(system(cmd, intern = TRUE))
  return(x)
}




#' @name fslsmooth
#' @title Gaussian smooth image using FSL
#' @description This function calls \code{fslmaths -s} to smooth an image and either
#' saves the image or returns an object of class nifti
#' @param file (character) image to be smoothed
#' @param sigma (numeric) sigma (in mm) of Gaussian kernel for smoothing
#' @param mask (character) optional mask given for image
#' @param smooth_mask (logical) Smooth mask?  If TRUE, the masked image 
#' will be divided by the smoothed mask.
#' @param outfile (character) resultant smoothed image name (optional)
#' if not give, will be the stub of the filename then _sigma
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.
#' @examples
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
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
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  verbose = TRUE,
  ...){
	
  leader = cmd = get.fsl()
  file = checkimg(file, ...)
	cmd <- paste0(cmd, sprintf('fslmaths "%s"', file))
	if (! is.null(mask)) {
    mask = checkimg(mask, ...)
    cmd <- paste(cmd, sprintf(' -mas "%s"', mask))
	}
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  outfile = nii.stub(outfile)
	cmd <- paste(cmd, sprintf(' -s %s "%s";', sigma, outfile))
  ext = get.imgext()
  
  rm.mask.img = FALSE
	### tempfile for mask.stub
  if ( !is.null(mask) & smooth_mask ) {
    rm.mask.img = TRUE
    mask = checkimg(mask, ...)
    mask.stub <- basename(mask)
    mask.stub = nii.stub(mask.stub)
  	mask.stub <- file.path(dirname(mask), mask.stub)
  	mask.blur <- sprintf("%s_%s", mask.stub, sigma)
   	cmd <- paste(cmd, paste0(leader, 
                              sprintf('fslmaths "%s" -s %s "%s";', 
   		mask, sigma, mask.blur)))
   	cmd <- paste(cmd, paste0(leader, 
                              sprintf('fslmaths "%s" -div "%s" -mas "%s" "%s";', 
   		outfile, mask.blur, mask, outfile)))
  }
  if (verbose){
    cat(cmd, "\n")
  }  
	res = system(cmd, intern=intern)
  outfile = paste0(outfile, ext)  
  if (rm.mask.img){
    file.remove(paste0(mask.blur, ext))    
  }
  
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }   
#   x = file.remove(paste0(mask.blur, ".nii"))
  return(res)
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
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) additional options to be passed to fslmask
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
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
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  outfile = nii.stub(outfile)
  file = checkimg(file, ...)
  mask = checkimg(mask, ...)
  cmd <- paste0(cmd, sprintf('fslmaths "%s" -mas "%s" %s "%s"', 
		file, mask, opts, outfile))
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

#' @name fslerode
#' @title Erode image using FSL
#' @description This function calls \code{fslmaths -ero} to erode an image with either 
#' the default FSL kernel or the kernel specified in \code{kopts}.  The function
#' either saves the image or returns an object of class nifti.
#' @param file (character) image to be eroded
#' @param outfile (character) resultant eroded image name 
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param kopts (character) options for kernel
#' @param opts (character) additional options to be passed to fslmaths
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.  If 
#' retimg is TRUE, then the image will be returned. 
#' @import oro.nifti
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
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
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  
  outfile = nii.stub(outfile)
  file = checkimg(file, ...)    
  cmd <- paste0(cmd, sprintf('fslmaths "%s" %s -ero %s "%s"', 
                            file, kopts, opts, outfile))
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)
  stopifnot(file.exists(outfile))
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
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
#' @return Character of infromation from fslhd field specified in keyword
#' @export
#' @import stringr
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
  if (verbose){
    cat(cmd, "\n")
  }
  return(str_trim(system(cmd, intern=TRUE)))
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
  return(fslhelp("fslval", help.arg=""))
}


#' @title Get NIfTI header using FSL
#' @description This function calls \code{fslhd} to obtain a nifti header 
#' @param file (character) image filename or character of class nifti
#' @param opts (character) additional options to be passed to fslhd
#' @param verbose (logical) print out command before running 
#' @param ... options passed to \code{\link{checkimg}}
#' @return Character of infromation from fslhd
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
	cmd <- paste0(cmd, sprintf('fslhd "%s" %s', file, opts))
  if (verbose){
    cat(cmd, "\n")
  }
	system(cmd, intern=TRUE)
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
  return(fslhelp("fslhd", help.arg=""))
}

#' @title Parse FSL Header
#' @description This function takes in a FSL header and parses the components
#' @param hd (character) header from \code{\link{fslhd}}
#' @return data.frame of information from FSL header
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  hd = fslhd(mnifile)
#'  fslhd.parse(hd)
#' }  
fslhd.parse <- function(hd){
  ss <- strsplit(hd, split=" ")
  ss <- lapply(ss, function(x) x[!x %in% ""])
  ss <- lapply(ss, function(x){
    if (grepl("_xyz", x[1])) 
      x <- c(x[1], paste(x[2:length(x)], sep="", collapse= " "))
    if (grepl("form_name", x[1])) 
      x <- c(x[1], paste(x[2:length(x)], sep="", collapse= "-"))
    if (grepl("descrip", x[1])) 
      x <- c(x[1], paste(x[2:length(x)], sep="", collapse= " "))  
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
  ss <- data.frame(value=ss[,2, drop=FALSE], stringsAsFactors=FALSE)
  return(ss)
}

#' @title Get Q and S Forms of orientation matrix
#' @description This function obtains the s and q forms of an image transformation 
#' matrix
#' @param file (character) filename of image to pass to header
#' @param ... options passed to \code{\link{checkimg}}
#' @return list with elements of sform and qform and their respective codes
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  getForms(mnifile)
#' }   
getForms <- function(file, ...){
  file = checkimg(file, ...)  
	x <- fslhd(file)
	convmat <- function(form){
		ss <- strsplit(form, " ")
		ss <- t(sapply(ss, function(x) x[x!=""]))
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
		ss <- strsplit(orient, " ")
		ss <- sapply(ss, function(x) x[x!=""])[2,]
		first <- substr(ss, 1,1)
		ss2 <- strsplit(ss, "-")
		ss2 <- sapply(ss2, function(x) x[length(x)])
		second <- substr(ss2, 1,1)
		paste(first, second, sep="")
	}
	ssor <- short_orient(sor)
	sqor <- short_orient(qor)
  
  sform_code <- as.numeric(
    gsub("sform_code", "", x[grepl("sform_code", x)])
  )

	qform_code <- as.numeric(
	  gsub("qform_code", "", x[grepl("qform_code", x)])
	)  
	return(list(qform=qform, sform=sform, sor=sor, qor=qor, 
		ssor=ssor, sqor=sqor, sform_code= sform_code, qform_code=qform_code ))
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
#' @param file (character) filename of image to be checked
#' @param ... options passed to \code{\link{checkimg}}
#' @return result of \code{\link{checkout}}
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
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
	check_sform(hd, value=value)
}
## if sign(det(res$sform)) == sign(det(res$qform)) and 
## res$ssor[1] == res$sqor[1] then all good


#' @title Get range of an image
#' @description This function calls \code{fslstats -R} to get the range of an image
#' @param file (character) filename of image to be checked
#' @param verbose (logical) print out command before running
#' @param ... options passed to \code{\link{checkimg}}
#' @return numeric vector of length 2
#' @import stringr
#' @export
#' @examples
#' if (have.fsl()){
#'  mnifile = file.path(fsldir(), "data", "standard", 
#'    "MNI152_T1_2mm.nii.gz")
#'  fslrange(mnifile)
#' }  
fslrange <- function(file, verbose =TRUE, ...){
	cmd <- get.fsl()
	file = checkimg(file, ...)
	cmd <- paste0(cmd, sprintf('fslstats "%s" -R', file))
  if (verbose){
    cat(cmd, "\n")
  }
  x = str_trim(system(cmd, intern = TRUE))
  x = strsplit(x, " ")[[1]]
  x = as.numeric(x)
  x
}

#' @title Fill image holes
#' @description This function calls \code{fslmaths -fillh} to fill in image holes
#' and either saves the image or returns an object of class nifti  
#' @param file (character) filename of image to be filled
#' @param outfile (character) name of resultant filled file
#' @param bin (logical) binarize the image before filling
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) pass to \code{\link{system}}
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
#' @examples
#' if (have.fsl()){
#' system.time({
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
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
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  
  outfile = nii.stub(outfile)
  
  file = checkimg(file, ...)    
  runbin = ""
  if (bin) runbin = "-bin"
  cmd <- paste0(cmd, sprintf('fslmaths "%s" %s -fillh "%s"', file, 
                            runbin, outfile))
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

#' @title Threshold an image
#' @description This function calls \code{fslmaths -thr -uthr} to threshold an image
#' and either saves the image or returns an object of class nifti   
#' @param file (character) filename of image to be thresholded
#' @param outfile (character) name of resultant thresholded file
#' @param thresh (numeric) threshold (anything below set to 0)
#' @param uthresh (numeric) upper threshold (anything above set to 0)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to be passed to fslmaths 
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  
  outfile = nii.stub(outfile)
  
  file = checkimg(file, ...)  
  
  if (!is.null(uthresh)){
    opts = paste(sprintf("-uthr %f", uthresh), opts)
  }
  cmd <- paste0(cmd, sprintf('fslmaths "%s" -thr %f %s "%s"', 
  	file, thresh, opts, outfile))
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

#' @title Subsample image by factor of 2
#' @description This function calls \code{fslmaths -subsamp2} to subsample an image
#' and either saves the image or returns an object of class nifti   
#' @param file (character) filename of image to be subsampled
#' @param outfile (character) name of resultant subsampled file
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  res = fslmaths(file=file, 
           outfile = outfile, 
           retimg = retimg,
           reorient = reorient,
           opts="-subsamp2",
           intern=intern, verbose = verbose, ... = ...)
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
#     img = readNIfTI(outfile, reorient=reorient, ...)
#     return(img)
#   }
  return(res)
}

#' @title Open image in FSLView
#' @description This function calls \code{fslview} to view an image 
#' in the FSL viewer
#' @param file (character) filename of image to be thresholded
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) options for FSLView
#' @param verbose (logical) print out command before running
#' @param ... options passed to \code{\link{checkimg}}
#' @return character or logical depending on intern
#' @export
fslview = function(file, intern=TRUE, opts ="", verbose = TRUE, ...){
  cmd <- get.fsl()
  if (is.nifti(file)) {
    file = checkimg(file)
  }
  file = lapply(file, checkimg, ...)
  if (length(file) != length(opts)) {
    opts = rep(opts, length = length(file))
  } else {
    if (length(file) > length(opts)) {
      opts = c(opts, rep("", length = (length(file) - length(opts))))
    } else {
      opts = opts[seq(length(file))]
    }
  }
  file = shQuote(file)
  file = paste(file, opts)
  file = paste(file, collapse = " ")
  cmd <- paste0(cmd, sprintf('fslview %s', file))
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern = intern)
  return(res)
}

#' @title FSLView help
#' @description This function calls \code{fslview}'s help
#' @return Prints help output and returns output as character vector
#' @export
#' @examples
#' if (have.fsl()){
#'  fslview.help()
#' }   
fslview.help = function(){
  return(fslhelp("fslview"))
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
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) pass to \code{\link{system}}
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = ".nii.gz")
  outfile = nii.stub(outfile)  
  infiles = sapply(infiles, checkimg)
  infiles = paste(infiles, sep="", collapse = " ")
  
  cmd <- paste0(cmd, sprintf('fslmerge -%s "%s" %s', 
                             direction, outfile, infiles))
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
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to FLIRT
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
#   infile = path.expand(infile)
#   outfile = path.expand(outfile)
#   reffile = path.expand(reffile)
  infile = checkimg(infile, ...)  
  reffile = checkimg(reffile, ...)  
  outfile = checkimg(outfile, ...)  
  outfile = nii.stub(outfile, ...)
  
  print.omat = FALSE
  if (is.null(omat)){
    omat = tempfile(fileext = ".mat")
    print.omat = TRUE
  }
  omat = path.expand(omat)
  cmd <- paste0(cmd, sprintf(
    'flirt -in "%s" -ref "%s" -out "%s" -dof %d -omat "%s" %s', 
    infile, reffile, outfile, dof, omat, opts))

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
  if (verbose & print.omat){
    cat(paste0("Output matrix not specified, but stored ", 
               "temporarily at ", omat, "\n"))
  }
  return(res)
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
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
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
fslhelp = function(func_name, help.arg = "--help",extra.args = ""){
    cmd = get.fsl()
    cmd <- paste0(cmd, sprintf('%s %s %s', func_name, 
                               help.arg, extra.args))
#     args = paste(help.arg, extra.args, sep=" ", collapse = " ")
    suppressWarnings({res = system(cmd, intern=TRUE)})
#     res = system2(func_name, args = args, stdout=TRUE, stderr=TRUE)
    cat(res, sep="\n")
    return(invisible(res))
}



#' @title Use FSL's Brain Extraction Tool (BET)
#' @description This function calls \code{bet} to extract a brain 
#' from an image, usually for skull stripping.
#' @param infile (character) input filename
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to \code{bet}
#' @param betcmd (character) Use \code{bet} or \code{bet2} function
#' @param verbose (logical) print out command before running 
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  infile = checkimg(infile, ...)  
  outfile = checkimg(outfile, ...)  
  outfile = nii.stub(outfile, ...)
  cmd <- paste0(cmd, sprintf('%s "%s" "%s" %s', 
                             betcmd, infile, outfile, opts))
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
  return(fslhelp(betcmd, help.arg="-h"))
}


#' @title Image Center of Gravity
#' @description Find Center of Gravity of Image, after thresholding
#' @param img Object of class nifti
#' @param thresh threshold for image, will find \code{img > 0}
#' @param ceil Run \code{\link{ceiling}} to force integers (usu for plotting)
#' @param warn Produce a warning if the image is empty after thresholding
#' @return Vector of length 3
#' @export
#' @examples
#' if (have.fsl()){
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' cog(img)
#' } 
cog = function(img, thresh = 0, ceil = FALSE, warn = TRUE){
#   stopifnot(inherits(img, "nifti"))
  mask = img > thresh
  if (sum(mask, na.rm = TRUE) == 0) {
    if (warn) {
      warning(paste0("No voxels found to be > ", round(thresh, 3), 
                     ", using the center of whole image"))
    }
    xyz = dim(mask) / 2
  } else {
    xyz = colMeans(which(mask, arr.ind = TRUE))
  }
  if (ceil) xyz = ceiling(xyz)
  return(xyz)
}



#' @title Image Center of Gravity Wrapper
#' @description Find Center of Gravity of Image, after thresholding and
#' take ceiling (wrapper for \code{\link{cog}})
#' @param ... Arguments ppssed to \code{\link{cog}}
#' @return Vector of length 3
#' @note Just a convenience wrapper for \code{cog(ceil=TRUE)}
#' @export
xyz = function(...){
  xyz = cog(..., ceil=TRUE)
  return(xyz)
}



#' @title Image Center of Gravity (FSL)
#' @description Find Center of Gravity of Image from FSL
#' @param img Object of class nifti, or path of file
#' @param mm Logical if the center of gravity (COG) would be in mm (default \code{TRUE})
#' or voxels (\code{FALSE})
#' @param verbose (logical) print out command before running 
#' @return Vector of length 3
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
fslcog = function(img, mm = TRUE, verbose = TRUE){
  opts = ifelse(mm, "-c", "-C")
  cog = fslstats(img, opts = opts, verbose = verbose)
  cog = as.numeric(strsplit(cog, " ")[[1]])
  cog
}



#' @title FSL Orient 
#' @description This function calls \code{fslorient}
#' @param file (character) image to be manipulated
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If \code{retimg}, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslorient}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  
  if (grepl("-get", opts) & retimg){
    warning(paste0("fslorient option was a -get, ",
                   "image was not changed - output not returned"))
  }  
  cmd = get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, sprintf('fslorient %s "%s"', opts, file))
  outfile = nii.stub(file)
  ext = get.imgext()  
  outfile = paste0(outfile, ext)
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
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
  return(fslhelp("fslorient", help.arg=""))
}



#' @title FSL Orient to MNI
#' @description This function calls \code{fslreorient2std}
#' @param file (character) image to be manipulated
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If \code{retimg}, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslreorient2std = function(
  file,
  retimg = TRUE,
  reorient = FALSE,
  intern = FALSE, 
  verbose = TRUE,
  ...){
  
  res = fslcmd(func="fslreorient2std", 
               file= file,
               outfile = NULL,
               retimg = retimg,
               reorient = reorient,
               intern = intern,
               opts = "",
               verbose = verbose,
               ... = ..., 
               samefile = TRUE)
    
  return(res)  
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
  return(fslhelp("fslreorient2std", help.arg=""))
}



#' @title FSL Swap Dimensions 
#' @description This function calls \code{fslswapdim}
#' @param file (character) image to be manipulated
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param a (character) Option for x domain in \code{fslswapdim}
#' @param b (character) Option for y domain in \code{fslswapdim}
#' @param c (character) Option for z domain in \code{fslswapdim}
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  
  res = fslcmd(func="fslswapdim", 
               file= file,
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
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{func} 
#' @param verbose (logical) print out command before running
#' @param samefile (logical) is the output the same file?
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
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
  ...){
  
  cmd = get.fsl()
  file = checkimg(file, ...)
  cmd <- paste0(cmd, sprintf('%s "%s"', func, file))
  no.outfile = is.null(outfile)
  if (no.outfile & samefile) outfile = ""  
  outfile = check_outfile(outfile=outfile, retimg=retimg, fileext = "")
  outfile = nii.stub(outfile)
  cmd <- paste(cmd, sprintf(' %s "%s";', opts, outfile))
  ext = get.imgext()
  if (verbose){
    cat(cmd, "\n")
  }
  res = system(cmd, intern=intern)
  outfile = paste0(outfile, ext)  
  if (retimg){
    if (samefile) outfile = file
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  } 
  
  return(res)  
}