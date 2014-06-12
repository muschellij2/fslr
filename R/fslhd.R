#' @name get.fsl
#' @title Get FSL's Location 
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
    cmd <- paste0("FSLDIR=", fsldir, "; ", 
                  "export FSLDIR; sh ${FSLDIR}/etc/fslconf/fsl.sh; ", 
                  "FSLOUTPUTTYPE=", fslout, "; export FSLOUTPUTTYPE; ", 
                  "$FSLDIR/bin/")
  } 
  if (fsldir %in% "" | is.null(fsldir)) stop("Can't find FSL")
  return(cmd)
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
    warning("Can't find FSLOUTPUTTYPE, going with NIFTI_GZ")
    fslout = "NIFTI_GZ"
  }
  if (fslout == "") {
    warning("Can't find FSLOUTPUTTYPE, going with NIFTI_GZ")
    fslout = "NIFTI_GZ"
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


#' @title Create temporary nii.gz file for FSL
#' @description Takes in a object of class nifit, writes it to a temp file, appends
#' .nii.gz as \code{\link{writeNIfTI}} adds it.
#' @param nim object of class nifti
#' @return filename of output nii.gz
#' @export
#' 
tempimg = function(nim){
  f = tempfile()
  nim = cal_img(nim)
  nim = zero_trans(nim)
  writeNIfTI(nim, filename= f, onefile = TRUE, gzipped = TRUE)
  f = paste0(f, ".nii.gz")
  return(f)
}

#' @title Check if filename is character or nifti object
#' @param file character or nifti object
#' @return character filename or temporary nii
#' @export
#' 
checkimg = function(file){
  if (inherits(file, "nifti")){
    return(tempimg(file))
  }
  if (inherits(file, "character")){
    file = path.expand(file)
    return(file)
  }
  stop("file not object of nifti or character")
  return(NULL)
}

#' @title FSL Maths Help
#' @description This function calls \code{fslmaths}'s help
#' @return Prints help output and returns output as character vector
#' @aliases fslsmooth.help fslmask.help fslerode.help fslfill.help 
#' fslsub2.help fslthresh.help
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
#' @param file (character) image to be smoothed
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) operations to be passed to \code{fslmaths}
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return If \code{retimg} then object of class nifit.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslmaths = function(
  file,
  outfile=NULL, 
  retimg = FALSE,
  reorient = FALSE,
  intern=TRUE, 
  opts = "", 
  ...){
  
  cmd = get.fsl()
  file = checkimg(file)
  cmd <- paste0(cmd, sprintf('fslmaths "%s"', file))
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  outfile = nii.stub(outfile)
  cmd <- paste(cmd, sprintf(' %s "%s";', opts, outfile))
  ext = get.imgext()
  
  res = system(cmd, intern=intern)
  outfile = paste0(outfile, ext)  
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  } 
  
  return(res)  
}



#' @title FSL Stats 
#' @description This function calls \code{fslstats}
#' @param file (character) filename of image to be checked
#' @param opts (character) operation passed to \code{fslstats}
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
fslstats <- function(file, opts=""){
  cmd <- get.fsl()
  file = checkimg(file)
  cmd <- paste0(cmd, sprintf('fslstats "%s" %s', file, opts))
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
#' @param outfile (character) resultant smoothed image name (optional)
#' if not give, will be the stub of the filename then _sigma
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param intern (logical) to be passed to \code{\link{system}}
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
  outfile=NULL, 
  retimg = FALSE,
  reorient = FALSE,
  intern=TRUE, 
  ...){
	
  cmd = get.fsl()
  file = checkimg(file)
	cmd <- paste0(cmd, sprintf('fslmaths "%s"', file))
	if (! is.null(mask)) {
    mask = checkimg(mask)
    cmd <- paste(cmd, sprintf(' -mas "%s"', mask))
	}
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  outfile = nii.stub(outfile)
	cmd <- paste(cmd, sprintf(' -s %s "%s";', sigma, outfile))
  ext = get.imgext()
  
  rm.mask.img = FALSE
	### tempfile for mask.stub
  if ( !is.null(mask) ) {
    rm.mask.img = TRUE
    mask = checkimg(mask)
    mask.stub <- basename(mask)
    mask.stub = nii.stub(mask.stub)
  	mask.stub <- file.path(dirname(mask), mask.stub)
  	mask.blur <- sprintf("%s_%s", mask.stub, sigma)
   	cmd <- paste(cmd, sprintf('fslmaths "%s" -s %s "%s";', 
   		mask, sigma, mask.blur))
   	cmd <- paste(cmd, sprintf('fslmaths "%s" -div "%s" -mas "%s" "%s";', 
   		outfile, mask.blur, mask, outfile))
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
                    retimg = FALSE,
                    reorient = FALSE,
                    intern=TRUE, opts="", ...){
	
  cmd = get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  outfile = nii.stub(outfile)
  file = checkimg(file)
	cmd <- paste0(cmd, sprintf('fslmaths "%s" -mas "%s" %s "%s"', 
		file, mask, opts, outfile))
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
                     retimg = FALSE,
                     reorient = FALSE,
                    intern=TRUE, kopts = "", opts="", 
                    ...){
  
  cmd = get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  outfile = nii.stub(outfile)
  file = checkimg(file)    
  cmd <- paste0(cmd, sprintf('fslmaths "%s" %s -ero %s "%s"', 
                            file, kopts, opts, outfile))
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




#' @title Get NIfTI header using FSL
#' @description This function calls \code{fslhd} to obtain a nifti header 
#' @param file (character) image to be masked
#' @param opts (character) additional options to be passed to fslhd
#' @return Character of infromation from fslhd
#' @export
fslhd <- function(file, opts=""){
	cmd <- get.fsl()
  file = checkimg(file)
	cmd <- paste0(cmd, sprintf('fslhd "%s" %s', file, opts))
	system(cmd, intern=TRUE)
}

#' @title FSLhd help
#' @description This function calls \code{fslhd}'s help
#' @return Prints help output and returns output as character vector
#' @export
fslhd.help = function(){
  return(fslhelp("fslhd", help.arg=""))
}

#' @title Parse FSL Header
#' @description This function takes in a FSL header and parses the components
#' @param hd (character) header from \code{\link{fslhd}}
#' @return data.frame of information from FSL header
#' @export
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
#' @return list with elements of sform and qform and their respective codes
#' @export
getForms <- function(file){
  file = checkimg(file)  
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
#' @return result of \code{\link{checkout}}
#' @export
check_file <- function(file){
  file = checkimg(file)  
	hd <- getForms(file)
	checkout(hd)
}

check_sform <- function(hd, value=0){
	hd$sform_code == value
}


check_sform_file <- function(file, value=0){
  file = checkimg(file)  
	hd <- getForms(file)
	check_sform(hd, value=value)
}
## if sign(det(res$sform)) == sign(det(res$qform)) and 
## res$ssor[1] == res$sqor[1] then all good


#' @title Get range of an image
#' @description This function calls \code{fslstats -R} to get the range of an image
#' @param file (character) filename of image to be checked
#' @return numeric vector of length 2
#' @import stringr
#' @export
fslrange <- function(file){
	cmd <- get.fsl()
	file = checkimg(file)
	cmd <- paste0(cmd, sprintf('fslstats "%s" -R', file))
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
                   retimg = FALSE,
                   reorient = FALSE,
                   intern=TRUE, ...){
  cmd <- get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }  
  outfile = nii.stub(outfile)
  
  file = checkimg(file)    
  runbin = ""
  if (bin) runbin = "-bin"
  cmd <- paste0(cmd, sprintf('fslmaths "%s" %s -fillh "%s"', file, 
                            runbin, outfile))
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
                     retimg = FALSE,
                     reorient = FALSE,
                     intern=TRUE, 
                     opts = "", ...){
  cmd <- get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }  
  outfile = nii.stub(outfile)
  
  file = checkimg(file)  
  
  if (!is.null(uthresh)){
    opts = paste(sprintf("-uthr %f", uthresh), opts)
  }
  cmd <- paste0(cmd, sprintf('fslmaths "%s" -thr %f %s "%s"', 
  	file, thresh, opts, outfile))
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
                   retimg = FALSE,
                   reorient = FALSE,
                   intern=TRUE, ...){
  cmd <- get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }  
  outfile = nii.stub(outfile)
  
  file = checkimg(file)  
  
  cmd <- paste0(cmd, sprintf('fslmaths "%s" -subsamp2 "%s"', 
                            file, outfile))
  res = system(cmd, intern=intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }
  return(res)
}

#' @title Open image in FSLView
#' @description This function calls \code{fslview} to view an image 
#' in the FSL viewer
#' @param file (character) filename of image to be thresholded
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) options for FSLView
#' @return character or logical depending on intern
#' @export
fslview = function(file, intern=TRUE, opts =""){
  cmd <- get.fsl()
  file = checkimg(file)
  cmd <- paste(cmd, sprintf('fslview "%s" %s', file, opts))
  res = system(cmd, intern=intern)
  return(res)
}

#' @title FSLView help
#' @description This function calls \code{fslview}'s help
#' @return Prints help output and returns output as character vector
#' @export
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
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
fslmerge = function(infiles, 
                   direction = c("x", "y", "z", "t", "a"), 
                   outfile = NULL, 
                   retimg = FALSE,
                   reorient = FALSE,                   
                   intern=TRUE, ...){
  cmd <- get.fsl()
  direction = direction[1]
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
    }
  } else {
    stopifnot(!is.null(outfile))
  }   
  outfile = nii.stub(outfile)  
  file = checkimg(file)  
  
  cmd <- paste0(cmd, sprintf('fslmerge "%s" -%s "%s"', 
                            outfile, direction, infiles))
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
fslmerge.help = function(){
  return(fslhelp("fslmerge"))
}



#' @title Register using FLIRT
#' @description This function calls \code{fslirt} to register infile to reffile
#' and either saves the image or returns an object of class nifti, along with the
#' transformation matrix omat  
#' @param infile (character) input filename
#' @param reffile (character) reference image to be registered to
#' @param omat (character) Output matrix name
#' @param dof (numeric) degrees of freedom
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to FLIRT
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return character or logical depending on intern
#' @export
flirt = function(infile, 
                    reffile, omat,
                    dof,
                    outfile = NULL,                  
                    retimg = FALSE,
                    reorient = FALSE,                 
                    intern=TRUE,
                    opts="", ...){
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
  infile = checkimg(infile)  
  reffile = checkimg(reffile)  
  outfile = checkimg(outfile)  
  outfile = nii.stub(outfile)

  omat = path.expand(omat)
  cmd <- paste0(cmd, sprintf('flirt -in "%s" -ref "%s" -out "%s" -dof %d %s', 
                            infile, reffile, outfile, dof, opts))
  res = system(cmd, intern=intern)
  ext = get.imgext()
  outfile = paste0(outfile, ext)  
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient, ...)
    return(img)
  }
  return(res)
}


#' @title FLIRT help
#' @description This function calls \code{flirt}'s help
#' @return Prints help output and returns output as character vector
#' @export
flirt.help = function(){
  return(fslhelp("flirt"))
}


#' @title Run MELODIC ICA
#' @description This function calls \code{melodic} 
#' @param file (character) image to be run
#' @param outdir (character) output directory. 
#' (Default \code{dirname(file)})
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) options for melodic
#' @return character or logical depending on intern
#' @export
melodic = function(file, 
                   outdir = dirname(file), 
                   intern=TRUE,                   
                   opts =""){
  cmd <- get.fsl()
  file = path.expand(outdir)

  outdir = path.expand(outdir)
  stopifnot(file.exists(outdir))
  file = checkimg(file)  

  cmd <- paste0(cmd, sprintf('melodic --in "%s" --outdir "%s" %s', 
                             file, outdir, opts))
  res = system(cmd, intern=intern)
  return(res)
}

#' @title MELODIC help
#' @description This function calls \code{melodic}'s help
#' @return Prints help output and returns output as character vector
#' @export
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
    get.fsl()
    args = paste(help.arg, extra.args, sep=" ", collapse = " ")
    res = system2(func_name, args = args, stdout=TRUE, stderr=TRUE)  
    cat(res, sep="\n")
    return(invisible(res))
}