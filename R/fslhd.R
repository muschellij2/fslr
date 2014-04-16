#' @name get.fsl
#' @title Determine FSL Directory
#' @return NULL if FSL in path, or bash code for setting up FSL DIR
#' @export
get.fsl = function(){
  cmd = NULL
  fsldir = Sys.getenv("FSLDIR")
  if (fsldir == "") {
    fsldir = getOption("fsl.path")
    cmd <- paste0("FSLDIR=", fsldir, "; ", 
                  "export FSLDIR; sh ${FSLDIR}/etc/fslconf/fsl.sh; ")    
  } 
  if (fsldir %in% "" | is.null(fsldir)) stop("Can't find FSL")
  return(cmd)
}

#' @name fslsmooth
#' @title Gaussian smooth image using FSL
#' @param file (character) image to be smoothed
#' @param mask (character) optional mask given for image
#' @param outfile (character) resultant smoothed image name (optional)
#' if not give, will be the stub of the filename then _sigma
#' @param sigma (numeric) sigma (in mm) of Gaussian kernel for smoothing
#' @param intern (logical) to be passed to \code{\link{system}}
#' @return Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslsmooth <- function(
  file,
  mask=NULL, 
  outfile=NULL, 
	sigma=10, intern=TRUE){
	
  cmd = get.fsl()

	cmd <- paste0(cmd, sprintf('fslmaths "%s"', file))
	if (! is.null(mask)) cmd <- paste(cmd, sprintf(' -mas "%s"', mask))
	if (is.null(outfile)) {
    outfile = nii.stub(file)
		outfile <- sprintf("%s_%s", outfile, sigma)
	}
	cmd <- paste(cmd, sprintf(' -s %s "%s";', sigma, outfile))

	### tempfile for mask.stub
  if ( !is.null(mask) ) {
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
	x = file.remove(paste0(mask.blur, ".nii.gz"))
  x = file.remove(paste0(mask.blur, ".nii"))
  return(res)
}

#' @name fslmask
#' @title Mask image using FSL
#' @param file (character) image to be masked
#' @param mask (character) mask given for image
#' @param outfile (character) resultant masked image name (optional)
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) additional options to be passed to fslmask
#' @return Result from system command, depends if intern is TRUE or FALSE.
#' @export
fslmask <- function(file, mask=NULL, outfile=NULL, 
	intern=TRUE, opts=""){
	
  cmd = get.fsl()
	cmd <- paste(cmd, sprintf('fslmaths "%s" -mas "%s" %s "%s"', 
		file, mask, opts, outfile))
	res = system(cmd, intern=intern)
  return(res)
}

#' @name fslerode
#' @title Erode image using FSL
#' @param file (character) image to be eroded
#' @param outfile (character) resultant eroded image name 
#' @param retimg (logical) Should the result be the eroded image?
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param opts (character) additional options to be passed to fslmaths
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param ... additional arguments passed to \code{\link{readNIfTI}}.
#' @return Result from system command, depends if intern is TRUE or FALSE.  If 
#' retimg is TRUE, then the image will be returned. 
#' @import oro.nifti
#' @export
fslerode <- function(file, outfile=NULL, retimg = FALSE,
                    intern=TRUE, opts="", reorient= FALSE,...){
  
  cmd = get.fsl()
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
      trash = TRUE
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  outfile = nii.stub(outfile)
  cmd <- paste(cmd, sprintf('fslmaths "%s" -ero %s "%s"', 
                            file, opts, outfile))
  res = system(cmd, intern=intern)
  outfile = paste0(outfile, ".nii.gz")
  stopifnot(file.exists(outfile))
  if (retimg){
    img = readNIfTI(outfile, reorient=reorient)
    return(img)
  }
  return(res)
}




#' @title Get NIfTI header using FSL
#' @param file (character) image to be masked
#' @param opts (character) additional options to be passed to fslhd
#' @return Character of infromation from fslhd
#' @export
fslhd <- function(file, opts=""){
	cmd <- get.fsl()
	cmd <- paste(cmd, sprintf('fslhd "%s" %s', file, opts))
	system(cmd, intern=TRUE)
}

#' @title Parse FSL Header
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
#' @param file (character) filename of image to pass to header
#' @return list with elements of sform and qform and their respective codes
#' @export
getForms <- function(file){
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
	hd <- getForms(file)
	checkout(hd)
}

check_sform <- function(hd, value=0){
	hd$sform_code == value
}


check_sform_file <- function(file, value=0){
	hd <- getForms(file)
	check_sform(hd, value=value)
}
## if sign(det(res$sform)) == sign(det(res$qform)) and 
## res$ssor[1] == res$sqor[1] then all good


#' @title Get range of an image
#' @param file (character) filename of image to be checked
#' @return numeric vector of length 2
#' @import stringr
#' @export
fslrange <- function(file){
	cmd <- get.fsl()
  file = path.expand(file)
	cmd <- paste(cmd, sprintf('fslstats "%s" -R', file))
  x = str_trim(system(cmd, intern = TRUE))
  x = strsplit(x, " ")[[1]]
  x = as.numeric(x)
  x
}

#' @title Fill image holes
#' @param file (character) filename of image to be filled
#' @param outfile (character) name of resultant filled file
#' @param bin (logical) binarize the image before filling
#' @param intern (logical) pass to \code{\link{system}}
#' @return character or logical depending on intern
#' @export
fslfill = function(file, outfile = file, bin=TRUE, intern=TRUE){
  cmd <- get.fsl()
  runbin = ""
  if (bin) runbin = "-bin"
  cmd <- paste(cmd, sprintf('fslmaths "%s" %s -fillh "%s"', file, 
                            runbin, outfile))
  system(cmd, intern=intern)
}

#' @title Threshold an image
#' @param file (character) filename of image to be thresholded
#' @param outfile (character) name of resultant thresholded file
#' @param thresh (numeric) threshold (anything below set to 0)
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) additional options to be passed to fslmaths 
#' @return character or logical depending on intern
#' @export
fslthresh = function(file, outfile = file, 
                     thresh = 0, intern=TRUE, 
                     opts = ""){
  cmd <- get.fsl()
  cmd <- paste(cmd, sprintf('fslmaths "%s" -thr %f %s "%s"', 
  	file, thresh, opts, outfile))
  system(cmd, intern=intern)
}

#' @title Subsample image by factor of 2
#' @param file (character) filename of image to be thresholded
#' @param outfile (character) name of resultant subsampled file
#' @param intern (logical) pass to \code{\link{system}}
#' @return character or logical depending on intern
#' @export
fslsub2 = function(file, outfile = file, intern=TRUE){
  cmd <- get.fsl()
  cmd <- paste(cmd, sprintf('fslmaths "%s" -subsamp2 "%s"', file, outfile))
  system(cmd, intern=intern)
}

#' @title Open image in FSLView
#' @param file (character) filename of image to be thresholded
#' @param intern (logical) pass to \code{\link{system}}
#' @param opts (character) options for FSLView
#' @return character or logical depending on intern
#' @export
fslview = function(file, intern=TRUE, opts =""){
  cmd <- get.fsl()
  cmd <- paste(cmd, sprintf('fslview "%s" %s', file, opts))
  system(cmd, intern=intern)
}


#' @title Merge images using FSL
#' @param infiles (character) input filenames
#' @param outfile (character) output filename
#' @param direction (character) direction to merge over, x, y, z, 
#' t (time), a (auto)
#' @param intern (logical) pass to \code{\link{system}}
#' @return character or logical depending on intern
#' @export
fslmerge = function(infiles, outfile, 
                   direction = c("x", "y", "z", "t", "a"), 
                   intern=TRUE){
  cmd <- get.fsl()
  direction = direction[1]
  cmd <- paste(cmd, sprintf('fslmerge "%s" -%s "%s"', 
                            outfile, direction, infiles))
  system(cmd, intern=intern)
}
