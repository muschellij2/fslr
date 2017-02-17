#' @title Scale Affine Matrix using \code{avscale}
#' @description This function calls \code{avscale} to get individual
#' matrices for FSL
#' @param file (character) matrix filename 
#' @param volume (character) non-reference volume filename or nifti image
#' @param parsed (logical) should \code{\link{parse_avscale}} be run 
#' after?
#' @param verbose (logical) print out command before running 
#' @return Character of information from avscale
#' @export
fsl_avscale <- function(file, volume = NULL,
                        parsed = TRUE, verbose = TRUE){
  cmd <- get.fsl()
  add_volume = !is.null(volume)
  if (add_volume) {
    volume = checkimg(volume)
  }
  file = path.expand(file)
  cmd <- paste0(
    cmd, 
    sprintf('avscale %s %s', shQuote(file), 
            ifelse(add_volume, shQuote(volume), ""))
  )
  if (verbose) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = TRUE)
  if (parsed) {
    res = parse_avscale(res)  
  }
  return(res)
}

#' @rdname fsl_avscale
#' @param ... not used, but used for duplicating \code{avscale} as alias 
#' @export
avscale = function(...) {
  res = fsl_avscale(...) 
  return(res)
}


#' @title Parse output from \code{avscale}
#' @description This function parses the output from 
#' \code{\link{fsl_avscale}} into something more manageable
#' @param av_out output from \code{\link{fsl_avscale}}, character vector
#' @return List of output values
#' 
#' @export
parse_avscale = function(av_out) {
  res = trimws(av_out)
  if (res[length(res)] == "") {
    res = res[-length(res)]
  }
  df = data.frame(x = res, 
                  skip = res == "",
                  stringsAsFactors = FALSE)
  df$ind = cumsum(df$skip)
  
  df = split(df, df$ind)
  
  
  
  
  df = lapply(df, function(x) {
    x = x$x
    x = x[ x != ""]
  })
  
  vals = c(rotation_matrix = "rotation.*matrix", 
           fwd_half_transform = "forward.*transform",
           back_half_transform = "backward.*transform",
           scales = "scales",
           skews = "skews",
           avg_scaling = "scaling",
           determinant = "determinant",
           orientation = "orientation"
  )
  
  rot = sapply(vals, function(x) {
    sapply(df, function(r) {
      any(grepl(x, tolower(r)))
    })
  })
  
  ####################################
  # Making sure formats haven't changed
  ####################################  
  rs = rowSums(rot)
  if (!all(rs %in% c(1, 2))) {
    warning("There may be some problem with this transform, do not trust")
  }
  
  xdf = df # need xdf later
  
  ####################################
  # removing text
  ####################################
  df = lapply(df, function(x) {
    x = gsub(".*(=|:)(.*)", "\\2", x)
    x = trimws(x)
    x = x[ x != ""]
    x
  })

  ############################
  # Splitting off orientation/determinant
  ############################
  orien_ind = apply(rot, 2, which)
  if (  orien_ind["determinant"] != orien_ind["orientation"] ) {
    warning("There may be some problem with this transform, do not trust")
  }
  
  ############################
  # Making sure the only one with 2 is determinant/orientation
  ############################
  rs = rs[-orien_ind["determinant"]]
  if (!all(rs == 1)) {
    warning("There may be some problem with this transform, do not trust")
  }  
  
  ####################################
  # splitting off the orientation/det data
  ####################################
  orien = df[[orien_ind["determinant"]]]
  xorien = xdf[[orien_ind["determinant"]]]
  df = df[ -orien_ind["determinant"]]
  
  orien_ind = orien_ind[ orien_ind != orien_ind["determinant"]]
  orien_ind = sort(orien_ind)
  
  names(df) = names(orien_ind)

  ####################################
  # making numbers and vectors
  ####################################
  df = lapply(df, function(x) {
    x = strsplit(x, " ")
    x = lapply(x, as.numeric)
    x = do.call("rbind", x)
    if (nrow(x) == 1 || ncol(x) == 1) {
      x = as.numeric(x)
    }
    return(x)
  })
  
  determinant = orien[grep("determinant", tolower(xorien))]
  determinant = as.numeric(determinant)
  orientation = orien[grep("orientation", tolower(xorien))]
  
  df$determinant = determinant
  df$orientation = orientation

  return(df)
}
