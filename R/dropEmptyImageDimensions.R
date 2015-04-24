#' @title Drop Empty Image Dimensions
#' @return Object of class nifti
#' @name dropEmptyImageDimensions
#' @param img nifti object
#' @param value Value to check against.  If zero, then 
#' \code{dropEmptyImageDimensions} will drop any dimension that has 
#' all zeroes
#' @param threshold Drop dimension if fewer than \code{threshold} voxels
#' are in the slice
#' @param other.imgs List of other nifti objects or filenames 
#' to apply the same dropping as \code{img}.
#' @param reorient Should image be reoriented if a filename?
#' @description Drops dimensions of an image that has all irrelevant
#' values
#' @export
dropEmptyImageDimensions <- function(img, 
                                     value = 0, 
                                     threshold = 0,
                                     other.imgs = NULL, 
                                     reorient = FALSE) {
  
  img = check_nifti(img, reorient = reorient)
  if (dim_(img)[1] > 3){
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by dim_"))
  }
  ############################
  # Get indices for slices with all zeros (or of certain value)
  ############################
  inds = vector(mode = "list", length = 3)
  for (i in 1:3){
    zero_x = apply(img, i, function(x) sum(x != value))
    dzero_x = !(
      cumsum(zero_x) <= threshold | 
        rev( cumsum(rev(zero_x)) <= threshold )
    )
    inds[[i]] = which(dzero_x)
    #     print(i)
  }
  ############################
  # Get matrix of indices
  ############################
  all.inds = as.matrix(expand.grid(inds))
  
  ############################
  # Subset the image
  ############################
  i2 = img[inds[[1]], inds[[2]], inds[[3]]]
  
  outimg = nifti(img = i2, dim = dim(i2),
                 datatype = datatype(img), cal.min = min(i2, na.rm=TRUE),
                 cal.max = max(i2, na.rm=TRUE), 
                 pixdim = pixdim(img))
  if (!is.null(other.imgs)){
    other.imgs = list(other.imgs)
    other.imgs = lapply(other.imgs, 
                        check_nifti, reorient = reorient)
    other.imgs = lapply(other.imgs, function(oimg){
      i2 = oimg[inds[[1]], inds[[2]], inds[[3]]]
      newimg = nifti(img = i2, dim = dim(i2),
                     datatype = datatype(oimg), 
                     cal.min = min(i2, na.rm=TRUE),
                     cal.max = max(i2, na.rm=TRUE), 
                     pixdim = pixdim(oimg))
      return(newimg)
    })
    return(list(outimg = outimg, other.imgs = other.imgs))
  }
  return(outimg)
}