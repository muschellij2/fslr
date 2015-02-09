#' @name scl_slope
#' @title Extract NIfTI 3D Image scl_slope attribute
#' @docType methods 
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to scl_slope 
#' @description Methods that act on the ``scl_slope'' in the NIfTI header.
#' @rdname scl_slope-methods
#' @aliases scl_slope-methods 
#' @aliases scl_slope
#' @export
setGeneric("scl_slope", function(object) standardGeneric("scl_slope"))

#' @name scl_slope
#' @rdname scl_slope-methods
#' @aliases scl_slope,nifti-method
setMethod("scl_slope", "nifti", function(object) { object@"scl_slope" })

#' @name scl_slope
#' @rdname scl_slope-methods
#' @aliases scl_slope,character-method   
setMethod("scl_slope", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "scl_slope", verbose = FALSE)
  res = as.numeric(res)
})

#' @name scl_slope
#' @rdname scl_slope-methods
#' @aliases scl_slope<- 
setGeneric("scl_slope<-", function(object, value) { standardGeneric("scl_slope<-") })

#' @name scl_slope
#' @rdname scl_slope-methods
#' @aliases scl_slope<-,nifti-method
setMethod("scl_slope<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"scl_slope" <- value 
            return(object)
          })

