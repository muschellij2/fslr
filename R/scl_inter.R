#' @name scl_inter
#' @title Extract NIfTI 3D Image scl_inter attribute
#' @docType methods 
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to scl_inter 
#' @description Methods that act on the ``scl_inter'' in the NIfTI header.
#' @rdname scl_inter-methods
#' @aliases scl_inter-methods 
#' @aliases scl_inter
#' @export
setGeneric("scl_inter", function(object) standardGeneric("scl_inter"))

#' @name scl_inter
#' @rdname scl_inter-methods
#' @aliases scl_inter,nifti-method
setMethod("scl_inter", "nifti", function(object) { object@"scl_inter" })

#' @name scl_inter
#' @rdname scl_inter-methods
#' @aliases scl_inter,character-method   
setMethod("scl_inter", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "scl_inter", verbose = FALSE)
  res = as.numeric(res)
})

#' @name scl_inter
#' @rdname scl_inter-methods
#' @aliases scl_inter<- 
setGeneric("scl_inter<-", function(object, value) { standardGeneric("scl_inter<-") })

#' @name scl_inter
#' @rdname scl_inter-methods
#' @aliases scl_inter<-,nifti-method
setMethod("scl_inter<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"scl_inter" <- value 
            return(object)
          })

