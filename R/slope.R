#' @name slope
#' @title Extract NIfTI 3D Image slope attribute
#' @docType methods 
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to slope 
#' @description Methods that act on the ``slope'' in the NIfTI header.
#' @rdname slope-methods
#' @aliases slope-methods 
#' @aliases slope
#' @export
setGeneric("slope", function(object) standardGeneric("slope"))

#' @name slope
#' @rdname slope-methods
#' @aliases slope,nifti-method
setMethod("slope", "nifti", function(object) { object@"slope" })

#' @name slope
#' @rdname slope-methods
#' @aliases slope,character-method   
setMethod("slope", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "slope", verbose = FALSE)
  res = as.numeric(res)
})

#' @name slope
#' @rdname slope-methods
#' @aliases slope<- 
setGeneric("slope<-", function(object, value) { standardGeneric("slope<-") })

#' @name slope
#' @rdname slope-methods
#' @aliases slope<-,nifti-method
setMethod("slope<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"slope" <- value 
            return(object)
          })

