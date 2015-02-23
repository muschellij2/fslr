#' @name glmin
#' @title Extract NIfTI 3D Image glmin attribute
#' @docType methods 
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to glmin 
#' @description Methods that act on the ``glmin'' in the NIfTI header.
#' @rdname glmin-methods
#' @aliases glmin-methods 
#' @aliases glmin
#' @export
setGeneric("glmin", function(object) standardGeneric("glmin"))

#' @name glmin
#' @rdname glmin-methods
#' @aliases glmin,nifti-method
setMethod("glmin", "nifti", function(object) { object@"glmin" })

#' @name glmin
#' @rdname glmin-methods
#' @aliases glmin,character-method   
setMethod("glmin", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "glmin", verbose = FALSE)
  res = as.numeric(res)
})

#' @name glmin
#' @rdname glmin-methods
#' @aliases glmin<- 
setGeneric("glmin<-", function(object, value) { standardGeneric("glmin<-") })

#' @name glmin
#' @rdname glmin-methods
#' @aliases glmin<-,nifti-method
setMethod("glmin<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"glmin" <- value 
            return(object)
          })

