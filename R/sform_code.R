#' @name sform_code
#' @title Extract NIfTI 3D Image sform_code attribute
#' @docType methods 
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to sform_code 
#' @description Methods that act on the ``sform_code'' in the NIfTI header.
#' @rdname sform_code-methods
#' @aliases sform_code-methods 
#' @aliases sform_code
#' @export
setGeneric("sform_code", function(object) standardGeneric("sform_code"))

#' @name sform_code
#' @rdname sform_code-methods
#' @aliases sform_code,nifti-method
setMethod("sform_code", "nifti", function(object) { object@"sform_code" })

#' @name sform_code
#' @rdname sform_code-methods
#' @aliases sform_code,character-method   
setMethod("sform_code", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "sform_code", verbose = FALSE)
  res = as.numeric(res)
})

#' @name sform_code
#' @rdname sform_code-methods
#' @aliases sform_code<- 
setGeneric("sform_code<-", function(object, value) { standardGeneric("sform_code<-") })

#' @name sform_code
#' @rdname sform_code-methods
#' @aliases sform_code<-,nifti-method
setMethod("sform_code<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"sform_code" <- value 
            return(object)
          })

