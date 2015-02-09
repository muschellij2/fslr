#' @name qform_code
#' @title Extract NIfTI 3D Image qform_code attribute
#' @docType methods 
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to qform_code 
#' @description Methods that act on the ``qform_code'' in the NIfTI header.
#' @rdname qform_code-methods
#' @aliases qform_code-methods 
#' @aliases qform_code
#' @export
setGeneric("qform_code", function(object) standardGeneric("qform_code"))

#' @name qform_code
#' @rdname qform_code-methods
#' @aliases qform_code,nifti-method
setMethod("qform_code", "nifti", function(object) { object@"qform_code" })

#' @name qform_code
#' @rdname qform_code-methods
#' @aliases qform_code,character-method   
setMethod("qform_code", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "qform_code", verbose = FALSE)
  res = as.numeric(res)
})

#' @name qform_code
#' @rdname qform_code-methods
#' @aliases qform_code<- 
setGeneric("qform_code<-", function(object, value) { standardGeneric("qform_code<-") })

#' @name qform_code
#' @rdname qform_code-methods
#' @aliases qform_code<-,nifti-method
setMethod("qform_code<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"qform_code" <- value 
            return(object)
          })

