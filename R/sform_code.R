#' @name sform_code
#' @title Extract NIfTI 3D Image sform_code attribute
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to sform_code 
#' @description Methods that act on the ``sform_code'' in the NIfTI header.
#' @rdname sform_code-methods
setGeneric("sform_code", function(object) standardGeneric("sform_code"))

#' @name sform_code
#' @rdname sform_code-methods
#' @exportMethod sform_code
setMethod("sform_code", "nifti", function(object) { object@"sform_code" })


#' @name sform_code
#' @rdname sform_code-methods
setGeneric("sform_code<-", function(object, value) { standardGeneric("sform_code<-") })

#' @name sform_code
#' @rdname sform_code-methods
#' @exportMethod sform_code<-
setMethod("sform_code<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"sform_code" <- value 
            return(object)
          })


#' @name sform_code
#' @rdname sform_code-methods
#' @exportMethod sform_code
setMethod("sform_code", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "sform_code", verbose = FALSE)
  res = as.numeric(res)
})
