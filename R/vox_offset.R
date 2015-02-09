#' @name vox_offset
#' @title Extract NIfTI 3D Image vox_offset attribute
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to vox_offset 
#' @description Methods that act on the ``vox_offset'' in the NIfTI header.
#' @rdname vox_offset-methods
setGeneric("vox_offset", function(object) standardGeneric("vox_offset"))

#' @name vox_offset
#' @rdname vox_offset-methods
#' @exportMethod vox_offset
setMethod("vox_offset", "nifti", function(object) { object@"vox_offset" })


#' @name vox_offset
#' @rdname vox_offset-methods
setGeneric("vox_offset<-", function(object, value) { standardGeneric("vox_offset<-") })

#' @name vox_offset
#' @rdname vox_offset-methods
#' @exportMethod vox_offset<-
setMethod("vox_offset<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"vox_offset" <- value 
            return(object)
          })


#' @name vox_offset
#' @rdname vox_offset-methods
#' @exportMethod vox_offset
setMethod("vox_offset", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "vox_offset", verbose = FALSE)
  res = as.numeric(res)
})
