#' @name %%
#' @title Extract NIfTI 3D Image %% attribute
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to %% 
#' @description Methods that act on the ``%%'' in the NIfTI header.
#' @rdname %%-methods
#' @export
setGeneric("%%", function(object) standardGeneric("%%"))

#' @describeIn %%-methods
#' @export
setMethod("%%", "nifti", function(object) { object@"%%" })

#' @describeIn %%-methods
#' @export
setGeneric("%%<-", function(object, value) { standardGeneric("%%<-") })

#' @describeIn %%-methods
#' @aliases %%<-,nifti,nifti-method   
#' @export
setMethod("%%<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"%%" <- value 
            return(object)
          })


#' @describeIn %%-methods
#' @aliases %%,character,character-method   
setMethod("%%", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "%%", verbose = FALSE)
  res = as.%type(res)
})