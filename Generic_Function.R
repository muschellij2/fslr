#' @name %%
#' @title Extract NIfTI 3D Image %% attribute
#' @param object is an object of class \code{nifti}
#' @param value Value to assign to %% 
#' @description Methods that act on the ``%%'' in the NIfTI header.
#' @rdname %%-methods
setGeneric("%%", function(object) standardGeneric("%%"))

#' @name %%
#' @rdname %%-methods
#' @exportMethod %%
setMethod("%%", "nifti", function(object) { object@"%%" })


#' @name %%
#' @rdname %%-methods
setGeneric("%%<-", function(object, value) { standardGeneric("%%<-") })

#' @name %%
#' @rdname %%-methods
#' @exportMethod %%<-
setMethod("%%<-", 
          signature(object="nifti"), 
          function(object, value) { 
            object@"%%" <- value 
            return(object)
          })


#' @name %%
#' @rdname %%-methods
#' @exportMethod %%
setMethod("%%", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "%%", verbose = FALSE)
  res = as.%type(res)
})