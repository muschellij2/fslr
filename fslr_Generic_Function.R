#' @name %%-methods
#' @title Extract Image %% attribute
#' @description Methods that extract the ``%ff%'' slot from the 
#' NIfTI/ANALYZE header.
#' @docType methods 
#' @param object is an object of class \code{character} 
#' @aliases %%-methods 
#' @aliases %%,character,character-method
#' @aliases %%
#' @export
%example% 
setMethod("%%", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "%ff%", verbose = FALSE)
  res = %numeric%(res)
  return(res)
})
