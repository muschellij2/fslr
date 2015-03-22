#' @docType methods
#' @rdname %ff%-methods
#' @title Extract Image %% attribute 
#' @name %%-methods
#' @aliases %%,character-method
#' @import oro.nifti
#' @param object is a filename to pass to \link{fslval}
#' @export
%example% 
setMethod("%%", "character", function(object) { 
  object = path.expand(object)
  stopifnot(file.exists(object))
  res = fslval(object, keyword = "%ff%", verbose = FALSE)
  res = %numeric%(res)
  return(res)
})
