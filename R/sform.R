#' @rdname orientation-methods
#' @title Extract NIfTI 3D Image Orientation
#' @aliases sform,character
setMethod("sform", "character", function(object){
  object = path.expand(object)
  stopifnot(file.exists(object))
  slots = paste0("sto_xyz:", 1:3)
  res = sapply(slots, function(key) {
    fslval(object, keyword = key, verbose = FALSE)
  })  
  convmat <- function(form){
    ss <- strsplit(form, " ")
    ss <- t(sapply(ss, function(x) x[x!=""]))
    class(ss) <- "numeric"
    return(ss)
  }
  res = convmat(res)
  rownames(res) = NULL  
  return(res)
})
