## Overloading binary operators
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @title Operations for NIfTI Objects
#' @name niftiops
#' @rdname niftiops
#' @aliases Ops,nifti,nifti-method
#' @param e1 object
#' @param e2 object
setMethod("Ops", signature(e1="nifti", e2="nifti"),
          function(e1, e2) {
            e1@.Data = callGeneric(e1@.Data, e2@.Data)
            e1 = zero_trans(e1)
            cr = range(e1, na.rm=TRUE)      
            cal.max(e1) = cr[2]
            cal.min(e1) = cr[1]
            e1 = drop_img_dim(e1)
            validObject(e1)
            return(e1)
          }
)
#' @rdname niftiops
#' @aliases Ops,nifti,numeric-method
setMethod("Ops", signature(e1="nifti", e2="numeric"),
          function(e1, e2) {
            e1@.Data = callGeneric(e1@.Data, e2)
            e1 = zero_trans(e1)            
            cr = range(e1, na.rm=TRUE)      
            cal.max(e1) = cr[2]
            cal.min(e1) = cr[1]
            e1 = drop_img_dim(e1)
            validObject(e1)
            return(e1)
          }
)
#' @rdname niftiops
#' @aliases Ops,numeric,nifti-method
setMethod("Ops", signature(e1="numeric", e2="nifti"),
          function(e1, e2) {
            e2@.Data = callGeneric(e1, e2@.Data)
            e1 = e2
            e1 = zero_trans(e1)            
            cr = range(e1, na.rm=TRUE)      
            cal.max(e1) = cr[2]
            cal.min(e1) = cr[1]
            e1 = drop_img_dim(e1)
            validObject(e1)
            return(e1)
          }
)