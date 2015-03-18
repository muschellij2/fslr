#' @title Logical Operators
#' @name Logic
#' @rdname Logic
setMethod("-",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data - e2@.Data)
          }
)

#' @rdname Logic
setMethod("+",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data + e2@.Data)
          }
)

#' @rdname Logic
setMethod("*",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data * e2@.Data)
          }
)

#' @rdname Logic
setMethod("/",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data * e2@.Data)
          }
)

#' @rdname Logic
setMethod("&",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data & e2@.Data)
          }
)

#' @rdname Logic
setMethod("|",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data | e2@.Data)
          }
)