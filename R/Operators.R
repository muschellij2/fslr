###################
# Subtraction
###################
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

setMethod("-",
          signature(e1 = "nifti", e2 = "array"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data - e2)
          }
)

setMethod("-",
          signature(e1 = "array", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e2, e1 - e2@.Data)
          }
)



###################
# Addition
###################
#' @rdname Logic
setMethod("+",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data + e2@.Data)
          }
)

setMethod("+",
          signature(e1 = "nifti", e2 = "array"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data + e2)
          }
)

setMethod("+",
          signature(e1 = "array", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e2, e1 + e2@.Data)
          }
)

###################
# Multiplication
###################
#' @rdname Logic
setMethod("*",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data * e2@.Data)
          }
)

setMethod("*",
          signature(e1 = "nifti", e2 = "array"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data * e2)
          }
)

setMethod("*",
          signature(e1 = "array", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e2, e1 * e2@.Data)
          }
)

###################
# Division
###################
#' @rdname Logic
setMethod("/",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data / e2@.Data)
          }
)

setMethod("/",
          signature(e1 = "nifti", e2 = "array"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data / e2)
          }
)

setMethod("/",
          signature(e1 = "array", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e2, e1 / e2@.Data)
          }
)

###################
# AND
###################
#' @rdname Logic
setMethod("&",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data & e2@.Data)
          }
)

setMethod("&",
          signature(e1 = "nifti", e2 = "array"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data & e2)
          }
)

setMethod("&",
          signature(e1 = "array", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e2, e1 & e2@.Data)
          }
)

###################
# OR
###################
#' @rdname Logic
setMethod("|",
          signature(e1 = "nifti", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data | e2@.Data)
          }
)

setMethod("|",
          signature(e1 = "nifti", e2 = "array"),
          function (e1, e2) 
          {
            niftiarr(e1, e1@.Data | e2)
          }
)

setMethod("|",
          signature(e1 = "array", e2 = "nifti"),
          function (e1, e2) 
          {
            niftiarr(e2, e1 | e2@.Data)
          }
)