#' @title Check if Objects have Same Dimensions
#' @description Wrapper to check if multiple objects all have the 
#' @param ... Arguments (matrices or arrays) where the dimension will be 
#' checked against the first object's dimension
#'
#' @return Logical indicating if all have the same dimensions or not
#' @export
#'
#' @examples
#' mat1 = matrix(1:9, ncol = 3)
#' mat2 = matrix(rnorm(9), ncol = 3)
#' mat3 = matrix(rnorm(16), ncol = 4)
#' mat4 = matrix(rnorm(9), ncol = 3)
#' same_dims(mat1, mat2)
#' same_dims(mat1, mat3)
#' same_dims(mat1, mat2, mat4)
same_dims = function(...){
  l = list(...)
  if (length(l) == 0) {
    stop("No arguments were passed in to check!")
  }
  # print(l)
  
  dims = lapply(l, dim)
  res = sapply(dims, 
               identical, 
               x = dims[[1]])
  res = all(res)
  return(res)
}