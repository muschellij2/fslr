
makefunc = function(funcname, type="numeric", write=FALSE){
  x = readLines('Generic_Function.R')
  x = gsub("%%", funcname, x)
  x = gsub("%type", type, x)
  if (write) writeLines(text=x, con = paste0("R/", funcname, ".R"))
  return(TRUE)
}

makefunc("vox_offset", write=TRUE)
makefunc("qform_code", write=TRUE)
makefunc("sform_code", write=TRUE)
makefunc("scl_inter", write=TRUE)
makefunc("scl_slope", write=TRUE)
makefunc("slope", write=TRUE)
