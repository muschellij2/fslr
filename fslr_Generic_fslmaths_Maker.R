
proper = function(x){
  paste0(toupper(substr(x, 1,1)), substr(x, 2, nchar(x)))
}
makefunc2 = function(opt, longname, propername = NULL,
                     remove = TRUE){
  if (is.null(propername)){
    propername = proper(longname)
  }
  x = readLines('fslr_Generic_fslmaths.R')
  x = gsub("%opt%", opt, x)
  x = gsub("%propername%", propername, x)
  x = gsub("%longname%", longname, x)
  funcname = paste0("fsl", opt)
  
  writeLines(text=x, con = paste0("R/", funcname, ".R"))
  if (remove) file.remove(paste0("R/", funcname, ".R"))
  return(TRUE)
}

makefunc = function(opt, longname, propername = NULL,
                    remove = TRUE){
  if (is.null(propername)){
    propername = proper(longname)
  }
  x = readLines('fslr_Generic_fslmaths_onefile.R')
  x = gsub("%opt%", opt, x)
  x = gsub("%propername%", propername, x)
  x = gsub("%longname%", longname, x)
  funcname = paste0("fsl", opt)
  
  writeLines(text=x, con = paste0("R/", funcname, ".R"))
  if (remove) file.remove(paste0("R/", funcname, ".R"))
  return(TRUE)
}

remove = FALSE
################################################################
# Make Functions that take in 2 files
################################################################
# makefunc("slope", write=TRUE, remove=remove)
makefunc2("div", longname = "be divided", propername = "Divide",
          remove = remove)
makefunc2("sub", longname = "be subtracted", propername = "Subtract",
          remove = remove)
makefunc2("mul", longname = "be multiplied", propername = "Multiply",
          remove = remove)
makefunc2("add", longname = "be added", propername = "Add",
          remove = remove)
makefunc2("rem", longname = paste0("divide the current image by ", 
          "and take remainder"), 
          propername = "Modulus Remainder of 2",
          remove = remove)


################################################################
# Make Functions that take in 1 file
################################################################
makefunc("exp", longname = "exponentiated", propername = "Exponentiate",
         remove = remove)
makefunc("log", longname = "log transform", 
         propername = "Log Transform",
         remove = remove)
makefunc("sin", longname = "sine transform", 
         propername = "Sine Transform",
         remove = remove)
makefunc("asin", longname = "arc sine transform", 
         propername = "Arc Sine Transform",
         remove = remove)
makefunc("cos", longname = "cosine transform", 
         propername = "Cosine Transform",
         remove = remove)
makefunc("acos", longname = "arc cosine transform", 
         propername = "Arc Cosine Transform",
         remove = remove)
makefunc("tan", longname = "tangent transform", 
         propername = "Tangent Transform",
         remove = remove)
makefunc("atan", longname = "arc tangent transform", 
         propername = "Arc Tangent Transform",
         remove = remove)
makefunc("sqr", longname = "square", 
         propername = "Square",
         remove = remove)
makefunc("sqrt", longname = "square root", 
         propername = "Square Root",
         remove = remove)
makefunc("recip", longname = "take the reciprocal (1/image)", 
         propername = "Reciprocal",
         remove = remove)
makefunc("abs", longname = "absolute value", 
         propername = "Absolute Value",
         remove = remove)
makefunc("binv", longname = "take the binarized inverse", 
         propername = "Binarized Inverse",
         remove = remove)
makefunc("index", longname = paste0("have non-zero entries ", 
                                    "replaced with index"), 
                                    propername = "Index",
                                    remove = remove)
makefunc("edge", longname = paste0("estimate edge strength"), 
         propername = "Edge Strength",
         remove = remove)
makefunc("nan", longname = paste0("replace NaNs (improper numbers) ", 
                                  "with 0"), 
         propername = "Replace NaNs in",
         remove = remove)
makefunc("nanm", longname = paste0("set to 1 for NaN voxels,", 
                                   " 0 otherwise"), 
         propername = "Mask NaNs in",
         remove = remove)
makefunc("rand", longname = paste0("add random uniform noise to"), 
         propername = "Add Random Uniform Noise",
         remove = remove)
makefunc("randn", longname = paste0("add random standard to", 
                                    " Gaussian noise"), 
         propername = "Add Random Standard Guassian Noise",
         remove = remove)


         