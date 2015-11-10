
proper = function(x){
  paste0(toupper(substr(x, 1,1)), substr(x, 2, nchar(x)))
}

#############################################
# Function maker that involves 1 file
#############################################
makefunc = function(opt, FUNC, func,
                    remove = TRUE){

  x = readLines('fslr_Generic_fslstats.R')
  x = gsub("%opt%", opt, x)
  x = gsub("%FUNC%", FUNC, x)
  x = gsub("%func%", func, x)
  funcname = paste0("fsl", opt)
  
  writeLines(text = x, con = paste0("R/", funcname, ".R"))
  if (remove) file.remove(paste0("R/", funcname, ".R"))
  return(TRUE)
}

#############################################
# Function maker that involves non-zero entries
#############################################
makefunc = function(opt, 
                    FUNC, 
                    func,
                    remove = TRUE){
  
  x = readLines('fslr_Generic_fslstats_nonzero.R')
  x = gsub("%opt%", opt, x)
  x = gsub("%OPT%", toupper(opt), x)
  x = gsub("%FUNC%", FUNC, x)
  x = gsub("%func%", func, x)
  funcname = paste0("fsl", func)
  
  writeLines(text = x, con = paste0("R/", funcname, ".R"))
  if (remove) file.remove(paste0("R/", funcname, ".R"))
  return(TRUE)
}


remove = FALSE
################################################################
# Make Functions that take in 1 file
################################################################
# makefunc("r", FUNC = "Robust Range", func = "rrange",
#          remove = TRUE)


makefunc("m", FUNC = "Mean", func = "mean",
         remove = remove)
makefunc("s", FUNC = "Standard Deviation", func = "sd",
         remove = remove)
makefunc("e", FUNC = "Mean Entropy", func = "entropy",
         remove = remove)





